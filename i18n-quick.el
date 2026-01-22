;;; i18n-quick.el --- Modern i18n tool with dedicated Consult search -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'pulse)
(require 'embark)
(require 'consult)

;; ==========================================
;; 1. 配置与变量
;; ==========================================
(defvar embark-keymap-alist nil)

(defgroup i18n-quick nil "Modern i18n tool." :group 'tools)

(defcustom i18n-quick-style 'nested
  "嵌套格式 (nested) 或 扁平格式 (flat)。" :type '(choice (const nested) (const flat)))

(defcustom i18n-quick-languages
  '(("zh-CN" . "src/locale/zh-CN/")
    ("en-US" . "src/locale/en-US/"))
  "i18n 语言路径映射" :type '(alist :key-type string :value-type string))

(defcustom i18n-quick-max-width 50
  "翻译显示的最大宽度，超过的部分会显示为 ..."
  :type 'integer
  :group 'i18n-quick)

(defvar i18n-quick-current-lang "zh-CN" "当前选中的语言名称")
(defvar i18n-quick--file-cache (make-hash-table :test 'equal) "JSON 数据缓存")

(defun i18n-quick--truncate (str len)
  "如果 STR 超过 LEN，则截断并添加省略号。"
  (if (and str (> (length str) len))
      (concat (substring str 0 (- len 3)) "...")
    str))

;; ==========================================
;; 2. 核心数据层
;; ==========================================

(cl-defstruct i18n-quick--ctx file-or-dir style root)

(defun i18n-quick--get-ctx ()
  (let* ((root (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                   (locate-dominating-file default-directory ".git")
                   default-directory))
         (raw-path (or (cdr (assoc i18n-quick-current-lang i18n-quick-languages))
                       (cdar i18n-quick-languages)))
         (path (if (symbolp raw-path) (symbol-name raw-path) raw-path)))
    (make-i18n-quick--ctx :root root :file-or-dir (expand-file-name path root) :style i18n-quick-style)))

(defun i18n-quick--resolve-target (ctx key)
  (let* ((base (i18n-quick--ctx-file-or-dir ctx)))
    (if (file-directory-p base)
        (let* ((parts (split-string key "\\."))
               (file-part (car parts))
               (inner-part (if (cdr parts) (mapconcat #'identity (cdr parts) ".") file-part))
               (filename (concat (replace-regexp-in-string "[^[:alnum:].-]" "" file-part) ".json")))
          (cons (expand-file-name filename base) inner-part))
      (cons base key))))

(defun i18n-quick--read-json (file)
  (let* ((attrs (file-attributes file))
         (mtime (and attrs (file-attribute-modification-time attrs)))
         (cache-entry (gethash file i18n-quick--file-cache)))
    (if (and cache-entry (equal (car cache-entry) mtime))
        (cdr cache-entry)
      (when (and file (file-exists-p file))
        (let ((data (with-temp-buffer
                      (insert-file-contents file)
                      (condition-case nil (json-parse-buffer :object-type 'alist
                                                             ) (error nil)))))
          (puthash file (cons mtime data) i18n-quick--file-cache)
          data)))))

;; (defun i18n-quick--save (file alist)
;;   "高性能保存函数。确保 alist 中没有不规范的占位符。"
;;   (with-temp-file file
;;     (insert (json-serialize alist
;;                             :pretty t
;;                             )))
;;   (remhash file i18n-quick--file-cache))

(defun i18n-quick--save (file alist)
  "保存 alist 到 JSON 文件，自动格式化。"
  (with-temp-file file
    (let ((json-string (json-serialize alist
                                       :null-object :null
                                       :false-object :false)))
      (insert json-string)
      ;; 使用 jq 格式化（如果可用）
      (when (executable-find "jq")
        (call-process-region (point-min) (point-max)
                             "jq" t t nil ".")))
    ;; 删除末尾换行
    (goto-char (point-max))
    (when (and (looking-back "\n" 1) (> (point-max) 1))
      (delete-char -1)))
  (remhash file i18n-quick--file-cache))

(defun i18n-quick-get-translation (key)
  (when (and key (stringp key))
    (let* ((ctx (i18n-quick--get-ctx))
           (target (i18n-quick--resolve-target ctx key))
           (file (car target))
           (inner-key (cdr target))
           (data (i18n-quick--read-json file))
           (keys (if (eq (i18n-quick--ctx-style ctx) 'flat) (list inner-key) (split-string inner-key "\\.")))
           (val data))
      (dolist (k keys) (setq val (if (listp val) (cdr (assoc k val #'string=)) nil)))
      (when (stringp val) val))))

;; ==========================================
;; 3. 生态接入 (Embark)
;; ==========================================

(defun i18n-quick-embark-target-finder ()
  "识别当前光标下的 i18n key。"
  (when i18n-quick-mode
    (save-excursion
      (let* ((re "\\(?:t(\\|i18nKey=\\)['\"]\\([^'\"{}() ]+\\)['\"]")
             (p (point))
             (line-beg (line-beginning-position))
             (line-end (line-end-position)))
        (goto-char line-beg)
        (let (found-key bounds)
          (while (and (not found-key) (re-search-forward re line-end t))
            (when (and (>= p (match-beginning 1)) (<= p (match-end 1)))
              (setq found-key (match-string-no-properties 1))
              (setq bounds (cons (match-beginning 1) (match-end 1)))))
          (when (and found-key bounds)
            `(i18n-key ,found-key . ,bounds)))))))

(defvar i18n-quick-embark-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map embark-general-map)
    (define-key map "j" #'i18n-quick-jump-or-create)
    (define-key map "s" #'i18n-quick-switch-lang)
    map))

;; ==========================================
;; 4. 专属搜索命令 (核心改进)
;; ==========================================

(defun i18n-quick-consult-line ()
  "针对当前 Buffer 内 i18n Key 的专属搜索视图。"
  (interactive)
  (let* ((candidates nil)
         (re "\\(?:t(\\|i18nKey=\\)['\"]\\([^'\"{}() ]+\\)['\"]"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (let* ((line (line-number-at-pos))
               (key (match-string-no-properties 1))
               (content (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
               (marker (copy-marker (match-beginning 1)))
               ;; 将数据封装在文本属性中
               (cand (propertize (format "%4d: %s" line (string-trim content))
                                 'consult-location (cons marker line)
                                 'i18n-key key)))
          (push cand candidates))))

    (if (not candidates)
        (message "当前文件未发现 i18n Key")
      (consult--read
       (nreverse candidates)
       :prompt "Search i18n Key: "
       :category 'i18n-key
       :sort nil
       :require-match t
       ;; 使用内置标注，支持长翻译自动截断
       :annotate (lambda (cand)
                   (when-let* ((key (get-text-property 0 'i18n-key cand))
                               (val (i18n-quick-get-translation key)))
                     (let ((truncated-val (i18n-quick--truncate val i18n-quick-max-width)))
                       (format "  # %s" (propertize truncated-val 'face 'marginalia-archive)))))
       :lookup #'consult--lookup-location
       :state (consult--location-state candidates)))))

;; ==========================================
;; 5. 交互命令
;; ==========================================

(defun i18n-quick--search-nested-path (keys)
  "逐层搜索嵌套路径，避免同名 key 冲突。
例如：keys = ('common' 'button' 'save')
步骤：
1. 搜索 \"common\":
2. 从该位置向下搜索 \"button\":
3. 从该位置向下搜索 \"save\":
返回：找到时 point 停留在 key 的冒号位置，否则返回 nil。"
  (unless (null keys)  ; 空 keys 返回 nil
    (catch 'found
      (dolist (key keys)
        ;; 搜索当前层级的 key
        (unless (re-search-forward (format "\"%s\"\\s-*:" (regexp-quote key)) nil t)
          (throw 'found nil))
        ;; 移动到 key 对应的 value 起始位置（对象或数组）
        (goto-char (match-end 0))
        (skip-chars-forward " \t\n")
        (when (looking-at "{")
          ;; 进入嵌套对象，继续下一层搜索
          (forward-char 1)
          (skip-chars-forward " \t\n")))
      ;; 所有层级都匹配成功
      t)))

(defun i18n-quick-jump-or-create ()
  "跳转或创建翻译。如果 Key 不存在，不询问直接创建空值并跳转。"
  (interactive)
  (let* ((cand (when (derived-mode-p 'minibuffer-mode)
                 (embark--target-get-status 'target)))
         (key (if cand
                  (or (get-text-property 0 'i18n-key cand)
                      (when (string-match "['\"]\\([^'\" ]+\\)['\"]" cand) (match-string 1 cand)))
                (let ((target (i18n-quick-embark-target-finder)))
                  (if (listp target) (cadr target) target)))))
    (unless key (setq key (read-string "I18n Key: ")))
    (let* ((ctx (i18n-quick--get-ctx))
           (target (i18n-quick--resolve-target ctx key))
           (file (car target))
           (inner-key (cdr target))
           (data (i18n-quick--read-json file))
           (keys (split-string inner-key "\\."))
           (val (i18n-quick-get-translation key)))

      (if val
          ;; --- 分支 1: 存在则直接跳转 ---
          (progn
            (find-file file)
            (goto-char (point-min))
            ;; 逐层搜索完整的路径，避免同名 key 冲突
            (if (i18n-quick--search-nested-path keys)
                (progn
                  (when (looking-at "[\"']") (forward-char 1)) ; 跳过引号，精准定位到值
                  (pulse-momentary-highlight-one-line (point))
                  (recenter))
              (message "找到文件但未匹配到具体的 Key 文本")))

        ;; --- 分支 2: 不存在则“静默创建” ---
        (let ((new-data (i18n-quick--update-alist data keys "")))
          (i18n-quick--save file new-data)
          ;; 重要：创建完成后再次调用自己，触发上面的“分支 1”进行跳转
          (i18n-quick-jump-or-create))))))

(defun i18n-quick-switch-lang ()
  (interactive)
  (let* ((langs (mapcar #'car i18n-quick-languages))
         (choice (completing-read "选择语言: " langs nil t)))
    (setq i18n-quick-current-lang choice)
    (message "已切换到: %s" choice)))

(defun i18n-quick--update-alist (alist keys value)
  "递归构建标准的点对 alist，确保 json-serialize 不报错。
KEYS 是字符串列表（如 '(\"menus\" \"platform\")），
会在内部转换为 symbols（因为 json-serialize 要求 alist 的 key 必须是 symbols）。"
  (let* ((alist (if (and (listp alist) (cl-every #'consp alist))
                    alist
                  nil))
         (k (intern (car keys)))  ; 将 string 转为 symbol
         (rest (cdr keys)))
    (if (null rest)
        ;; 最后一层：插入 ('symbol . "")
        (cons (cons k value)
              (assq-delete-all k alist))
      ;; 中间层：递归向下构建对象
      (let* ((old-cell (assq k alist))  ; 使用 assq（符号比较）
             (old-val (and old-cell (cdr old-cell)))
             (new-child (i18n-quick--update-alist old-val rest value)))
        (cons (cons k new-child)
              (assq-delete-all k alist))))))

(defun i18n-quick-clear-cache ()
  "手动清除 i18n-quick 的 JSON 数据缓存。"
  (interactive)
  (clrhash i18n-quick--file-cache)
  (message "i18n-quick 缓存已清空"))

;; ==========================================
;; 6. Mode 定义
;; ==========================================
(define-minor-mode i18n-quick-mode
  "Modern i18n tool." :lighter " i18n"
  (if i18n-quick-mode
      (progn
        ;; 注册 Embark 目标识别
        (add-hook 'embark-target-finders #'i18n-quick-embark-target-finder)
        ;; 注册 Embark 动作菜单
        (with-eval-after-load 'embark
          (setf (alist-get 'i18n-key embark-keymap-alist) 'i18n-quick-embark-map)))
    ;; 退出模式时移除钩子
    (remove-hook 'embark-target-finders #'i18n-quick-embark-target-finder)))

(provide 'i18n-quick)
