;;; i18n-quick.el --- Modern i18n tool with dedicated Consult search -*- lexical-binding: t; -*-

(require 'cl-lib)
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

(defun i18n-quick-get-translation (key)
  "使用 tree-sitter 获取 KEY 的翻译值。
返回翻译字符串，如果 key 不存在则返回 nil。"
  (when (and key (stringp key))
    ;; 确保 tree-sitter 模块已加载
    (unless (featurep 'i18n-quick-treesit)
      (require 'i18n-quick-treesit nil t))
    (let* ((ctx (i18n-quick--get-ctx))
           (target (i18n-quick--resolve-target ctx key))
           (file (car target))
           (inner-key (cdr target))
           (keys (split-string inner-key "\\.")))
      (when (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (unless (derived-mode-p 'json-ts-mode)
            (json-ts-mode))
          (let* ((root (treesit-buffer-root-node))
                 (result (i18n-quick--treesit-find-path root keys)))
            (when result
              (let* ((parent-node (car result))
                     (key-name (cdr result))
                     (pair (i18n-quick--treesit-find-pair-in-object parent-node key-name)))
                (when pair
                  (let* ((value-node (treesit-node-child pair 2))
                         (value-type (treesit-node-type value-node)))
                    (when (string= value-type "string")
                      (i18n-quick--extract-string-content value-node))))))))))))

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

;;;###autoload
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

;;;###autoload
(defun i18n-quick-jump-or-create ()
  "跳转或创建翻译。如果 Key 不存在，不询问直接创建空值并跳转。
需要 Emacs 29+ 和 tree-sitter 支持。"
  (interactive)
  ;; 确保 tree-sitter 模块已加载
  (unless (featurep 'i18n-quick-treesit)
    (condition-case err
        (require 'i18n-quick-treesit)
      (error (user-error "无法加载 i18n-quick-treesit: %S" err))))
  ;; 检查 tree-sitter 是否可用
  (unless (i18n-quick--treesit-available-p)
    (user-error "i18n-quick 需要 Emacs 29+ 和 tree-sitter 支持（请确保已安装 tree-sitter）"))

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
           (keys (split-string inner-key "\\.")))

      ;; 先打开文件，检查 key 是否存在
      (find-file file)
      (goto-char (point-min))
      (unless (derived-mode-p 'json-ts-mode)
        (json-ts-mode))
      (unless (treesit-parser-list)
        (treesit-parser-create 'json))

      (let* ((root (treesit-buffer-root-node))
             (exists (i18n-quick--treesit-find-path root keys)))

        (if exists
            ;; --- 分支 1: key 存在，直接跳转 ---
            (progn
              (message "✓ Key '%s' 已存在，正在跳转..." key)
              (if (i18n-quick--search-nested-path keys)
                  (progn
                    (when (looking-at "[\"']") (forward-char 1)) ; 跳过引号，精准定位到值
                    (pulse-momentary-highlight-one-line (point))
                    (recenter))
                (message "⚠️ 查找失败：i18n-quick--treesit-find-path 返回存在，但 i18n-quick--search-nested-path 未找到")))

          ;; --- 分支 2: key 不存在，创建 ---
          (message "未找到 Key '%s'，正在创建..." key)
          (if (i18n-quick--treesit-save file keys "")
              ;; 创建成功，重新解析并跳转
              (progn
                ;; 重新获取 root 节点（因为 buffer 已修改）
                (setq root (treesit-buffer-root-node))
                (when (i18n-quick--search-nested-path keys)
                  (when (looking-at "[\"']") (forward-char 1))
                  (pulse-momentary-highlight-one-line (point))
                  (recenter))
                (message "Key '%s' 创建成功" key))
            (user-error "创建 Key 失败，请检查文件格式")))))))

;;;###autoload
(defun i18n-quick-switch-lang ()
  (interactive)
  (let* ((langs (mapcar #'car i18n-quick-languages))
         (choice (completing-read "选择语言: " langs nil t)))
    (setq i18n-quick-current-lang choice)
    (message "已切换到: %s" choice)))

;; ==========================================
;; tree-sitter 的集成
;; ==========================================

(require 'i18n-quick-treesit nil t)

;;;###autoload
(defun i18n-quick--search-nested-path (keys)
  "使用 tree-sitter 搜索嵌套路径 KEYS。
KEYS 是字符串列表，例如 (\"common\" \"button\" \"save\")。
如果找到，跳转到该 key 的值位置并返回 t，否则返回 nil。"
  (i18n-quick--search-nested-path-treesit keys))

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
