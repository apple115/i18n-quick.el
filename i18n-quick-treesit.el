;;; i18n-quick-treesit.el --- tree-sitter integration -*- lexical-binding: t; -*-

(require 'treesit)

(defun i18n-quick--treesit-available-p()
  "检查treesitter 是否可用"
  (and (version<= "29" emacs-version)
       (fboundp 'treesit-available-p)
       (treesit-available-p)
       (condition-case nil
           (progn
             (treesit-parser-create 'json)
             t)
         (error nil))))


(defun i18n-quick--read-json-treesit (file)
  (with-temp-buffer
    (insert-file-contents file)
    (json-ts-mode)
    (let ((root (treesit-buffer-root-node)))
      (i18n-quick--json-node-to-alist root))))

(defun i18n-quick--json-node-to-alist (node)
  "将 treesit json 节点转换为 emacs lisp 数据结构"
  (let ((type (treesit-node-type node)))
    (cond
     ((string= type "document")
      (i18n-quick--json-node-to-alist
       (treesit-node-child node 0)))
     ((string= type "object")
      (let* ((children (treesit-node-children node))
             (pairs (cl-remove-if-not
                     (lambda (n) (string= (treesit-node-type n) "pair"))
                     children)))
        (mapcar (lambda (pair)
                  (let* ((key-node (treesit-node-child pair 0))
                         (key-string (i18n-quick--extract-string-content key-node))
                         (value-node (treesit-node-child pair 2))
                         (value (i18n-quick--json-node-to-alist value-node)))
                    (cons key-string value)))
                pairs)))
     ((string= type "array") (mapcar #'i18n-quick--json-node-to-alist (treesit-node-children node)))
     ((string= type "string") (i18n-quick--extract-string-content node))
     ((string= type "number") (string-to-number (treesit-node-text node)))
     ((string= type "true") t)
     ((string= type "false") :false)
     ((string= type "null") :null)
     (t nil)
     )))


(defun i18n-quick--extract-string-content(string-node)
  "从 tree-sitter 的 string 中提取实际内容 "
  (let ((content (treesit-node-child string-node 1)))
    (substring-no-properties (treesit-node-text content))))

(defun i18n-quick--search-nested-path-treesit (keys)
  "使用 tree-sitter 在当前 JSON buffer 中查找嵌套路径 KEYS。
KEYS 是一个字符串列表，例如 (\"common\" \"button\" \"save\")。
如果找到，跳转到该 key 的冒号位置并返回 t，否则返回 nil。"
  (let* ((node (treesit-buffer-root-node))
         (current-node node)
         (found-pos nil))
    (unless (treesit-parser-list)
      (treesit-parser-create 'json))

    (catch 'break
      (dolist (key keys)
        (let* ((obj-node (if (string= (treesit-node-type current-node) "document")
                             (treesit-node-child current-node 0)
                           current-node))
               (pairs (cl-remove-if-not
                       (lambda (n) (string= (treesit-node-type n) "pair"))
                       (treesit-node-children obj-node t)))
               (matched-pair nil))
          (setq  matched-pair
                 (cl-find key pairs
                          :test (lambda (k p)
                                  (string= k (i18n-quick--extract-string-content (treesit-node-child p 0))))))
          (if (not matched-pair)
              (throw 'break nil)
            (setq current-node (treesit-node-child matched-pair 2))
            (setq found-pos (treesit-node-end (treesit-node-child matched-pair 0))))))

      (when found-pos
        (goto-char found-pos)
        (skip-chars-forward " \t\n:")
        t))))

;; ==========================================
;; AST 直接操作函数
;; ==========================================

(defun i18n-quick--treesit-get-object-node (node)
  "获取 NODE 的对象节点。
如果 NODE 是 document，返回其子节点 object。
如果 NODE 是 object，直接返回。
否则返回 nil。"
  (let ((type (treesit-node-type node)))
    (cond
     ((string= type "document")
      (let ((child (treesit-node-child node 0)))
        (when (and child (string= (treesit-node-type child) "object"))
          child)))
     ((string= type "object") node)
     (t nil))))

(defun i18n-quick--treesit-find-pair-in-object (obj-node key)
  "在对象节点 OBJ-NODE 中查找键为 KEY 的 pair 节点。
返回匹配的 pair 节点，未找到返回 nil。"
  (when obj-node
    (let ((pairs (cl-remove-if-not
                  (lambda (n) (string= (treesit-node-type n) "pair"))
                  (treesit-node-children obj-node t))))
      (cl-find key pairs
               :test (lambda (k p)
                       (string= k (i18n-quick--extract-string-content
                                   (treesit-node-child p 0))))))))

(defun i18n-quick--treesit-find-path (root-node keys)
  "在 AST 上查找键路径 KEYS。
ROOT-NODE 是树的根节点（通常是 document 或 object）。
KEYS 是字符串列表，例如 (\"common\" \"button\" \"save\")。

返回值：
- 如果找到完整路径：返回 (parent-node . key-string)
  parent-node 是最后一个 key 的父对象节点
  key-string 是最后一个 key 的名称
- 如果未找到：返回 nil"
  (when (and keys root-node)
    (let* ((current-node (i18n-quick--treesit-get-object-node root-node))
           (remaining-keys keys)
           (parent-node nil))

      ;; 遍历路径，除了最后一个 key
      (while (and (cdr remaining-keys) current-node)
        (let* ((key (car remaining-keys))
               (pair (i18n-quick--treesit-find-pair-in-object current-node key)))
          (if (not pair)
              (setq current-node nil)  ; 中间路径不存在
            (let ((value-node (treesit-node-child pair 2)))
              ;; 只有当值是 object 时才能继续深入
              (if (string= (treesit-node-type value-node) "object")
                  (setq current-node value-node)
                (setq current-node nil)))))
        (setq remaining-keys (cdr remaining-keys)))

      ;; 处理最后一个 key
      (when (and current-node remaining-keys)
        (setq parent-node current-node))

      ;; 返回 (parent-node . last-key)，但前提是最后一个 key 真的存在
      (when (and parent-node
                 (i18n-quick--treesit-find-pair-in-object parent-node (car remaining-keys)))
        (cons parent-node (car remaining-keys))))))

(defun i18n-quick--treesit-get-indentation (obj-node)
  "获取对象节点的缩进级别。
返回缩进字符串（空格或制表符）。"
  (save-excursion
    (let ((start (treesit-node-start obj-node)))
      (goto-char start)
      (back-to-indentation)
      (buffer-substring-no-properties (line-beginning-position) (point)))))

(defun i18n-quick--treesit-insert-at-end (obj-node text)
  "在对象节点 OBJ-NODE 的末尾插入 TEXT。
保持正确的缩进和逗号处理。"
  (let ((children (treesit-node-children obj-node t)))
    (if (null children)
        ;; 空对象 {}，在 { 后插入
        (let ((start (treesit-node-start obj-node)))
          (goto-char (1+ start))
          (insert text))
      ;; 非空对象，在最后一个子节点后插入
      (let* ((last-child (car (last children)))
             (last-child-type (treesit-node-type last-child))
             (last-child-end (treesit-node-end last-child))
             (indent (i18n-quick--treesit-get-indentation obj-node))
             (new-indent (concat indent "  ")))
        (goto-char last-child-end)
        ;; 检查最后一个子节点是否是 pair，决定是否需要添加逗号
        (when (string= last-child-type "pair")
          ;; 检查 pair 后面是否有逗号
          (save-excursion
            (goto-char last-child-end)
            (skip-chars-forward " \t\n")
            (unless (looking-at ",")
              (insert ","))))
        ;; 插入新内容
        (insert (format "\n%s%s" new-indent text))))))

(defun i18n-quick--treesit-create-empty-object (parent-obj key _indent)
  "在父对象 PARENT-OBJ 中创建一个名为 KEY 的空对象。
返回新创建的 object 节点。"
  (let* ((formatted-key (format "\"%s\": {}" key)))
    (i18n-quick--treesit-insert-at-end parent-obj formatted-key)
    ;; 重新解析以获取新节点
    (treesit-buffer-root-node)))

(defun i18n-quick--treesit-create-path (node keys value)
  "在 AST 上创建嵌套路径 KEYS 并设置值 VALUE。
NODE 是起始节点（通常是 document 或 object）。
KEYS 是字符串列表，例如 (\"common\" \"button\" \"save\")。
VALUE 是要设置的值字符串。

直接在当前 buffer 中修改，不返回值。"
  (when (and keys node)
    (let* ((current-node (i18n-quick--treesit-get-object-node node))
           (remaining-keys keys))

      ;; 确保路径存在
      (while (and (cdr remaining-keys) current-node)
        (let* ((key (car remaining-keys))
               (pair (i18n-quick--treesit-find-pair-in-object current-node key)))
          (if (not pair)
              ;; 路径不存在，创建空对象
              (let ((indent (i18n-quick--treesit-get-indentation current-node)))
                (i18n-quick--treesit-create-empty-object current-node key indent)
                ;; 重新获取当前节点
                (setq current-node (i18n-quick--treesit-get-object-node
                                    (treesit-buffer-root-node)))
                ;; 再次查找刚创建的 pair
                (setq pair (i18n-quick--treesit-find-pair-in-object current-node key))
                (when pair
                  (setq current-node (treesit-node-child pair 2))))
            ;; 路径存在，检查值是否为 object
            (let ((value-node (treesit-node-child pair 2)))
              (if (string= (treesit-node-type value-node) "object")
                  (setq current-node value-node)
                ;; 不是 object，无法继续深入
                (setq current-node nil)))))
        (setq remaining-keys (cdr remaining-keys)))

      ;; 创建最后一个 key-value 对
      (when (and current-node remaining-keys (null (cdr remaining-keys)))
        (let* ((key (car remaining-keys))
               (quoted-value (if (stringp value)
                                 (format "\"%s\"" value)
                               (format "%s" value)))
               (formatted-pair (format "\"%s\": %s" key quoted-value)))
          (i18n-quick--treesit-insert-at-end current-node formatted-pair))))))

(defun i18n-quick--treesit-replace-value (node new-value)
  "替换节点的值。
NODE 是一个 pair 节点。
NEW-VALUE 是新的值（字符串类型）。
直接在当前 buffer 中修改，不返回值。"
  (when node
    (let* ((value-node (treesit-node-child node 2))
           (value-start (treesit-node-start value-node))
           (value-end (treesit-node-end value-node))
           (quoted-value (if (stringp new-value)
                             (format "\"%s\"" new-value)
                           (format "%s" new-value))))
      ;; 删除旧值并插入新值
      (goto-char value-start)
      (delete-region value-start value-end)
      (insert quoted-value))))

(defun i18n-quick--treesit-save (file keys value)
  "使用 tree-sitter 直接在 AST 上保存键值对。
FILE 是目标 JSON 文件路径。
KEYS 是键路径列表。
VALUE 是要设置的值。

直接操作 AST，避免 alist 转换，保留原有格式和注释。"
  (with-current-buffer (find-file-noselect file)
    (unless (derived-mode-p 'json-ts-mode)
      (json-ts-mode))
    (unless (treesit-parser-list)
      (treesit-parser-create 'json))
    ;; 如果文件为空，创建基础结构
    (when (= (point-max) 1)
      (insert "{}")
      (treesit-parser-create 'json))
    (let* ((root (treesit-buffer-root-node))
           (find-result (i18n-quick--treesit-find-path root keys)))
      (if find-result
          ;; 路径存在，替换值
          (let* ((parent-node (car find-result))
                 (key (cdr find-result))
                 (pair (i18n-quick--treesit-find-pair-in-object parent-node key)))
            (when pair
              (i18n-quick--treesit-replace-value pair value)))
        ;; 路径不存在，创建新路径
        (i18n-quick--treesit-create-path root keys value)))
    ;; 保存 buffer
    (save-buffer)
    t))

(provide 'i18n-quick-treesit)
