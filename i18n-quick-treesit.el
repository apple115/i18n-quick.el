;;; i18n-quick-treesit.el --- tree-sitter integration -*- lexical-binding: t; -*-

(require 'treesit)

(defun i18n-quick--treesit-available-p()
  "检查treesitter 是否可用"
  (and (version<= "29" emacs-version)
       (boundp 'treesit-available-p)
       (treesit-available-p)
       (treesit-language-p 'json)))


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
        (skip-chars-forward " \t\n:")
        t))))

