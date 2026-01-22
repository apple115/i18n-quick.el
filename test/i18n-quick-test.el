;;; i18n-quick-test.el --- ERT tests for i18n-quick -*- lexical-binding: t; -*-

(require 'ert)
(require 'i18n-quick)

;; ============================================
;; 测试辅助函数
;; ============================================

(defun i18n-quick-test--create-temp-buffer (content)
  "创建临时 buffer 并插入 CONTENT，返回 buffer 对象。"
  (let ((buf (generate-new-buffer "*i18n-quick-test*")))
    (with-current-buffer buf
      (insert content)
      ;; 如果有 json-mode 就使用，否则使用 fundamental-mode
      (condition-case nil
          (json-mode)
        (error (fundamental-mode))))
    buf))

(defun i18n-quick-test--extract-string-value-at-point ()
  "提取当前 point 位置的 JSON 字符串值（假设 point 在 key:value 的冒号后）。
返回字符串值内容（不含引号）。"
  (save-excursion
    (skip-chars-forward " \t\n")
    (when (looking-at "\"")
      (forward-char 1)  ; 跳过开始引号
      (let ((start (point)))
        ;; 找到结束引号（简单处理：找下一个未转义的引号）
        (while (and (not (eobp))
                    (not (and (looking-at "\"")
                             (not (= (char-before) ?\\)))))
          (forward-char 1))
        (buffer-substring start (point))))))

;; ============================================
;; 测试用例：逐层搜索嵌套路径
;; ============================================

(ert-deftest i18n-quick-search-nested-path-simple ()
  "测试简单路径搜索：单层 key"
  (let ((buf (i18n-quick-test--create-temp-buffer "{
  \"save\": \"保存\"
}")))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (should (i18n-quick--search-nested-path '("save")))
          ;; 验证光标在正确的位置
          (should (looking-at-p "['\"]?"))
          (should (search-backward "\"save\":" nil t)))
      (kill-buffer buf))))

(ert-deftest i18n-quick-search-nested-path-two-levels ()
  "测试两层嵌套路径"
  (let ((buf (i18n-quick-test--create-temp-buffer "{
  \"common\": {
    \"save\": \"保存\"
  }
}")))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (should (i18n-quick--search-nested-path '("common" "save")))
          (should (search-backward "\"save\":" nil t)))
      (kill-buffer buf))))

(ert-deftest i18n-quick-search-nested-path-three-levels ()
  "测试三层嵌套路径"
  (let ((buf (i18n-quick-test--create-temp-buffer "{
  \"common\": {
    \"button\": {
      \"save\": \"保存\"
    }
  }
}")))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (should (i18n-quick--search-nested-path '("common" "button" "save")))
          (should (search-backward "\"save\":" nil t)))
      (kill-buffer buf))))

;; ============================================
;; 测试用例：同名 key 冲突场景（核心修复）
;; ============================================

(ert-deftest i18n-quick-search-nested-path-duplicate-keys ()
  "测试同名 key 冲突：应跳转到正确路径，而非第一个匹配项"
  (let ((buf (i18n-quick-test--create-temp-buffer "{
  \"common\": {
    \"button\": {
      \"save\": \"保存\"
    }
  },
  \"dialog\": {
    \"button\": {
      \"save\": \"保存对话框\"
    }
  }
}")))
    (unwind-protect
        (with-current-buffer buf
          ;; 测试跳转到 common.button.save
          (goto-char (point-min))
          (should (i18n-quick--search-nested-path '("common" "button" "save")))
          ;; 验证提取的值是 "保存" 而不是 "保存对话框"
          (should (string= (i18n-quick-test--extract-string-value-at-point) "保存"))

          ;; 测试跳转到 dialog.button.save
          (goto-char (point-min))
          (should (i18n-quick--search-nested-path '("dialog" "button" "save")))
          ;; 验证提取的值是 "保存对话框"
          (should (string= (i18n-quick-test--extract-string-value-at-point) "保存对话框")))
      (kill-buffer buf))))

(ert-deftest i18n-quick-search-nested-path-multiple-siblings ()
  "测试多个同级 key：验证不会跳转到错误的兄弟节点"
  (let ((buf (i18n-quick-test--create-temp-buffer "{
  \"common\": {
    \"button\": {
      \"save\": \"保存\",
      \"cancel\": \"取消\"
    },
    \"menu\": {
      \"save\": \"保存菜单\"
    }
  }
}")))
    (unwind-protect
        (with-current-buffer buf
          ;; 测试 common.button.save（不是 common.menu.save）
          (goto-char (point-min))
          (should (i18n-quick--search-nested-path '("common" "button" "save")))
          (should (string= (i18n-quick-test--extract-string-value-at-point) "保存"))

          ;; 测试 common.menu.save
          (goto-char (point-min))
          (should (i18n-quick--search-nested-path '("common" "menu" "save")))
          (should (string= (i18n-quick-test--extract-string-value-at-point) "保存菜单")))
      (kill-buffer buf))))

;; ============================================
;; 测试用例：边界情况
;; ============================================

(ert-deftest i18n-quick-search-nested-path-not-found ()
  "测试不存在的 key：应返回 nil"
  (let ((buf (i18n-quick-test--create-temp-buffer "{
  \"common\": {
    \"save\": \"保存\"
  }
}")))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          ;; 第一层就不存在
          (should-not (i18n-quick--search-nested-path '("dialog" "save")))
          ;; 第二层不存在
          (goto-char (point-min))
          (should-not (i18n-quick--search-nested-path '("common" "cancel"))))
      (kill-buffer buf))))

(ert-deftest i18n-quick-search-nested-path-empty-keys ()
  "测试空 key 列表：应返回 nil"
  (let ((buf (i18n-quick-test--create-temp-buffer "{}")))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (should-not (i18n-quick--search-nested-path '())))
      (kill-buffer buf))))

(ert-deftest i18n-quick-search-nested-path-with-whitespace ()
  "测试带有空格和换行的 JSON 格式"
  (let ((buf (i18n-quick-test--create-temp-buffer "{
  \"common\": {

    \"button\": {

      \"save\": \"保存\"
    }
  }
}")))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (should (i18n-quick--search-nested-path '("common" "button" "save"))))
      (kill-buffer buf))))

;; ============================================
;; 测试用例：辅助函数
;; ============================================

(ert-deftest i18n-quick-resolve-target-directory-mode ()
  "测试目录模式下的路径解析"
  (let ((i18n-quick-style 'nested)
        (i18n-quick-languages '(("zh-CN" . "locale/")))
        (default-directory "/tmp/test/"))
    (cl-letf (((symbol-function 'locate-dominating-file)
               (lambda (_dir _file) "/tmp/test/"))
              ((symbol-function 'file-directory-p)
               (lambda (_file) t)))  ; Mock：所有路径都是目录
      (let* ((ctx (i18n-quick--get-ctx))
             (target (i18n-quick--resolve-target ctx "common.button.save")))
        (should (string-match "locale/common\\.json$" (car target)))
        (should (string= "button.save" (cdr target)))))))

(ert-deftest i18n-quick-resolve-target-file-mode ()
  "测试文件模式下的路径解析"
  (let ((i18n-quick-style 'flat)
        (i18n-quick-languages '(("zh-CN" . "locale/zh-CN.json")))
        (default-directory "/tmp/test/"))
    (cl-letf (((symbol-function 'locate-dominating-file)
               (lambda (_dir _file) "/tmp/test/"))
              ((symbol-function 'file-directory-p)
               (lambda (_file) nil)))  ; Mock：所有路径都是文件
      (let* ((ctx (i18n-quick--get-ctx))
             (target (i18n-quick--resolve-target ctx "common.button.save")))
        (should (string-match "locale/zh-CN\\.json$" (car target)))
        (should (string= "common.button.save" (cdr target)))))))

(ert-deftest i18n-quick-update-alist-simple ()
  "测试简单的 alist 更新"
  (let ((result (i18n-quick--update-alist nil '("key") "value")))
    (should (equal result '((key . "value"))))))  ; keys 是 symbols

(ert-deftest i18n-quick-update-alist-nested ()
  "测试嵌套 alist 的创建"
  (let ((result (i18n-quick--update-alist nil '("common" "button" "save") "保存")))
    (should (equal result '((common . ((button . ((save . "保存"))))))))))  ; symbols

(ert-deftest i18n-quick-update-alist-merge ()
  "测试与现有 alist 的合并"
  (let ((existing '((common . ((cancel . "取消"))))))  ; symbols
    (let ((result (i18n-quick--update-alist existing '("common" "save") "保存")))
      (should (equal (cdr (assq 'common result))  ; 使用 assq
                     '((save . "保存") (cancel . "取消")))))))

;; ============================================
;; 测试用例：JSON 序列化（模拟真实场景）
;; ============================================

(ert-deftest i18n-quick-json-serialize-simple ()
  "测试简单结构的 JSON 序列化"
  (let ((data '((key . "value"))))
    (should (stringp (json-serialize data
                                     :null-object :null
                                     :false-object :false)))))

(ert-deftest i18n-quick-json-serialize-nested ()
  "测试嵌套结构的 JSON 序列化"
  (let ((data '((menus . ((platform . ((agents . nil))))))))
    (should (stringp (json-serialize data
                                     :null-object :null
                                     :false-object :false)))))

(ert-deftest i18n-quick-json-serialize-deep-nested ()
  "测试深层嵌套（5层）的 JSON 序列化，模拟 menus.platform.agents.detail.titles"
  (let* ((keys '("menus" "platform" "agents" "detail" "titles"))
         (result (i18n-quick--update-alist nil keys "")))
    ;; 验证 alist 结构正确（keys 是 symbols）
    (should (equal result '((menus . ((platform . ((agents . ((detail . ((titles . ""))))))))))))
    ;; 验证能序列化为 JSON
    (should (stringp (json-serialize result
                                     :null-object :null
                                     :false-object :false)))))

(ert-deftest i18n-quick-json-serialize-with-nil-value ()
  "测试包含 nil 值的 JSON 序列化"
  (let ((data '((key1 . "value1")
                (key2 . :null)  ; 明确的 null 值
                (key3 . "value3"))))  ; symbols
    (should (stringp (json-serialize data
                                     :null-object :null
                                     :false-object :false)))))

(ert-deftest i18n-quick-json-serialize-empty-to-nested ()
  "测试从空对象创建深层嵌套的完整流程"
  ;; 模拟真实场景：从空的 menus.json 创建新的 key
  (let* ((existing nil)  ; 空文件
         (keys '("menus" "platform" "agents" "detail" "titles"))
         (result (i18n-quick--update-alist existing keys "")))
    ;; 验证结构
    (should (consp result))
    (should (assq 'menus result))  ; 使用 assq 查找 symbol
    ;; 验证能序列化
    (let ((json-str (json-serialize result
                                    :null-object :null
                                    :false-object :false)))
      (should (stringp json-str))
      (should (string-match "menus" json-str))
      (should (string-match "platform" json-str))
      (should (string-match "agents" json-str))
      (should (string-match "detail" json-str))
      (should (string-match "titles" json-str)))))

(ert-deftest i18n-quick-json-serialize-partial-existing ()
  "测试部分存在时的合并和序列化"
  ;; 模拟：menus.platform 已存在，添加 agents.detail.titles
  (let* ((existing '((menus . ((platform . ((existing . "已存在")))))))  ; symbols
         (keys '("menus" "platform" "agents" "detail" "titles"))
         (result (i18n-quick--update-alist existing keys "")))
    ;; 验证旧数据保留（使用 assq 查找 symbols）
    (let ((platform (cdr (assq 'menus result))))
      (should (assq 'existing (cdr (assq 'platform platform))))
      (should (assq 'agents (cdr (assq 'platform platform)))))
    ;; 验证能序列化
    (should (stringp (json-serialize result
                                     :null-object :null
                                     :false-object :false)))))

(provide 'i18n-quick-test)
