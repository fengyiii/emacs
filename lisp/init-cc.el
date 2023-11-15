;;; init-cc.el --- Support elisp manually installed in the package dir -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 需要安装 llvm clangd
(use-package c++-mode
  :functions 			; suppress warnings
  c-toggle-hungry-state
  :hook
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (c++-mode . c-toggle-hungry-state))

(use-package dap-lldb
  :after dap-mode
  :custom
  (dap-lldb-debug-program '("/usr/local/opt/llvm/bin/lldb-vscode"))
  ;; ask user for executable to debug if not specified explicitly (c++)
  (dap-lldb-debugged-program-function
    (lambda () (read-file-name "Select file to debug: "))))


(provide 'init-cc)
;;; init-cc.el ends here
