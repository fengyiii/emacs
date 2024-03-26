;;; init-cc.el --- Support elisp manually installed in the package dir -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; =====================================================
;; Programming Environment for C/C++ (using LSP)
;; =====================================================
;; Last modified on 27 Feb 2021

;; Dependencies:
;; - clangd: lsp C++ server, install by "brew install llvm"
;; - cmake, bear: generating build flags, install by brew
;;
;; Generating "compile_commands.json":
;; - cmake-based projects:
;;   enable by "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1"
;; - other build systems, use Bear:
;;   run "make clean", then "bear -- make" to generate it.

(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
              ("C-c C-c" . compile)
	      ("C-c C-a" . compile-all-and-run)
	      ("C-c C-t" . compile-test-and-run))
  :init
  (setq-default c-default-style "linux"  ;; "stroustrup"
                c-basic-offset 4)
  :config
  ;; code fontify
  (use-package modern-cpp-font-lock
    :diminish (modern-c++-font-lock-mode)
    :init (modern-c++-font-lock-global-mode t))
  )

(defun compile-all-and-run ()
  "Compile and run the current C/C++ file in Eshell."
  (interactive)
  (save-buffer)
  (let* ((filename (buffer-file-name))
         (directory (file-name-directory filename))
         (cmd (format "make clean && make all && %s/demo.exe" directory)))
    (eshell)
    (eshell-send-input)
    (insert cmd)
    (eshell-send-input)))

(defun compile-test-and-run ()
  "Compile and run the current C/C++ file in Eshell."
  (interactive)
  (save-buffer)
  (let* ((filename (buffer-file-name))
         (directory (file-name-directory filename))
         (cmd (format "make clean && make test && %s/demo.exe" directory)))
    (eshell)
    (eshell-send-input)
    (insert cmd)
    (eshell-send-input)))


;; ------------------------------------------------
;; Other editing supports
;; ------------------------------------------------

;; /smartparens/: insert pair of symbols
;; when you press RET, the curly braces automatically add another newline
(with-eval-after-load "smartparens"
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '(("| " "SPC") ("* ||\n[i]" "RET")))))

;; CMake supports
(require 'init-cmake)

(provide 'init-cc)
;;; init-cc.el ends here
