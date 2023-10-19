;;; init-latex.el --- latex support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package pdf-tools
  :ensure t
  :init
  (pdf-loader-install))
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))

(defun pdf-view-kill-rmn-ring-save ()
  "Copy the region to the `kill-ring' after remove all newline characters."
  (interactive)
  (pdf-view-assert-active-region)
  (let* ((txt (replace-regexp-in-string "\n" " "
        (car (pdf-view-active-region-text)))))
    (pdf-view-deactivate-region)
	(kill-new txt)))

(use-package pdf-view-mode
  :bind
  ("C-c C-w" . pdf-view-kill-rmn-ring-save))

(provide 'init-latex)
;;; init-latex.el ends here