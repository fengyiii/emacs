;;; init-org-download.el --- Support elisp manually installed in the package dir -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org-download
  :ensure t
  :defer t
  :init
  ;; Add handlers for drag-and-drop when Org is loaded.
  (with-eval-after-load 'org
    (org-download-enable))
  :bind (("C-S-y" . org-download-screenshot)
    ("C-S-p" . org-download-clipboard))
  :config
    (setq-default org-download-image-dir "./images/")
    (add-hook 'dired-mode-hook 'org-download-enable))

(setq org-startup-indented t
	org-export-preserve-breaks t)

(provide 'init-org-download)
;;; init-org-download.el ends here