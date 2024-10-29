;;; init-pinyin.el --- Support elisp manually installed in the package dir -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package general
  :ensure t)

(use-package rime
  :ensure t
  :general
  ;; 配置快捷键
  (:keymaps 'rime-active-mode-map
            "M-DEL" 'rime--escape
            "C-w" 'rime--escape)
  (:keymaps '(evil-insert-state-map minibuffer-local-map ivy-minibuffer-map)
            "C-\\" 'convert-code-or-disable-rime)
  :custom
  (default-input-method "rime")
  :config
  ;; 配置 `popup` 依赖包
  (use-package popup)

  ;; 配置 Rime 的相关设置
  (setq default-input-method "rime"
        rime-user-data-dir "~/.config/emacs_rime"
        rime-show-candidate 'posframe
        rime-posframe-properties
        (list :background-color "#333333"
              :foreground-color "#dcdccc"
              :internal-border-width 10))

  ;; 手动同步 ibus 和 Emacs Rime 的用户数据
  (defun sync-ibus-and-emacs-rime-userdb ()
    (interactive)
    (rime-sync)
    (start-process-shell-command
     ""
     nil
     "ibus exit;cd ~/.config/ibus/rime; rime_dict_manager -s;ibus-daemon --xim -d -r")
    (message "ibus-rime and emacs rime sync done"))

  ;; 光标颜色设置
  (defvar input-method-cursor-color "white"
    "Cursor color when an input method is active.")
  (defvar default-cursor-color (frame-parameter nil 'cursor-color)
    "Default cursor color.")

  ;; 更新光标颜色函数
  (defun change-cursor-color-on-input-method ()
    "Change cursor color based on the input method status."
    (interactive)
    (set-cursor-color (if current-input-method
                          input-method-cursor-color
                        default-cursor-color)))

  ;; 添加钩子和 advice，确保在切换输入法和命令执行时更新光标颜色
  (add-hook 'post-command-hook 'change-cursor-color-on-input-method)
  (advice-add 'toggle-input-method :after 'change-cursor-color-on-input-method)

  ;; 配置 Rime 的禁用条件
  (setq rime-disable-predicates
        '(rime-predicate-after-ascii-char-p
          rime-predicate-evil-mode-p
          rime-predicate-punctuation-after-space-cc-p
          rime-predicate-punctuation-after-ascii-p
          rime-predicate-punctuation-line-begin-p
          rime-predicate-org-in-src-block-p
          rime-predicate-prog-in-code-p
          rime-predicate-org-latex-mode-p
          rime-predicate-space-after-cc-p
          rime-predicate-current-uppercase-letter-p
          rime-predicate-tex-math-or-command-p)))
(provide 'init-pinyin)
;;; init-pinyin.el ends here
