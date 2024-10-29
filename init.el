;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "27.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Bootstrap config
; 以上为抄来的不出需要修改的部分
(require 'common)
(require 'init-theme)
(require 'init-package)
(require 'init-lsp)
(require 'init-git)
(require 'init-cc)
(require 'init-markdown)
;;(require 'init-tab)
(require 'cc-mode)
(require 'init-pinyin)
(require 'init-latex)
(require 'init-org-download)

(setq server-use-tcp t)  ; 启用 TCP 连接（如果 Unix socket 有问题）
;;(server-start)           ; 手动确保启动 Emacs 服务器

;; 去除#文件
(setq create-lockfiles nil)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#E74C3C" "#A6E22E" "#E6DB74" "#268bd2" "#F92660" "#66D9EF" "#F8F8F2"])
 '(c-require-final-newline
   '((c-mode nil)
     (c++-mode nil)
     (objc-mode nil)
     (java-mode nil)
     (idl-mode nil)
     (pike-mode nil)
     (awk-mode nil)))
 '(custom-enabled-themes '(doom-dark+))
 '(custom-safe-themes
   '("8d8207a39e18e2cc95ebddf62f841442d36fcba01a2a9451773d4ed30b632443" "37b6695bae243145fa2dfb41440c204cd22833c25cd1993b0f258905b9e65577" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "c8b3d9364302b16318e0f231981e94cbe4806cb5cde5732c3e5c3e05e1472434" "a9dc7790550dcdb88a23d9f81cc0333490529a20e160a8599a6ceaf1104192b5" "9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392" "0d0936adf23bba16569c73876991168d0aed969d1e095c3f68d61f87dd7bab9a" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290" "f4d1b183465f2d29b7a2e9dbe87ccc20598e79738e5d29fc52ec8fb8c576fcfd" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "38c0c668d8ac3841cb9608522ca116067177c92feeabc6f002a27249976d7434" "162201cf5b5899938cfaec99c8cb35a2f1bf0775fc9ccbf5e63130a1ea217213" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "9e1cf0f16477d0da814691c1b9add22d7cb34e0bb3334db7822424a449d20078" "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad" "e14884c30d875c64f6a9cdd68fe87ef94385550cab4890182197b95d53a7cf40" "c1d5759fcb18b20fd95357dcd63ff90780283b14023422765d531330a3d3cec2" "70e7f094987e3c6226c54078dd986e11cab7246ea1c9e58a9907afa90f3c10c9" "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8" "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "badd1a5e20bd0c29f4fe863f3b480992c65ef1fa63951f59aa5d6b129a3f9c4c" "680f62b751481cc5b5b44aeab824e5683cf13792c006aeba1c25ce2d89826426" "a44e2d1636a0114c5e407a748841f6723ed442dc3a0ed086542dc71b92a87aee" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "b1acc21dcb556407306eccd73f90eb7d69664380483b18496d9c5ccc5968ab43" "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9" "c517e98fa036a0c21af481aadd2bdd6f44495be3d4ac2ce9d69201fcb2578533" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1" "6f96a9ece5fdd0d3e04daea6aa63e13be26b48717820aa7b5889c602764cf23a" "8b148cf8154d34917dfc794b5d0fe65f21e9155977a36a5985f89c09a9669aa0" "bf948e3f55a8cd1f420373410911d0a50be5a04a8886cabe8d8e471ad8fdba8e" default))
 '(exwm-floating-border-color "#3c3d38")
 '(fci-rule-color "#555556")
 '(highlight-tail-colors ((("#333a23") . 0) (("#2d3936") . 20)))
 '(ispell-dictionary nil)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#FD971F"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#A6E22E"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#525254"))
 '(mode-require-final-newline nil)
 '(objed-cursor-color "#E74C3C")
 '(package-selected-packages
   '(general rime doom-themes modern-cpp-font-lock all-the-icons docstr pdf-tools xref helm-xref all-the-icons-ivy pack use-package))
 '(pdf-view-midnight-colors (cons "#F8F8F2" "#272822"))
 '(require-final-newline nil)
 '(rustic-ansi-faces
   ["#272822" "#E74C3C" "#A6E22E" "#E6DB74" "#268bd2" "#F92660" "#66D9EF" "#F8F8F2"])
 '(safe-local-variable-values '((org-image-actual-width)))
 '(vc-annotate-background "#272822")
 '(vc-annotate-color-map
   (list
    (cons 20 "#A6E22E")
    (cons 40 "#bbdf45")
    (cons 60 "#d0dd5c")
    (cons 80 "#E6DB74")
    (cons 100 "#edc457")
    (cons 120 "#f5ad3b")
    (cons 140 "#FD971F")
    (cons 160 "#fb7134")
    (cons 180 "#fa4b4a")
    (cons 200 "#F92660")
    (cons 220 "#f33254")
    (cons 240 "#ed3f48")
    (cons 260 "#E74C3C")
    (cons 280 "#c14d41")
    (cons 300 "#9c4f48")
    (cons 320 "#77504e")
    (cons 340 "#555556")
    (cons 360 "#555556")))
 '(vc-annotate-very-old-color nil))
;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(whitespace-tab ((t (:foreground "#636363")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

