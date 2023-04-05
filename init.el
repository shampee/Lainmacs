;; Make emacs startup faster
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
 
(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
 
(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))
 
(defun startup/reset-gc ()
  (setq gc-cons-threshold 16777216
    gc-cons-percentage 0.1))
 
(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'startup/reset-gc)
;;

;; Initialize melpa repo
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
        '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load Witchmacs theme
(load-theme 'kaolin-mono-light t)

;; Set font
(set-frame-font "SGI Screen 10" nil t)

;; Load config.org for init.el configuration
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(kaolin-light))
 '(custom-safe-themes
   '("5a00018936fa1df1cd9d54bee02c8a64eafac941453ab48394e2ec2c498b834a" "249e100de137f516d56bcf2e98c1e3f9e1e8a6dce50726c974fa6838fbfcec6b" "c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428" "e0e363ecc40c5c3476f74570731d0f7d993d51f6b8f1913c443bdef079bbbbb2" "11cc65061e0a5410d6489af42f1d0f0478dbd181a9660f81a692ddc5f948bf34" "a131602c676b904a5509fff82649a639061bf948a5205327e0f5d1559e04f5ed" "2ce76d65a813fae8cfee5c207f46f2a256bac69dacbb096051a7a8651aa252b0" "81248e7e8ccce9fc9640a6efb2059c2e5fb68e3a90642806650f81002bf7de1d" "91bfe1fd1a701c69c9887be81458d44d5be6be73e78984159b42057dd0ad3601" "acb636fb88d15c6dd4432e7f197600a67a48fd35b54e82ea435d7cd52620c96d" "8796655c94190b16b194757ba41e33aedbc2aa91c31232c087e34d13ac1920b4" "fc6a63def3a8bdbfe0e3f845b5ef87e1a54458b2c76a534148ee4b72019d609a" "a17123ade1598b8e3d7039e35dfe69977b8f7e1a011b0ff899aabf89f0dbb0c5" default))
 '(doc-view-continuous t)
 '(evil-undo-system 'undo-redo)
 '(package-selected-packages
   '(consult-company consult-yasnippet consult-flycheck consult-org-roam eglot-box org-remark org-sword org-noter org-roam org-ref vterm kaolin-themes goggles embark-consult embark slime-company rust-mode company-shell ac-geiser parinfer-rust-mode vertico meghanada company-irony company-c-headers yasnippet-snippets yasnippet company magit treemacs-icons-dired treemacs-evil treemacs undo-tree page-break-lines async ido-vertical-mode switch-window avy beacon evil swiper which-key dashboard spaceline diminish auto-package-update htmlize use-package))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "#ef8ef" :foreground "#fg9fg"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "#ef8ef" :foreground "#ef8ef")))))
 
