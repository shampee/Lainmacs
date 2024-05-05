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

;; Load theme for Lainmacs
(load-theme 'BellePintosGrande t)

;; Set font
(set-frame-font "Go Mono 11" nil t)
(add-to-list 'default-frame-alist '(font . "Go Mono 11"))
;; Load config.org for init.el configuration
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; TODO: We should bind these!
(defun open-init ()
  (find-file "~/.emacs.d/init.el"))
(defun open-conf ()
  (find-file "~/.emacs.d/config.org"))
(defun reload ()
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(BellePintosGrande))
 '(package-selected-packages
   '(beacon git-gutter zig-mode yasnippet-snippets which-key vertico-posframe undo-tree treemacs-icons-dired treemacs-evil tracking switch-window spaceline sly-asdf rustic ranger rainbow-delimiters quelpa-use-package paredit page-break-lines org-roam org-remark org-ref org-noter orderless mix marginalia magit lua-mode lsp-ui lsp-ivy lsp-haskell lispy lfe-mode ivy-youtube ido-vertical-mode hl-todo goggles go-mode glue geiser-guile geiser-chicken flymake-lua flycheck-clojure fennel-mode exunit evil-collection embark-consult elixir-mode diminish deadgrep dashboard dap-mode corfu copilot consult-lsp consult-hoogle chatgpt carp-emacs cape auto-package-update async affe a)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 
