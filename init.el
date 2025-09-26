;;; init.el --- init file for Emacs  -*- lexical-binding: t; indent-tabs-mode: nil -*-


;;;; Garbage collector stuff

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

;; Garbage-collect on focus-out.
(add-function :after after-focus-change-function
  (defun m/garbage-collect-maybe ()
    (unless (frame-focus-state)
      (garbage-collect))))


;; Initialize melpa repo
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)


;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Initialize straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(use-package exec-path-from-shell :ensure t)

;; This sets $MANPATH, $PATH and exec-path from your shell,
;; but only when executed in a GUI frame on OS X and Linux.
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Get some nice themes
(use-package ef-themes :ensure t)
(use-package timu-rouge-theme :ensure t)
(use-package doric-themes :ensure t)
;; Load theme for Lainmacs
(load-theme 'doric-water t)

;; Set font
(set-frame-font "Aporetic Serif Mono 12" nil t)
(add-to-list 'default-frame-alist '(font . "Aporetic Serif Mono 12"))
; (setcar default-frame-alist '(font . "Iosevka Comfy 12"))

;; Load config.org for init.el configuration
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; Disable menu-bar, tool-bar and scroll-bar to increase the usable space.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Also shrink fringes to 1 pixel.
(fringe-mode 0)

;; Turn on `display-time-mode' if you don't use an external bar.
(setq display-time-default-load-average nil)
(display-time-mode t)

;; If you run Emacs as a daemon through systemd.
(when (daemonp)
  (exec-path-from-shell-initialize))


;;;; misc

;; TODO: We should bind these!
(defun open-init ()
  "Open Emacs init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(defun open-conf ()
  "Open config file."
  (interactive)
  (find-file "~/.emacs.d/config.org"))
(defun reload ()
  "Recompile `config.org' and reload the compiled elisp."
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d0c05a0b5a7619bca4c28d1bd2eeb15562afa634888a46c9e5b1d31b7d06ed36"
     "130bda3c4c328c13e6f91009e429ebd7522c6a0d49af6fc32b9fa1bc9b8cf753"
     "45333f79e4a7fdeff9924d5b6658f84fb468ef38f749455e5b58ba4154782007"
     "59c36051a521e3ea68dc530ded1c7be169cd19e8873b7994bfc02a216041bf3b"
     "19b62f442479efd3ca4c1cef81c2311579a98bbc0f3684b49cdf9321bd5dfdbf"
     "6af300029805f10970ebec4cea3134f381cd02f04c96acba083c76e2da23f3ec"
     "ac893acecb0f1cf2b6ccea5c70ea97516c13c2b80c07f3292c21d6eb0cb45239"
     "aff0396925324838889f011fd3f5a0b91652b88f5fd0611f7b10021cc76f9e09"
     "90185f1d8362727f2aeac7a3d67d3aec789f55c10bb47dada4eefb2e14aa5d01"
     "b9c002dc827fb75b825da3311935c9f505d48d7ee48f470f0aa7ac5d2a595ab2"
     "3d9938bbef24ecee9f2632cb25339bf2312d062b398f0dfb99b918f8f11e11b1"
     "ae20535e46a88faea5d65775ca5510c7385cbf334dfa7dde93c0cd22ed663ba0"
     "36c5acdaf85dda0dad1dd3ad643aacd478fb967960ee1f83981d160c52b3c8ac"
     "d609d9aaf89d935677b04d34e4449ba3f8bbfdcaaeeaab3d21ee035f43321ff1"
     "e85a354f77ae6c2e47667370a8beddf02e8772a02e1f7edb7089e793f4762a45"
     "d6b369a3f09f34cdbaed93eeefcc6a0e05e135d187252e01b0031559b1671e97"
     default))
 '(elfeed-feeds '("https://sachachua.com/blog/feed"))
 '(lisp-indent-function 'common-lisp-indent-function)
 '(mastodon-images-in-notifs t)
 '(mastodon-tl--show-avatars t)
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name ":name" :query "fuck you")))
 '(notmuch-search-oldest-first nil)
 '(org-agenda-files '("~/src/org/todo.org"))
 '(package-selected-packages 'nil)
 '(send-mail-function 'sendmail-send-it)
 '(simple-modeline-mode t)
 '(virtualenv-root "~/src/python/cv/"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 
