;;; init.el --- init file for Emacs  -*- lexical-binding: t; indent-tabs-mode: nil -*-



;; Garbage-collect on focus-out.
(add-function :after after-focus-change-function
  (defun m/garbage-collect-maybe ()
    (unless (frame-focus-state)
      (garbage-collect))))


;; Initialize melpa repo
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)


;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package exec-path-from-shell :ensure t)

;; Import the shell environment once, before configuring external programs.
(when (or (daemonp) (display-graphic-p))
  (exec-path-from-shell-initialize))

;; Keep alternate themes available without loading them at startup.
(use-package ef-themes :ensure t :defer t)
(use-package timu-rouge-theme :ensure t :defer t)
(use-package doric-themes :ensure t :defer t)
(use-package doom-themes :ensure t :demand t)
(load-theme 'doom-ayu-dark t)


;; Set font
;; (set-frame-font "Ttyp0 OTB 11" nil t)
;; (add-to-list 'default-frame-alist '(font . "Ttyp0 OTB 11"))
(add-to-list 'default-frame-alist '(font . "Aporetic Serif Mono 12"))
(when (display-graphic-p)
  (set-frame-font "Aporetic Serif Mono 12" nil t))

;; Load the literate configuration, then saved Custom settings.
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;; Shrink fringes to 1 pixel (bars are disabled in early-init.el and config.org).
(when (fboundp 'fringe-mode) (fringe-mode '(1 . 1)))

;; Turn on `display-time-mode' if you don't use an external bar.
(setq display-time-default-load-average nil)
(display-time-mode t)

(require 'server)
(unless (server-running-p)
  (server-start))


;;;; misc

;; TODO: We should bind these!
(defun open-init ()
  "Open Emacs init file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))
(defun open-conf ()
  "Open config file."
  (interactive)
  (find-file (expand-file-name "config.org" user-emacs-directory)))
(defun reload ()
  "Tangle `config.org' when needed and reload its Emacs Lisp."
  (interactive)
  (org-babel-load-file (expand-file-name "config.org" user-emacs-directory)))

