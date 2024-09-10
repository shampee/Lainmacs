;;; init.el --- init file for Emacs  -*- lexical-binding: t; indent-tabs-mode: nil -*-


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
;

;; Initialize melpa repo
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

;; This sets $MANPATH, $PATH and exec-path from your shell,
;; but only when executed in a GUI frame on OS X and Linux.
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; If you run Emacs as a daemon through systemd.
(when (daemonp)
  (exec-path-from-shell-initialize))

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

;; Load theme for Lainmacs
(load-theme 'ef-dream t)



;; Set font
(set-frame-font "Iosevka Comfy Motion 12" nil t)
(add-to-list 'default-frame-alist '(font . "Iosevka Comfy Motion 12"))
;; Load config.org for init.el configuration
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))


;; Exwm 

;; Disable menu-bar, tool-bar and scroll-bar to increase the usable space.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Also shrink fringes to 1 pixel.
(fringe-mode 1)

;; Turn on `display-time-mode' if you don't use an external bar.
(setq display-time-default-load-average nil)
(display-time-mode t)

;; You are strongly encouraged to enable something like `icomplete-vertical-mode' to alter
;; the default behavior of 'C-x b', or you will take great pains to switch
;; to or back from a floating frame (remember 'C-x 5 o' if you refuse this
;; proposal however).
;(icomplete-vertical-mode 1)

;; Emacs server is not required to run EXWM but it has some interesting uses
;; (see next section).
(server-start)


;;;; Init
(defun run-and-notify (program)
  "Run PROGRAM and notify that it has done so."
  (cl-flet ((run (lambda (p)
				   (start-process-shell-command p nil p)))
			(notify (lambda (p)
					  (start-process-shell-command
					   p nil
					   (format "%s %S" "notify-send \"Running command\"" p)))))
	(run program)
	(notify program)))

(let ((programs '("sh $HOME/.fehbg")))
  (cl-loop for program in programs
		   do (run-and-notify program)))


;;;; Below are configurations for EXWM.

;; Add paths (not required if EXWM is installed from GNU ELPA).
;(add-to-list 'load-path "/path/to/xelb/")
;(add-to-list 'load-path "/path/to/exwm/")

;; Load EXWM.
(require 'exwm)

;; Set the initial number of workspaces (they can also be created later).
(setq exwm-workspace-number 9)

;; All buffers created in EXWM mode are named "*EXWM*". You may want to
;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
;; are run when a new X window class name or title is available.  Here's
;; some advice on this topic:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + For applications with multiple windows (e.g. GIMP), the class names of
;    all windows are probably the same.  Using window titles for them makes
;;   more sense.
;; In the following example, we use class names for all windows except for
;; Java applications and GIMP.
(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

(defun toggle-float ()
  (exwm-floating-toggle-floating))
(defun toggle-modeline ()
	(exwm-layout-toggle-mode-line))
(defun toggle-float-and-modeline ()
  "Does just that."
  (toggle-float)
  (toggle-modeline))

;; Configuring windows when they appear
(defun configure-window-by-class ()
  "Configure window by class."
  (pcase exwm-class-name
	("mpv" (toggle-float-and-modeline))
	("pavucontrol" (toggle-float-and-modeline))
	("Godot" (toggle-float-and-modeline))
	("Blender" (toggle-float-and-modeline))
	("Electrum" (toggle-float-and-modeline))
	("gpa" (toggle-float-and-modeline))
	("Tor Browser" (toggle-float-and-modeline))))
(add-hook 'exwm-manage-finish-hook #'configure-window-by-class)

(require 's)
(defun configure-window-by-title ()
  "Configure window by title."
  (pcase exwm-title
	(`(lambda (,title) (s-contains-p "Godot" ,title)) (toggle-float-and-modeline))
	("CEPL" (exwm-floating-toggle-floating))))
(add-hook 'exwm-manage-finish-hook #'configure-window-by-title)

;; Global keybindings can be defined with `exwm-input-global-keys'.
;; Here are a few examples:
(setq exwm-input-global-keys
      `(
        ;; Bind "s-r" to exit char-mode and fullscreen mode.
        ([?\s-r] . exwm-reset)
        ;; Bind "s-w" to switch workspace interactively.
        ([?\s-w] . exwm-workspace-switch)
        ;; Bind "s-m" to move workspace interactively.
        ([?\s-m] . exwm-workspace-move)
        ;; Bind "s-f" to show used & available memory
        ([?\s-f] . (shell-command "free -mh"))
        ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))
        ;; Bind "s-d" to launch applications ('M-&' also works if the output
        ;; buffer does not bother you).
        ([?\s-d] . (lambda (command)
					 (interactive (list (read-shell-command "$ ")))
					 (start-process-shell-command command nil (format "LD_LIBRARY_PATH= %s" command))))))



;;;; Randr
(setq exwm-randr-workspace-monitor-plist '(0 "HDMI-1-3" 1 "DP-1"))
(add-hook 'exwm-randr-screen-change-hook
		  (lambda ()
			(start-process-shell-command
			 "xrandr" nil "xrandr --output DP-1 --mode 1920x1080 --rate 75 --left-of HDMI-1-3 --output HDMI-1-3 --mode 1920x1080 --rate 75")))
(exwm-randr-mode 1)


;; System tray is disabled by default
;(exwm-systemtray-mode 1)


;;;; Simulation keys

;; To add a key binding only available in line-mode, simply define it in
;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

;; The following example demonstrates how to use simulation keys to mimic
;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
;; and DEST is what EXWM actually sends to application.  Note that both SRC
;; and DEST should be key sequences (vector or string).
(setq exwm-input-simulation-keys
      '(
        ;; movement
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-u] . [up])
        ([?\C-k] . [S-end delete])
        ;; cut/paste.
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])))

(require 'dash) ;; include `-any'
(defvar ignore-simulation-keys-apps '("ikatube" "Tor Browser" "qutebrowser" "Godot" "mpv"
									  "Firefox" "Thunderbird" "gpa" "xfce4-terminal" "pavucontrol"))
(add-hook 'exwm-manage-finish-hook
		  (lambda ()
			(when (and exwm-class-name
					   (-any (lambda (name) (string= exwm-class-name name)) ignore-simulation-keys-apps))
			  (exwm-input-set-local-simulation-keys nil))))



;; You can hide the minibuffer and echo area when they're not used, by
;; uncommenting the following line.
;(setq exwm-workspace-minibuffer-position 'bottom)

;; Do not forget to enable EXWM. It will start by itself when things are
;; ready.  You can put it _anywhere_ in your configuration.
(exwm-enable)




;; TODO: We should bind these!
(defun open-init ()
  "Open Emacs init file."
  (find-file "~/.emacs.d/init.el"))
(defun open-conf ()
  "Open config file."
  (find-file "~/.emacs.d/config.org"))
(defun reload ()
  "Recompile `config.org' and reload the compiled elisp."
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
 
 
