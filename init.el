;;; init.el --- init file for Emacs  -*- lexical-binding: t; indent-tabs-mode: nil -*-

(require 's)


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

(use-package exec-path-from-shell :ensure t)

;; This sets $MANPATH, $PATH and exec-path from your shell,
;; but only when executed in a GUI frame on OS X and Linux.
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Load theme for Lainmacs
(use-package ef-themes :ensure t)
(load-theme 'ef-dream t)

(save-excursion
  (exwm-workspace-switch-create 9)
  (exwm-workspace-switch-create 0))

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

(defun normalize-init-entry (lst)
  "Make sure LST has a message.
Populate the CDR of LST with message \"Running command\" if there is none."
  (when lst
    (let ((_ (car lst))
          (message (cdr lst)))
      (unless message
        (setcdr lst "Running command"))))
  lst)

(defun run-and-notify (init-entry &optional notify-p)
  "INIT-ENTRY is an alist of a command and a message.
Run the command and send a notification if NOTIFY-P is T."
  (let* ((init-entry (normalize-init-entry init-entry))
         (program (car init-entry))
         (message (cdr init-entry))
         (run (lambda (p) (start-process-shell-command p nil p)))
         (notify (lambda (p m) (notifications-notify :title m :body p))))
    
    (start-process-shell-command program nil program)
    (when notify-p (notifications-notify :title message :body program))))

;; Start dunst so we can see the notifications.
(start-process-shell-command "dunst" nil "dunst")

(let ((entries '(("sh $HOME/.fehbg" . "Setting wallpaper") ; t
                 ;; ("picom --blur-background-fixed -b" . "Starting picom to enable transparency") ; nil
                 )))
  (dolist (entry entries) (run-and-notify (normalize-init-entry entry) t)))


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
  "Toggle between if the window should be floating or not."
  (exwm-floating-toggle-floating))

(defun toggle-modeline ()
  "Toggle between if the window should have a modeline or not."
  (exwm-layout-toggle-mode-line))

(defun toggle-float-and-modeline ()
  "Do just that."
  (toggle-float)
  (toggle-modeline))

;; Configuring windows when they appear
(defun configure-window-by-class ()
  "Configure window by class."
  (interactive)
  (pcase exwm-class-name
    ;("Alacritty" (toggle-modeline))
    ("Firefox-esr" (toggle-modeline))
    ("xarchiver" (toggle-float-and-modeline))
    ("mpv" (toggle-float-and-modeline))
    ("pavucontrol" (toggle-float-and-modeline))
    ("Godot" (toggle-float-and-modeline))
    ("Blender" (toggle-float-and-modeline))
    ("electrum" (toggle-float-and-modeline))
    ("Gpa" (toggle-float-and-modeline))
    ("Tor Browser" (toggle-float-and-modeline))))
(add-hook 'exwm-manage-finish-hook #'configure-window-by-class)

(defun configure-window-by-title ()
  "Configure window by title."
  (interactive)
  (cl-flet
      ((title= (lambda (str) (string= str exwm-title)))
       (title-contains-p (lambda (str) (not (null (cl-search str exwm-title))))))
    (cond
     ((title= "CEPL") (toggle-float-and-modeline))
     ((title= "Volume Control" ) (toggle-float-and-modeline))
     ((title-contains-p "GNU Privacy Assistant") (toggle-float-and-modeline))
     ((title-contains-p "xarchiver") (toggle-float-and-modeline))
     (t nil))))
(add-hook 'exwm-manage-finish-hook #'configure-window-by-title)


;; Helper functions for keybindings.

(defun launch-terminal ()
  "Launch terminal."
  (interactive)
  (start-process-shell-command "alacritty" nil "alacritty"))

(defun show-free-mem ()
  "Show how much memory is used and how much is available."
  (interactive)
  (shell-command "free -mh"))

(defun open-emacs-terminal ()
  "Start up an Eat terminal."
  (interactive)
  (eat))

(defun toggle-ld-lib ()
  "Toggle between setting LD_LIBRARY_PATH to an empty string and LIBRARY_PATH."
  (interactive)
  (pcase (getenv "LD_LIBRARY_PATH")
    ("" (progn
          (setenv "LD_LIBRARY_PATH"
                  (concat "/usr/lib/x86_64-linux-gnu:" (getenv "LIBRARY_PATH")))
          (message "Setting LD_LIBRARY_PATH to %S" (getenv "LD_LIBRARY_PATH"))))
    (_ (progn (message "Unsetting LD_LBIRARY_PATH.")
              (setenv "LD_LIBRARY_PATH" "")))))

;; TODO: Possible cleanup.
(defun ncspot-control (command &optional n)
  "COMMAND being one of the following:
playpause|pause, stop, prev, next, save|love, volup, voldown.
N is optional and works in conjunction with [inc]/[dec]reasing volume by N.
See: https://gist.github.com/shampee/0c38ab31b40c3b45a61c15fc7a258d81"
  (if (string= command "status")
      (string-trim (shell-command-to-string "ncspot-controller") "\"" "[\"\\\nla]+")
    (async-start-process "ncspot-control" "ncspot-controller" 'ignore command (if (null n) "" n))))

(defun ncspot-play/pause ()
  "Play/pause toggle."
  (interactive)
  (ncspot-control "pause"))

(defun ncspot-next ()
  "Play next song."
  (interactive)
  (ncspot-control "next"))

(defun ncspot-prev ()
  "Play previous song."
  (interactive)
  (ncspot-control "prev"))

(defun ncspot-save ()
  "Save song."
  (interactive)
  (ncspot-control "save"))

(defun ncspot-volup ()
  "Increase the volume by 1."
  (interactive)
  (ncspot-control "volup"))

(defun ncspot-voldown ()
  "Decrease the volume by 1."
  (interactive)
  (ncspot-control "voldown"))

(defun ncspot-status ()
  "Return currently played song."
  (interactive)
  (ncspot-control "status"))

(defun screenshot-to-clipboard (&optional select)
  (if select
      (async-start-process "screenshot" "scr-to-clip" 'ignore "-s")
    (async-start-process "screenshot" "scr-to-clip" 'ignore "")))

(defun scr-clip ()
  (interactive)
  (screenshot-to-clipboard))

(defun scr-clip-select ()
  (interactive)
  (screenshot-to-clipboard t))

(defun exwm-input-set-keys (keybinds)
  "KEYBINDS is a list of lists containing (keybind function)."
  (dolist (key-commands keybinds)
    (let ((keys (car key-commands))
          (command (cadr key-commands)))
      (exwm-input-set-key (kbd keys) command))))

(exwm-input-set-keys '(("s-<return>" open-emacs-terminal)
                       ("s-p" toggle-ld-lib)
                       ("s-?" dictionary-search)
                       ("<Print>" scr-clip)
                       ("C-<Print>" scr-clip-select)
                       ("C-<XF86AudioPlay>" ncspot-play/pause)
                       ("C-<XF86AudioNext>" ncspot-next)
                       ("C-<XF86AudioPrev>" ncspot-prev)
                       ("C-<XF86AudioLowerVolume>" ncspot-voldown)
                       ("C-<XF86AudioRaiseVolume>" ncspot-volup)))

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

        ([?\s-f] . show-free-mem)
        ([?\s-t] . launch-terminal)

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
                     (interactive
                      (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))))





;;;; Randr
(setq exwm-randr-workspace-monitor-plist '(0 "HDMI-1-3" 1 "DP-1"))
(add-hook 'exwm-randr-screen-change-hook
	  (lambda ()
	    (start-process-shell-command
	      "xrandr" nil
	      "xrandr --output DP-1 --mode 1920x1080 --rate 75 --left-of HDMI-1-3 --output HDMI-1-3 --mode 1920x1080 --rate 75")))
(exwm-randr-mode 1)
(start-process-shell-command "ForceCompositionPipeline" nil "$HOME/bin/force-comp")


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
        ([?\C-k] . [S-end delete])
        ;; cut/paste.
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])))


(defvar auto-char-mode-by-class
  '("Tor Browser" "Godot" "Thunderbird" "gpa" "electrum" "mpv" "pavucontrol" "Zathura"))
(defvar auto-char-mode-by-title
  '("ikatube"))

(setq exwm-manage-configurations
      '(((member exwm-class-name auto-char-mode-by-class)
         char-mode t)
        ((member exwm-title auto-char-mode-by-title)
         char-mode t)))

(defvar ignore-simulation-keys-apps
  '("Alacritty" "ikatube" "Tor Browser" "qutebrowser" "Godot" "zathura"
    "Firefox-esr" "Thunderbird" "gpa" "mpv" "xfce4-terminal" "pavucontrol"))

(defun ignore-simkeys ()
  "Ignore simulation keys for the applications in `ignore-simulation-keys-apps'."
  (interactive)
  (when (or
         (and exwm-class-name
              (-any (lambda (name) (string= exwm-class-name name)) ignore-simulation-keys-apps))
         (s-contains-p "ikatube" exwm-title))
    ;; (exwm-input-set-simulation-key ...)
    (exwm-input-set-local-simulation-keys nil)))
(add-hook 'exwm-manage-finish-hook 'ignore-simkeys)




;; Do this before exwm-init
;(perspective-exwm-mode)

;; You can hide the minibuffer and echo area when they're not used, by
;; uncommenting the following line.
;(setq exwm-workspace-minibuffer-position 'bottom)

;; Do not forget to enable EXWM. It will start by itself when things are
;; ready.  You can put it _anywhere_ in your configuration.
(exwm-enable)


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
 '(package-selected-packages
   '())
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 
