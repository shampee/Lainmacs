;;; Major mode for uLisp
;;; M-x ulisp-connect (or C-u M-x ulisp-connect to change the default serial device).
;;;
;;; C-x C-e         eval last sexp
;;; C-c C-e         eval defun
;;; C-c C-k         eval buffer
;;; C-c C-k         eval region
;;; C-c C-t         terminate running program

(require 'inf-lisp)

(defvar ulisp-port-name "/dev/ttyACM0"
  "Serial port name for uLisp.")

(defvar ulisp-port-speed 9600
  "Serial port speed for uLisp.")

(defvar ulisp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") 'ulisp-terminate)
    (define-key map (kbd "C-x C-e") 'lisp-eval-last-sexp)
    (define-key map (kbd "C-c C-e") 'lisp-eval-defun)
    (define-key map (kbd "C-c C-k") 'ulisp-eval-buffer)
    (define-key map (kbd "C-c C-r") 'lisp-eval-region)
    map)
  "Keymap for `ulisp-mode'.")

(defvar ulisp--inferior-buffer-name "*ulisp*")
(defvar ulisp--inferior-process-name "ulisp")

(define-derived-mode ulisp-mode lisp-data-mode "uLisp"
  "Major mode for editing programs in uLisp"
  (setq-local inferior-lisp-buffer ulisp--inferior-buffer-name))

(defun ulisp-eval-buffer ()
  (interactive)
  (comint-send-region (inferior-lisp-proc) (point-min) (point-max))
  (comint-send-string (inferior-lisp-proc) "\n"))

(defun ulisp-connect (port speed)
  "Run a uLisp serial process, input and output via buffer *ulisp*."
  (interactive
   (if current-prefix-arg
       (list
        (read-file-name "Serial port: " "/dev" ulisp-port-name)
        ulisp-port-speed)
     (list ulisp-port-name ulisp-port-speed)))
  (if (get-buffer ulisp--inferior-buffer-name)
      (pop-to-buffer ulisp--inferior-buffer-name)
    (progn
      (with-current-buffer
          (make-comint-in-buffer ulisp--inferior-process-name
                                 ulisp--inferior-buffer-name
                                 nil)
        (setq-local inferior-lisp-buffer ulisp--inferior-buffer-name)
        (inferior-lisp-mode))
      (when (not (string-equal
                  "serial"
                  (process-type
                   (get-buffer-process ulisp--inferior-buffer-name))))
        (let ((process-connection-type nil))
          (make-serial-process
           :buffer ulisp--inferior-buffer-name
           :port port
           :speed speed
           :noquery t)))
      (pop-to-buffer ulisp--inferior-buffer-name))))
      
(defun ulisp-terminate ()
  "Terminate running program."
  (interactive)
  (let ((b (get-buffer ulisp--inferior-buffer-name))
        (p (get-process ulisp--inferior-process-name)))
    (if (and b p)
        (with-current-buffer b
          (comint-send-string (inferior-lisp-proc) "~\n"))
      (message "uLisp is not running."))))      

(add-to-list 'auto-mode-alist '("\\.ulisp\\'" . ulisp-mode))

(provide 'ulisp-mode)
