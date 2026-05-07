;;; esp-scheme-mode.el --- Major mode for esp-scheme  -*- lexical-binding: t -*-

(require 'scheme)
(require 'sly nil t)


;; Customization

(defgroup esp-scheme nil
  "ESP-Scheme development mode."
  :group 'languages)

(defcustom esp-scheme-default-host "127.0.0.1"
  "Default host for esp-scheme SWANK connection."
  :type 'string
  :group 'esp-scheme)

(defcustom esp-scheme-default-port 4005
  "Default port for esp-scheme SWANK connection."
  :type 'integer
  :group 'esp-scheme)


;; Font lock

(defvar esp-scheme-builtins
  '(;; System
    "esp-reset" "heap-free" "delay-ms"
    ;; GPIO
    "pin-output" "pin-input" "pin-set!" "pin-get" "pin-toggle!"
    ;; SPI
    "spi-open" "spi-device" "spi-transfer!" "spi-write!" "spi-read!"
    ;; I2C
    "i2c-open" "i2c-write!" "i2c-read!" "i2c-write-read!"))

(defvar esp-scheme-standard-functions
  '(;; Stream
    "stream-read" "stream-read-exact" "stream-read-line"
    "stream-flush" "stream-write!" "stream-close"
    "send-message" "recv-message"
    "tcp-listen" "tcp-accept" "listener-close"

    ;; Arithmetic
    "+" "-" "*" "/" "=" "<" "<=" ">" ">="
    "expt" "sqrt" "log" "truncate" "floor"
    "ceiling" "round" "modulo" "remainder"
    "abs" "sin" "cos" "tan" "acos" "asin"
    "atan" "atan2" "min" "max"
    "1+" "1-" "2*" "2/"

    ;; Iteration
    "map" "for-each"

    ;; List
    "append" "list-ref" "list-tail" "list-last"

    ;; String
    "substring" "string-length" "string-ref" "string-append"
    "string-equal?" "string-uppercase?" "string-lowercase?"

    ;; Vector
    "make-vector" "vector" "vector-append" "vector-append!"
    "vector-length" "vector-ref" "vector-set!" "vector-fill!" "vector-copy"

    ;; Bits and bitwise
    "bitwise-and" "bitwise-or" "bitwise-xor" "bitwise-not" "bit-set?" "bit-count"
    "arithmetic-shift"

    ;; Bytevector
    "make-bytes" "make-bytes-from" "bytes-length" "bytes-ref"
    "bytes-set!" "bytes-fill!" "bytes-copy" "bytes-sort" "bytes-sort!"
    "bytes-append" "bytes-append!"

    ;; Conversion
    "write"
    "list->string" "string->list"
    "bytes->list" "list->bytes"
    "bytes->string" "string->bytes"
    "string->number" "number->string"
    "vector->list" "list->vector"
    "symbol->string" "string->symbol"
    "string->codepoint"

    ;; Type predicates
    "number?" "symbol?" "string?" "boolean?" "pair?" "procedure?"
    "port?" "stream?" "listener?" "bytes?" "record?" "vector?"

    ;; Equality
    "eq?" "equal?" "null?" "even?" "odd?" "positive?" "negative?" "zero?"
    ;; IO
    "display" "displayln" "newline"
    ;; Error
    "error" "raise" "error-object?" "error-object-message"))

(defvar esp-scheme-keywords
  `("quote" "quasiquote" "if" "when" "unless"
            "with-exception-handler" "raise" "define"
            "define-record-type" "lambda" "begin"
            "set!" "let" "let*" "letrec" "and"
            "or" "cond" "not" "else" "define-module" "import"))


(defvar esp-scheme-globals
  '("spi2" "spi3" "i2c0" "i2c1" "rfid"))

(defvar esp-scheme-font-lock-keywords
  `(;; ESP builtins
    (,(regexp-opt esp-scheme-builtins 'symbols)
     . font-lock-builtin-face)
    ;; Standard functions
    (,(regexp-opt esp-scheme-standard-functions 'symbols)
     . font-lock-function-name-face)
    ;; Keywords
    (,(regexp-opt esp-scheme-keywords 'symbols)
     . font-lock-keyword-face)
    ;; Pre-bound hardware globals
    (,(regexp-opt esp-scheme-globals 'symbols)
     . font-lock-constant-face)
    ;; Booleans
    ("#[tf]\\b"    . font-lock-constant-face)
    ;; Numeric literals — #x #b #o
    ("#x[0-9a-fA-F]+" . font-lock-number-face)
    ("#b[01]+"        . font-lock-number-face)
    ("#o[0-7]+"       . font-lock-number-face)
    ;; Bytevector prefix — must come before vector prefix
    ("#u8(.+)" . font-lock-string-face)
    ;; Vector prefix
    ("#(.+)"   . font-lock-string-face))
  "Font lock keywords for esp-scheme-mode.")


;; Indentation

(defvar esp-scheme-indent-rules
  '((dotimes                . 1)
    (forever                . 0)
    (let                    . scheme-let-indent)
    (let*                   . 1)
    (letrec                 . 1)
    (when                   . 1)
    (with-exception-handler . 0)
    (unless                 . 1)))

(defun esp-scheme-setup-indentation ()
  (pcase-dolist (`(,sym . ,n) esp-scheme-indent-rules)
    (put sym 'scheme-indent-function n)))


;; Connection

(defvar esp-scheme--last-host nil)
(defvar esp-scheme--last-port nil)

;;;###autoload
(defun esp-scheme-connect (host port)
  "Connect to an esp-scheme SWANK server at HOST:PORT.
Remembers the last connection for quick reconnect."
  (interactive
   (list
    (read-string
     (format "ESP32 host (default %s): "
             (or esp-scheme--last-host esp-scheme-default-host))
     nil nil
     (or esp-scheme--last-host esp-scheme-default-host))
    (read-number
     (format "Port (default %d): "
             (or esp-scheme--last-port esp-scheme-default-port))
     (or esp-scheme--last-port esp-scheme-default-port))))
  (setq esp-scheme--last-host host
        esp-scheme--last-port port)
  (message "Connecting to esp-scheme at %s:%d..." host port)
  (sly-connect host port))

;;;###autoload
(defun esp-scheme-reconnect ()
  "Reconnect to the last esp-scheme SWANK server."
  (interactive)
  (if (and esp-scheme--last-host esp-scheme--last-port)
      (sly-connect esp-scheme--last-host esp-scheme--last-port)
    (call-interactively #'esp-scheme-connect)))

(defun esp-scheme-reset-and-reconnect ()
  "Send esp-reset to the device then reconnect after a short delay."
  (interactive)
  (when (sly-connected-p)
    (sly-interactive-eval "(esp-reset)"))
  (message "Resetting ESP32, reconnecting in 3s...")
  (run-at-time 3 nil #'esp-scheme-reconnect))


;; Mode definition

;;;###autoload
(define-derived-mode esp-scheme-mode scheme-mode "ESP-λ"
  "Major mode for esp-scheme files targeting ESP32 microcontrollers."
  ;; (font-lock-add-keywords nil esp-scheme-font-lock-keywords 'append)
  (setq-local font-lock-defaults
              '((esp-scheme-font-lock-keywords)))
  (esp-scheme-setup-indentation)
  (when (fboundp 'sly-mode)
    (sly-mode 1))
  (setq-local comment-start ";")
  (setq-local comment-end "")
  ;; Keybindings
  (local-set-key (kbd "C-c C-c") 'sly-eval-defun)
  (local-set-key (kbd "C-c C-e") 'sly-eval-last-expression)
  (local-set-key (kbd "C-c C-r") 'sly-eval-region)
  (local-set-key (kbd "C-c C-d c") 'esp-scheme-connect)
  (local-set-key (kbd "C-c C-d C-c") 'esp-scheme-connect)
  (local-set-key (kbd "C-c C-d r") 'esp-scheme-reconnect)
  (local-set-key (kbd "C-c C-d C-r") 'esp-scheme-reconnect)
  (local-set-key (kbd "C-c C-d R") 'esp-scheme-reset-and-reconnect)
  (local-set-key (kbd "C-c C-d C-R") 'esp-scheme-reset-and-reconnect))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.escm\\'" . esp-scheme-mode))

(provide 'esp-scheme-mode)
;;; esp-scheme-mode.el ends here
