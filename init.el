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
;(load-theme 'kaolin-mono-light t)
(load-theme 'twilight-bright t)

;; Set font
(set-frame-font "SGI Screen 10" nil t)
(defun reload ()
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

(defun open-init ()
  (find-file "~/.emacs.d/init.el"))
(defun open-conf ()
  (find-file "~/.emacs.d/config.org"))
;; Load config.org for init.el configuration
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(twilight-bright))
 '(custom-safe-themes
   '("95903acb4c3a389ba3ed155ed0d7105214ddce7ad1884d6bcec79af2d2bc8704" "06041ceb48fec8e44adae15c8e711ed34df79c07a264cb9cfa677b1eb4cc5b1c" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "2a4f55147b6c3dcdc09c29c21b06e270e372352d3be0df749c708b3f61aad33f" "3b34cb8c036acecc790f835e28503c46a96ef5b526cec99d7d97fe66000000da" "bf17e1ebf8842e3b017c2f63f4f0d645a9301ab9d37d10601f4f17c1cdcc7429" "2d99b66b00b7886d09c2c91fcff0e3495a2ec307790e12a781e2b7b91fae14dd" "ef0dc9e0f70096c3a9e365cd59fc1ef017fe5195163e64b0853210256c3987cf" "005f2c92baa0fb6748811c38de696fb1a4feff19ca0c5fc89de98a1d2d89a8b6" "ba9c91bc43996f2fa710e4b5145d9de231150103e142acdcf24adcaaf0db7a17" "06ed754b259cb54c30c658502f843937ff19f8b53597ac28577ec33bb084fa52" "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "d537a9d42c6f5349d1716ae9be9a0645cc168f7aff2a8353819d570e5d02c0b3" "3c451787cee570710bff441154a7db8b644cdbb7d270189b2724c6041a262381" "8d412c0ed46b865312d6df5c1dfd1821d349dd3cba00049cf88c4ad34403597e" "e7ba99d0f4c93b9c5ca0a3f795c155fa29361927cadb99cfce301caf96055dfd" "dc8ad8b5833ae06e373cc3d64be28e67e6c3d084ea5f0e9e77225b3badbec661" "c376e68aa20c8648d8b797cdf62e067761721e0b5d365f7957ad5a17504b1e61" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "1d78d6d05d98ad5b95205670fe6022d15dabf8d131fe087752cc55df03d88595" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "d516f1e3e5504c26b1123caa311476dc66d26d379539d12f9f4ed51f10629df3" "c5a81a42df109b02a9a68dfe0ed530080372c1a0bbcb374da77ee3a57e1be719" "6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3" "e8567ee21a39c68dbf20e40d29a0f6c1c05681935a41e206f142ab83126153ca" "e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "f00a605fb19cb258ad7e0d99c007f226f24d767d01bf31f3828ce6688cbdeb22" "9cd57dd6d61cdf4f6aef3102c4cc2cfc04f5884d4f40b2c90a866c9b6267f2b3" "5a00018936fa1df1cd9d54bee02c8a64eafac941453ab48394e2ec2c498b834a" "249e100de137f516d56bcf2e98c1e3f9e1e8a6dce50726c974fa6838fbfcec6b" "c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428" "e0e363ecc40c5c3476f74570731d0f7d993d51f6b8f1913c443bdef079bbbbb2" "11cc65061e0a5410d6489af42f1d0f0478dbd181a9660f81a692ddc5f948bf34" "a131602c676b904a5509fff82649a639061bf948a5205327e0f5d1559e04f5ed" "2ce76d65a813fae8cfee5c207f46f2a256bac69dacbb096051a7a8651aa252b0" "81248e7e8ccce9fc9640a6efb2059c2e5fb68e3a90642806650f81002bf7de1d" "91bfe1fd1a701c69c9887be81458d44d5be6be73e78984159b42057dd0ad3601" "acb636fb88d15c6dd4432e7f197600a67a48fd35b54e82ea435d7cd52620c96d" "8796655c94190b16b194757ba41e33aedbc2aa91c31232c087e34d13ac1920b4" "fc6a63def3a8bdbfe0e3f845b5ef87e1a54458b2c76a534148ee4b72019d609a" "a17123ade1598b8e3d7039e35dfe69977b8f7e1a011b0ff899aabf89f0dbb0c5" default))
 '(doc-view-continuous t)
 '(evil-undo-system 'undo-redo)
 '(package-selected-packages
   '(solo-jazz-theme moe-theme twilight-anti-bright-theme twilight-theme twilight-bright-theme github-theme srv srcery-theme clojure-mode-extra-font-locking cloud-to-butt-erc cg @ sly-repl-ansi-color sly consult-company consult-yasnippet consult-flycheck consult-org-roam eglot-box org-remark org-sword org-noter org-roam org-ref vterm kaolin-themes goggles embark-consult embark slime-company rust-mode company-shell ac-geiser parinfer-rust-mode vertico meghanada company-irony company-c-headers yasnippet-snippets yasnippet magit treemacs-icons-dired treemacs-evil treemacs undo-tree page-break-lines async ido-vertical-mode switch-window avy beacon evil swiper which-key dashboard spaceline diminish auto-package-update htmlize use-package))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "#8a8a8a"))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "#ef8ef" :foreground "#fg9fg"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "#ef8ef" :foreground "#ef8ef")))))
 
