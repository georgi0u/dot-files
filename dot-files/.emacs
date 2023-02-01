;; Set Load Path
(setq-default abbrev-mode nil)
(quietly-read-abbrev-file)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list `load-path "~/.emacs.d/lisp")
;; Byte compile everything
(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

(when (>= emacs-major-version 24)
  (add-to-list `custom-theme-load-path "~/.emacs.d/themes/"))

;; Local Emacs Stuff
(when (file-exists-p "~/.local_emacs") (load "~/.local_emacs"))

;; Third party language modes
(when (>= emacs-major-version 24)
  (load "less-css-mode"))

;; ;; Variable Customizations
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#002451" "#ff9da4" "#d1f1a9" "#ffeead" "#bbdaff" "#ebbbff" "#bbdaff" "#ffffff"])
 '(ansi-term-color-vector
   [unspecified "#002451" "#ff9da4" "#d1f1a9" "#ffeead" "#bbdaff" "#ebbbff" "#bbdaff" "#ffffff"])
 '(column-number-mode t)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(css-indent-offset 2)
 '(custom-enabled-themes '(sanityinc-tomorrow-eighties))
 '(custom-safe-themes
   '("a547c442bab67ad5f726c15696464e0250d83428a1d82c09f4300ae9cf041621" "57d7235c8b21a8b84e99813f77c20e009386f34974a62f8f7dd92892753a3355" "e2643d45c3c8fe56994fbc76523e001a49ffda245ade04f9309b1f673e114db5" "89d7305e740a66570f751a4fa0264d891fb0885ce09c6ce1076d2e118bb9314a" "0f302165235625ca5a827ac2f963c102a635f27879637d9021c04d845a32c568" "2047464bf6781156ebdac9e38a17b97bd2594b39cfeaab561afffcbbe19314e2" "aaf783d4bfae32af3e87102c456fba8a85b79f6e586f9911795ea79055dee3bf" "ec0c9d1715065a594af90e19e596e737c7b2cdaa18eb1b71baf7ef696adbefb0" "5c5de678730ceb4e05794431dd65f30ffe9f1ed6c016fa766cdf909ba03e4df4" "b6f06081b007b57be61b82fb53f27315e2cf38fa690be50d6d63d2b62a408636" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" "be5b03913a1aaa3709d731e1fcfd4f162db6ca512df9196c8d4693538fa50b86" "2cd58e6f91afa05a56d8b22c2a7aed3c1193ed4e36a1c703a1a3679c40b45f7f" "99bc178c2856c32505c514ac04bf25022eaa02e2fddc5e7cdb40271bc708de39" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "8261f6e303524a9131536b51d525ea7b57d9bf253eb5359ad8f7242f90851e2c" "b63a3999093a513e2f263b2177a00cc85d945c0b98ee90d18fe2edcbee2a4299" "07b1b3a4854da0ed3a95772ed21da55a0d656ac4646596d4e80a71da5d8112b1" "5592dca15c2fb4f011ee3781eb6c44d13d4fa03d019de50ea34b13d667f2bf0a" "fa10d9e1fe9c534bb87bf1a6d8601ad42b1f11f43715a6b2929a5ecc91d4f5c9" "b12a903ec4c7ce0936dc394aa91788f91c73b33ef4b1a9d8acf94b6db7e144b4" "ca7fd88fa81ce1bdd1101dee25044967f037165af1fa29d7c2c78c4c4f069a85" "7a32ae82b734203858620bd2fcdfd8ee88b061d531abb38181b14f23cd62f563" "298561398e160a6bbb4bbd9a5d144bd76c8b63421fb13ec75edaa2f7459bb96b" "bf7ed640479049f1d74319ed004a9821072c1d9331bc1147e01d22748c18ebdf" "d677ef584c6dfc062B97901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e439d894bf9406baf73056cf7e3c913ee5c794b6adadbbb9f614aebed0fd9ce7" "cf2bb5e8046ca363183c87e8d33932f2a76a3d705b9db2721631777bbce92968" "4870e6cb6f0a70c14ee73db30b69a8a1f08d6ec9a689c366e88636fb81e8022d" default))
 '(fci-rule-color "#eee8d5")
 '(global-linum-mode nil)
 '(gpm-mouse-mode nil)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-tail-colors
   '(("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100)))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(js2-bounce-indent-p t)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(package-selected-packages
   '(color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow solarized-theme tide))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(safe-local-variable-values
   '((c-file-offsets
      (arglist-intro . +)
      (arglist-close . 0))
     (c-file-offsets
      (arglist-intro . ++)
      (arglist-close . 0))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(vc-follow-symlinks t))

;; No scratch message
(setq initial-scratch-message "")

;; Initial Mode is Markdown
(setq initial-major-mode 'text-mode)

;; yes or no switched to y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Get back a line of code
(menu-bar-mode 0)

;; Make sure DEL key does what I want
(when window-system
  (normal-erase-is-backspace-mode 1))

;; Goto line
(global-set-key "\C-xg" 'goto-line)

;; Comments
(global-set-key "\C-c\C-c" 'comment-or-uncomment-region)

;; If I open the buffer menu, bring the cursor
(global-set-key "\C-xb" 'buffer-menu-other-window)
(global-set-key "\C-x\C-b" 'buffer-menu-other-window)

;; Stop C-[ (i.e. escape) from closing buffers 
(global-unset-key "")

;; Mice
(require 'mouse) 
(xterm-mouse-mode 1)

;; Make emacs mouse mode work when emacs is open in a tmux session
(add-hook 'server-visit-hook 'xterm-mouse-mode)

;; highlight brackets
(show-paren-mode 1)

;; I don't have any lost love for the <insert> key, so disable it
(global-set-key [insert] (lambda () (interactive)))
(global-set-key [insertchar] (lambda () (interactive)))

;; Prevent Emacs from making backup files
(setq make-backup-files nil) 

;; f5 toggles whitespace mode
;; (global-set-key [f5] 'whitespace-mode)

;; Fuck you tutorial, etc.
(global-set-key "\C-ht" nil)
(global-set-key "\C-hT" nil)
(global-set-key "\C-h\C-t" nil)

;; Disable upcase region
(put 'upcase-region 'disabled nil)

;; Make buffer names unique
(setq uniquify-buffer-name-style 'forward)
(require 'uniquify)

;; ediff
(setq ediff-split-window-function 'split-window-horizontally) 
(setq ediff-merge-split-window-function 'split-window-horizontally)

;; Remove annoying message that prevents you from killing buffers when in emacsclient
(defun server-remove-kill-buffer-hook () 
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook 'server-remove-kill-buffer-hook)

;; Navigation stuff
(defun select-next-window ()
  "Switch to the next window"
  (interactive)
  (select-window (next-window)))
(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window)))
(defun five-previous-lines() 
  (interactive)
  (cl-loop for i from 1 to 5 do (previous-line))
  )
(defun five-next-lines() 
  (interactive)
  (cl-loop for i from 1 to 5 do (next-line))
  )
(when (>= emacs-major-version 24)
  (define-key input-decode-map "\e\eOA" [(meta up)])
  (define-key input-decode-map "\e\eOB" [(meta down)])
  (define-key input-decode-map "\e\eOC" [(meta right)])
  (define-key input-decode-map "\e\eOD" [(meta left)])
  (global-set-key (kbd "C-M-n") 'five-next-lines)
  (global-set-key (kbd "C-M-p") 'five-previous-lines)
  )

;; Line number stuff
(eval-after-load 'linum
  '(progn
     (defface adams-linum
       `((t :inherit 'linum
            :background "#333"))
       nil 
       :group 'linum)
     (defface darker
       `((t :inherit 'linum
            :background "#333"))
       nil 
       :group 'linum)
     (defface linum-leading-zero
       `((t :inherit 'adams-linum
            :foreground  "#333"))
       "Face for displaying leading zeroes for line numbers in display margin."
       :group 'linum)
     (defun linum-format-func (line)
       (let ((w (length
                 (number-to-string (count-lines (point-min) (point-max))))))
         (concat (concat
                  (propertize (make-string (- w (length (number-to-string line))) ?0)
                              'face 'linum-leading-zero)
                  (propertize (number-to-string line) 'face 'adams-linum)
                  ) (propertize " " 'face 'darker))))
     (setq linum-format 'linum-format-func)))
(global-set-key [f6] 'global-linum-mode)

;; Whitespace stuff
(require 'whitespace)
(setq whitespace-style '(face empty tabs trailing indentation space-before-tab space-after-tab lines-tail))
(defun eighty-char-limit-hook ()
  (setq whitespace-line-column 80))
(defun hundred-char-limit-hook ()
  (setq whitespace-line-column 100))
(add-hook 'python-mode-hook 'whitespace-mode)
(add-hook 'python-mode-hook 'eighty-char-limit-hook)
(add-hook 'c-mode-hook 'whitespace-mode)
(add-hook 'c-mode-hook 'eighty-char-limit-hook)
(add-hook 'javascript-mode-hook 'whitespace-mode)
(add-hook 'javascript-mode-hook 'eighty-char-limit-hook)
(add-hook 'js2-mode-hook 'whitespace-mode)
(add-hook 'js2-mode-hook 'eighty-char-limit-hook)

(add-hook 'c++-mode-hook 'whitespace-mode)
(add-hook 'c++-mode-hook 'eighty-char-limit-hook)
(add-hook 'java-mode-hook 'whitespace-mode)
(add-hook 'java-mode-hook 'hundred-char-limit-hook)

;; Flyspell
(add-hook 'c-mode-hook 'flyspell-prog-mode)
(add-hook 'c++-mode-hook 'flyspell-prog-mode)
(add-hook 'perl-mode-hook 'flyspell-prog-mode)
(add-hook 'python-mode-hook 'flyspell-prog-mode)
(add-hook 'php-mode-hook 'flyspell-prog-mode)
(add-hook 'javascript-mode-hook 'flyspell-prog-mode)
(add-hook 'html-mode-hook 'flyspell-prog-mode)
(add-hook 'sh-mode-hook 'flyspell-prog-mode)
(add-hook 'css-mode-hook 'flyspell-prog-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; auto mode list
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.local_bashrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.local_zshrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\README\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.gss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'handlebars-mode)
