;; Set Load Path
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
(load "markdown-mode")
(require 'go-mode)
(require 'soy-mode)
(require 'dart-mode)

;; Variable Customizations
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#002451" "#ff9da4" "#d1f1a9" "#ffeead" "#bbdaff" "#ebbbff" "#bbdaff" "#ffffff"])
 '(ansi-term-color-vector [unspecified "#002451" "#ff9da4" "#d1f1a9" "#ffeead" "#bbdaff" "#ebbbff" "#bbdaff" "#ffffff"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes (quote ("8261f6e303524a9131536b51d525ea7b57d9bf253eb5359ad8f7242f90851e2c" "b63a3999093a513e2f263b2177a00cc85d945c0b98ee90d18fe2edcbee2a4299" "07b1b3a4854da0ed3a95772ed21da55a0d656ac4646596d4e80a71da5d8112b1" "5592dca15c2fb4f011ee3781eb6c44d13d4fa03d019de50ea34b13d667f2bf0a" "fa10d9e1fe9c534bb87bf1a6d8601ad42b1f11f43715a6b2929a5ecc91d4f5c9" "b12a903ec4c7ce0936dc394aa91788f91c73b33ef4b1a9d8acf94b6db7e144b4" "ca7fd88fa81ce1bdd1101dee25044967f037165af1fa29d7c2c78c4c4f069a85" "7a32ae82b734203858620bd2fcdfd8ee88b061d531abb38181b14f23cd62f563" "298561398e160a6bbb4bbd9a5d144bd76c8b63421fb13ec75edaa2f7459bb96b" "bf7ed640479049f1d74319ed004a9821072c1d9331bc1147e01d22748c18ebdf" "d677ef584c6dfc062B97901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e439d894bf9406baf73056cf7e3c913ee5c794b6adadbbb9f614aebed0fd9ce7" "cf2bb5e8046ca363183c87e8d33932f2a76a3d705b9db2721631777bbce92968" "4870e6cb6f0a70c14ee73db30b69a8a1f08d6ec9a689c366e88636fb81e8022d" default)))
 '(fci-rule-color "#eee8d5")
 '(global-linum-mode 1)
 '(gpm-mouse-mode nil)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors (quote (("#eee8d5" . 0) ("#B4C342" . 20) ("#69CABF" . 30) ("#69B7F0" . 50) ("#DEB542" . 60) ("#F2804F" . 70) ("#F771AC" . 85) ("#eee8d5" . 100))))
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((c-file-offsets (arglist-intro . +) (arglist-close . 0)) (c-file-offsets (arglist-intro . ++) (arglist-close . 0)))))
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(vc-follow-symlinks t)
 '(js-indent-level 2)
 '(indent-tabs-mode nil)
 '(css-indent-offset 2))

;; No scratch message
(setq initial-scratch-message "")

;; Initial Mode is Markdown
(setq initial-major-mode 'markdown-mode)

;; yes or no switched to y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Get back a line of code
(menu-bar-mode 0)

;; Make sure DEL key does what I want
(when window-system
  (normal-erase-is-backspace-mode 1))

;; Goto line
(global-set-key "\C-xg" 'goto-line)

;; Comment box
(global-set-key "\C-cc" 'comment-box)

;; Stop C-[ (i.e. escape) from closing buffers 
(global-unset-key "")

;; Mice
(require 'mouse) 
(xterm-mouse-mode 1)

;; Make emacs mouse mode work when emacs is open in a tmux session
(add-hook 'server-visit-hook 'xterm-mouse-mode)

;; highlight brackets
(show-paren-mode 1)

;; VCS
(require 'git)
(require 'git-blame)

;; I don't have any lost love for the <insert> key, so disable it
(global-set-key [insert] (lambda () (interactive)))
(global-set-key [insertchar] (lambda () (interactive)))

;; Prevent Emacs from making backup files
(setq make-backup-files nil) 

;; f5 toggles whitespace mode
(global-set-key [f5] 'whitespace-mode)

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

;; Transpose windows
(require 'transpose-frame)
(global-set-key (kbd "\C-x SPC") 'transpose-frame)
(global-set-key (kbd "\C-x \C-@") 'flop-frame) ;; for whatever reason, the second control+space gets registered as an `@`

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
  (loop for i from 1 to 5 do (previous-line))
  )
(defun five-next-lines() 
  (interactive)
  (loop for i from 1 to 5 do (next-line))
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
(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))

;;;;;;;;;;;;;;;
;; to delete ;;
;;;;;;;;;;;;;;;

;;========LOOK AT THE PRETTY COLORS!=========
;; (defun terminal-init-screen ()
;;   "Terminal initialization function for screen."
;;   ;; Use the xterm color initialization code.
;;   (load "term/xterm")
;;   (xterm-register-default-colors)
;;   (tty-set-up-initial-frame-faces))

;; handle tmux's xterm-keys
;; put the following line in your ~/.tmux.conf:
;;   setw -g xterm-keys on
;; (define-key key-translation-map (kbd "M-[ 1 ; 2 A") (kbd "S-<up>"))
;; (define-key key-translation-map (kbd "M-[ 1 ; 2 B") (kbd "S-<down>"))
;; (define-key key-translation-map (kbd "M-[ 1 ; 2 C") (kbd "S-<right>"))
;; (define-key key-translation-map (kbd "M-[ 1 ; 2 D") (kbd "S-<left>"))
