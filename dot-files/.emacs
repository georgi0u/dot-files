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
 '(custom-enabled-themes '(sanityinc-tomorrow-night))
 '(custom-safe-themes
   '("a547c442bab67ad5f726c15696464e0250d83428a1d82c09f4300ae9cf041621" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default))
 '(package-selected-packages
   '(zen-mode go-mode typescript-mode solarized-theme dart-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized)))

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
