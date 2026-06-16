;; Set Load Path
(setq-default abbrev-mode nil)
(quietly-read-abbrev-file)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Ensure packages are installed
(unless package-archive-contents
  (package-refresh-contents))

;; Variable Customizations
(custom-set-variables
 '(package-selected-packages
   '(## color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow
	dart-mode go-mode solarized-theme typescript-mode zen-mode)))
(package-install-selected-packages)

(defun georgiou-os-light-mode-p ()
  "Return non-nil when the underlying OS is using a light theme."
  (and (eq system-type 'darwin)
       (not (string-match-p
             "Dark"
             (shell-command-to-string
              "defaults read -g AppleInterfaceStyle 2>/dev/null")))))

(defvar georgiou-current-theme nil)

(defun georgiou-apply-os-theme ()
  "Apply the Emacs theme matching the underlying OS theme."
  (let ((theme (if (georgiou-os-light-mode-p)
                   'solarized-light
                 'sanityinc-tomorrow-night)))
    (unless (eq theme georgiou-current-theme)
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme theme t)
      (setq georgiou-current-theme theme))))

(georgiou-apply-os-theme)
(add-hook 'server-visit-hook 'georgiou-apply-os-theme)

(add-to-list `load-path "~/.emacs.d/lisp")

(when (>= emacs-major-version 24)
  (add-to-list `custom-theme-load-path "~/.emacs.d/themes/"))

;; Do not prompt when opening symbolic links to version-controlled files.
(setq vc-follow-symlinks t)

;; Local Emacs Stuff
(when (file-exists-p "~/.local_emacs") (load "~/.local_emacs"))

;; Third party language modes
(when (>= emacs-major-version 24)
  (load "less-css-mode"))


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
