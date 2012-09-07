(add-to-list `load-path "~/.emacs.d/")
(add-to-list `custom-theme-load-path "~/.emacs.d/themes/")

;; Local Emacs Stuff
(when (file-exists-p "~/.local_emacs") (load "~/.local_emacs"))
(add-to-list `load-path "~/.local_emacs.d/")

;;;====Variable Customizations====
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#002451" "#ff9da4" "#d1f1a9" "#ffeead" "#bbdaff" "#ebbbff" "#bbdaff" "#ffffff"])
 '(ansi-term-color-vector [unspecified "#002451" "#ff9da4" "#d1f1a9" "#ffeead" "#bbdaff" "#ebbbff" "#bbdaff" "#ffffff"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (tomorrow-night-bright)))
 '(custom-safe-themes (quote ("e439d894bf9406baf73056cf7e3c913ee5c794b6adadbbb9f614aebed0fd9ce7" "cf2bb5e8046ca363183c87e8d33932f2a76a3d705b9db2721631777bbce92968" "4870e6cb6f0a70c14ee73db30b69a8a1f08d6ec9a689c366e88636fb81e8022d" default)))
 '(global-linum-mode t)
 '(gpm-mouse-mode nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((c-file-offsets (arglist-intro . +) (arglist-close . 0)) (c-file-offsets (arglist-intro . ++) (arglist-close . 0)))))
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(tab-width 4))

;;=====Get back a line of code!====
(menu-bar-mode 0)

 ;; Make sure DEL key does what I want
 (when window-system
   (normal-erase-is-backspace-mode 1))

 ;;=====Goto Line===================
 (global-set-key "\C-xg" 'goto-line)

 ;; Map some keys to find-function/find-variable
 (global-set-key "\C-xF" nil)
 (global-set-key "\C-xFf" 'find-function)
 (global-set-key "\C-xFv" 'find-variable)
 (global-set-key "\C-xFl" 'find-library)
 (global-set-key "\C-xFa" 'find-face-definition)

 ;; Open a location quickly
 (global-set-key "\C-ca" nil)

 ;; I don't have any lost love for the <insert> key, so disable it
 (global-set-key [insert] (lambda () (interactive)))
 (global-set-key [insertchar] (lambda () (interactive)))

 ;;========LOOK AT THE PRETTY COLORS!=========
 (defun terminal-init-screen ()
   "Terminal initialization function for screen."
   ;; Use the xterm color initialization code.
   (load "term/xterm")
   (xterm-register-default-colors)
   (tty-set-up-initial-frame-faces))

 (setq auto-mode-alist
       (append
        (list
         '("\\.md" . markdown-mode)
         '("\\.text" . markdown-mode)
         '("\\.txt" . markdown-mode)
         auto-mode-alist)))

 (add-hook 'c-mode-common-hook 'flyspell-prog-mode)
 (add-hook 'perl-mode-hook 'flyspell-prog-mode)
 (add-hook 'python-mode-hook 'flyspell-prog-mode)
 (add-hook 'php-mode-hook 'flyspell-prog-mode)
 (add-hook 'javascript-mode-hook 'flyspell-prog-mode)
 (add-hook 'html-mode-hook 'flyspell-prog-mode)
 (add-hook 'sh-mode-hook 'flyspell-prog-mode)
 (add-hook 'css-mode-hook 'flyspell-prog-mode)

 ;;=====Navigation stuff=====
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

 (define-key input-decode-map "\e\eOA" [(meta up)])
 (define-key input-decode-map "\e\eOB" [(meta down)])
 (define-key input-decode-map "\e\eOC" [(meta right)])
 (define-key input-decode-map "\e\eOD" [(meta left)])

 (global-set-key [(meta left)] 'hide-subtree)
 (global-set-key [(meta right)] 'show-subtree)
 (global-set-key [(meta up)] 'five-previous-lines)
 (global-set-key [(meta down)] 'five-next-lines)

 (global-set-key (kbd "C-M-n") 'five-next-lines)
 (global-set-key (kbd "C-M-p") 'five-previous-lines)

 ;; highlight brackets
 (show-paren-mode 1)

 ;; ========== Prevent Emacs from making backup files ==========
 (setq make-backup-files nil) 

 ;; ======== Markdown ===================
 (autoload 'markdown-mode "markdown-mode.el" 
   "Major mode for editing Markdown files" t) 


 ;; Show line numbers
 (eval-after-load 'linum
   '(progn
      (defface adams-linum
        `((t :inherit 'linum
             :background "#222"))       
        nil 
        :group 'linum)

      (defface darker
        `((t :inherit 'linum
             :background "#222"))
        nil 
        :group 'linum)
      

      (defface linum-leading-zero
        `((t :inherit 'adams-linum
             :foreground  "#222"))
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

 (put 'upcase-region 'disabled nil)


 ;; ========== ediff ==========
 (setq ediff-split-window-function 'split-window-horizontally) 
 (setq ediff-merge-split-window-function 'split-window-horizontally)
