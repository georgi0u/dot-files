;; pub.el
;;
;; Emacs major modes for editing Pubfiles and Pintfiles
;; Author: Beckett Madden-Woods
;; $Id$

(defvar pub-mode-map
  (let ((pub-mode-map (make-sparse-keymap)))
    (define-key pub-mode-map "{" 'pub-electric-terminator)
    (define-key pub-mode-map "}" 'pub-electric-terminator)
    ;; (define-key pub-mode-map "\C-<your-key>" '<your-function>)
    pub-mode-map)
  "Keymap for Pub major mode")

(defvar pint-mode-map
  (let ((pint-mode-map (make-sparse-keymap)))
    (define-key pint-mode-map "{" 'pub-electric-terminator)
    (define-key pint-mode-map "}" 'pub-electric-terminator)
    ;; (define-key pint-mode-map "\C-<your-key>" '<your-function>)
    pint-mode-map)
  "Keymap for Pint major mode")

(defvar pub-mode-hook nil)
(defvar pint-mode-hook nil)

;; autoload configuration
(add-to-list 'auto-mode-alist '("Pubfile\\'" . pub-mode))
(add-to-list 'auto-mode-alist '("Pintfile\\'" . pint-mode))

(defconst pub-font-lock-keywords
  (list
   '("\\<\\(Binary\\|Crontab\\|De\\(fine\\|scription\\)\\|Files\\|M\\(apFiles\\|odules\\)\\|Name\\|Options\\|PostInstall\\|S\\(haredLibrary\\|ources\\|taticLibrary\\|ystemLibs\\)\\|Test\\|V\\(ariables\\|ersion\\)\\)\\>[ 	]*{" 1 font-lock-function-name-face)
   '("\\<export\\>" . font-lock-keyword-face)
   '("\\(\\<\\w+\\>\\)[ 	]*{" 1 font-lock-variable-name-face)
   '("<.*>" . font-lock-comment-face))
  "Default highlighting expressions for Pub mode")

(defconst pint-font-lock-keywords
  (list
   '("\\<\\(B\\(aseDir\\|inary\\)\\|Crontab\\|De\\(fine\\|scription\\)\\|Exclude\\|Files\\|Group\\|Links\\|M\\(apFiles\\|odules\\)\\|Name\\|Options\\|PostInstall\\|S\\(haredLibrary\\|ources\\|taticLibrary\\|ystemLibs\\)\\|Test\\|User\\|V\\(ariables\\|ersion\\)\\)\\>[ 	]*{" 1 font-lock-function-name-face)
   '("\\<machine\\>" . font-lock-keyword-face)
   '("\\(\\<\\w+\\>\\)[ 	]*{" 1 font-lock-variable-name-face)
   '("<.*>" . font-lock-comment-face))
  "Default highlighting expressions for Pint mode")

(defun pub-indent-line ()
  "Indent current Pubfile/Pintfile line"
  (interactive)
  (let (icol				; final column indentation
	(count 0)			; total (+/-) number of indents
	(cpos (- (point-max) (point))))	; save (point)
    (beginning-of-line)
    (if (bobp)
	(indent-line-to 0)
      (progn
	(save-excursion
	  (forward-line -1)
	  ; scan upwards until we find an interesting line
	  (while (and (not (looking-at "[^#}{\n]*[}{][^\n]*\n"))
		      (not (bobp)))
	    (forward-line -1))
	  (setq icol (current-indentation))
	  (while (not (eolp))
	    (if (= (char-after) ?#) ; ignore comment lines
		(end-of-line)
	      (if (= (char-after) ?})
		  (progn
		    (while (not (or (eolp)
				    (= (char-after) ?{)))
		      (forward-char))
		    (if (not (eolp))
			(progn
			  (setq count (1+ count))
			  (forward-char))))
		(if (= (char-after) ?{)
		    (progn
		      (while (not (or (eolp)
				      (= (char-after) ?})))
			(forward-char))
		      (if (eolp)
			  (setq count (1+ count))
			(forward-char)))
		  (forward-char))))))
	; we have to check the current line too for modifiers
	(save-excursion
	  (while (not (eolp))
	    (if (or (= (char-after) ?#)
		    (= (char-after) ?{))
		(end-of-line)
	      (if (= (char-after) ?})
		  (progn
		    (setq count (1- count))
		    (end-of-line) t)
		(forward-char)))))
	(setq count (* count tab-width))
	(setq icol (+ icol count))
	(if (< icol 0) (setq icol 0))	; sanity check
	(indent-line-to icol)
	; restore cursor if need be
	(if (> (- (point-max) cpos) (point))
	    (goto-char (- (point-max) cpos)))))))

(defun pub-electric-terminator (arg)
  "Insert character and adjust indentation (if eolp and not in a comment/quote)."
  (interactive "p")			; auto-convert arg to number
  (let ((cpos (point)))
    (and (< arg 2)
	 (eolp)
	 (save-excursion
	   (beginning-of-line)
	   (and (not
		 (and comment-start-skip
		      (re-search-forward comment-start-skip cpos t)))))
	 (progn
	   (insert-char last-command-char 1)
	   (pub-indent-line)
	   (delete-char -1))))
  (self-insert-command arg)) ; handle repeat-style syntax w/o auto-indent

(defvar pub-mode-syntax-table
  (let ((pub-mode-syntax-table (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?_ "w" pub-mode-syntax-table)
    (modify-syntax-entry ?\n ">" pub-mode-syntax-table)
    (modify-syntax-entry ?# "<" pub-mode-syntax-table)
    pub-mode-syntax-table)
  "Syntax table for Pub/Pint modes")

(defun pub-mode ()
  "Major mode for editing Pubfiles"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table pub-mode-syntax-table)
  (use-local-map pub-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(pub-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'pub-indent-line)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "\\(^\\|\\s-\\);?#+ *")
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (setq major-mode 'pub-mode)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq comment-column 40)
  (setq mode-name "Pubfile")
  (run-hooks 'pub-mode-hook))

(defun pint-mode ()
  "Major mode for editing Pintfiles"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table pub-mode-syntax-table)
  (use-local-map pint-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(pint-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'pub-indent-line)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "\\(^\\|\\s-\\);?#+ *")
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (setq major-mode 'pint-mode)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq comment-column 40)
  (setq mode-name "Pintfile")
  (run-hooks 'pint-mode-hook))

(provide 'pub-mode)
(provide 'pint-mode)
