(deftheme sanityinc-tomorrow-eighties
  "Created 2021-07-07.")

(custom-theme-set-variables
 'sanityinc-tomorrow-eighties
 '(ansi-color-names-vector ["#cccccc" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#66cccc" "#2d2d2d"])
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold]))

(custom-theme-set-faces
 'sanityinc-tomorrow-eighties
 '(bold ((((class color) (min-colors 89)) (:weight bold))))
 '(bold-italic ((t nil)))
 '(underline ((((class color) (min-colors 89)) (:underline t))))
 '(italic ((t nil)))
 '(font-lock-builtin-face ((((class color) (min-colors 89)) (:foreground "#cc99cc"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#999999"))))
 '(font-lock-comment-face ((t (:foreground "#999999"))))
 '(font-lock-constant-face ((((class color) (min-colors 89)) (:foreground "#6699cc"))))
 '(font-lock-doc-face ((((class color) (min-colors 89)) (:foreground "#cc99cc"))))
 '(font-lock-function-name-face ((((class color) (min-colors 89)) (:foreground "#f99157"))))
 '(font-lock-keyword-face ((((class color) (min-colors 89)) (:foreground "#99cc99"))))
 '(font-lock-negation-char-face ((((class color) (min-colors 89)) (:foreground "#6699cc"))))
 '(font-lock-preprocessor-face ((((class color) (min-colors 89)) (:foreground "#cc99cc"))))
 '(font-lock-regexp-grouping-backslash ((((class color) (min-colors 89)) (:foreground "#ffcc66"))))
 '(font-lock-regexp-grouping-construct ((((class color) (min-colors 89)) (:foreground "#cc99cc"))))
 '(font-lock-string-face ((((class color) (min-colors 89)) (:foreground "#66cccc"))))
 '(font-lock-type-face ((((class color) (min-colors 89)) (:foreground "#6699cc"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 89)) (:foreground "#ffcc66"))))
 '(font-lock-warning-face ((((class color) (min-colors 89)) (:weight bold :foreground "#f2777a"))))
 '(shadow ((((class color) (min-colors 89)) (:foreground "#999999"))))
 '(success ((((class color) (min-colors 89)) (:foreground "#99cc99"))))
 '(error ((((class color) (min-colors 89)) (:foreground "#f2777a"))))
 '(warning ((((class color) (min-colors 89)) (:foreground "#f99157"))))
 '(outline-4 ((((class color) (min-colors 89)) (:slant normal :foreground "#999999"))))
 '(match ((((class color) (min-colors 89)) (:foreground "#6699cc" :background "#2d2d2d" :inverse-video t))))
 '(isearch ((((class color) (min-colors 89)) (:foreground "#ffcc66" :background "#2d2d2d" :inverse-video t))))
 '(isearch-fail ((((class color) (min-colors 89)) (:background "#2d2d2d" :inherit font-lock-warning-face :inverse-video t))))
 '(ido-subdir ((((class color) (min-colors 89)) (:foreground "#cc99cc"))))
 '(ido-first-match ((((class color) (min-colors 89)) (:foreground "#f99157"))))
 '(ido-only-match ((((class color) (min-colors 89)) (:foreground "#99cc99"))))
 '(ido-indicator ((((class color) (min-colors 89)) (:foreground "#f2777a" :background "#2d2d2d"))))
 '(ido-virtual ((((class color) (min-colors 89)) (:foreground "#999999"))))
 '(cursor ((((class color) (min-colors 89)) (:background "#f2777a"))))
 '(fringe ((((class color) (min-colors 89)) (:background "#393939"))))
 '(linum ((((class color) (min-colors 89)) (:background "#393939"))))
 '(border ((((class color) (min-colors 89)) (:background "#393939"))))
 '(highlight ((((class color) (min-colors 89)) (:inverse-video nil :background "#393939"))))
 '(mode-line ((((class color) (min-colors 89)) (:foreground nil :background "#393939" :box (:line-width 1 :color "#cccccc")))))
 '(mode-line-buffer-id ((((class color) (min-colors 89)) (:foreground "#cc99cc" :background nil))))
 '(mode-line-inactive ((((class color) (min-colors 89)) (:inherit mode-line :foreground "#999999" :background "#393939" :weight normal :box (:line-width 1 :color "#cccccc")))))
 '(mode-line-emphasis ((((class color) (min-colors 89)) (:foreground "#cccccc" :slant italic))))
 '(mode-line-highlight ((((class color) (min-colors 89)) (:foreground "#cc99cc" :box nil :weight bold))))
 '(minibuffer-prompt ((((class color) (min-colors 89)) (:foreground "#6699cc"))))
 '(region ((((class color) (min-colors 89)) (:background "#515151"))))
 '(secondary-selection ((((class color) (min-colors 89)) (:background "#393939"))))
 '(header-line ((((class color) (min-colors 89)) (:inherit mode-line :foreground "#cc99cc" :background nil))))
 '(trailing-whitespace ((((class color) (min-colors 89)) (:foreground "#f2777a" :inverse-video t :underline nil))))
 '(whitespace-trailing ((((class color) (min-colors 89)) (:foreground "#f2777a" :inverse-video t :underline nil))))
 '(whitespace-space-after-tab ((((class color) (min-colors 89)) (:foreground "#f2777a" :inverse-video t :underline nil))))
 '(whitespace-space-before-tab ((((class color) (min-colors 89)) (:foreground "#f2777a" :inverse-video t :underline nil))))
 '(whitespace-empty ((((class color) (min-colors 89)) (:foreground "#f2777a" :inverse-video t :underline nil))))
 '(whitespace-line ((((class color) (min-colors 89)) (:background nil :foreground "#f2777a"))))
 '(whitespace-indentation ((((class color) (min-colors 89)) (:background nil :foreground "#66cccc"))))
 '(whitespace-space ((((class color) (min-colors 89)) (:background nil :foreground "#515151"))))
 '(whitespace-newline ((((class color) (min-colors 89)) (:background nil :foreground "#515151"))))
 '(whitespace-tab ((((class color) (min-colors 89)) (:background nil :foreground "#515151"))))
 '(whitespace-hspace ((((class color) (min-colors 89)) (:background nil :foreground "#515151"))))
 '(show-paren-match ((((class color) (min-colors 89)) (:background nil :foreground nil :inverse-video t))))
 '(show-paren-mismatch ((((class color) (min-colors 89)) (:background "#cc99cc" :foreground "#2d2d2d"))))
 '(ediff-even-diff-A ((((class color) (min-colors 89)) (:foreground nil :background nil :inverse-video t))))
 '(ediff-even-diff-B ((((class color) (min-colors 89)) (:foreground nil :background nil :inverse-video t))))
 '(ediff-odd-diff-A ((((class color) (min-colors 89)) (:foreground "#999999" :background nil :inverse-video t))))
 '(ediff-odd-diff-B ((((class color) (min-colors 89)) (:foreground "#999999" :background nil :inverse-video t))))
 '(eldoc-highlight-function-argument ((((class color) (min-colors 89)) (:foreground "#99cc99" :weight bold))))
 '(link ((((class color) (min-colors 89)) (:foreground nil :underline t))))
 '(widget-button ((((class color) (min-colors 89)) (:underline t))))
 '(widget-field ((((class color) (min-colors 89)) (:background "#393939" :box (:line-width 1 :color "#cccccc")))))
 '(compilation-column-number ((((class color) (min-colors 89)) (:foreground "#ffcc66"))))
 '(compilation-line-number ((((class color) (min-colors 89)) (:foreground "#ffcc66"))))
 '(compilation-mode-line-exit ((((class color) (min-colors 89)) (:foreground "#99cc99"))))
 '(compilation-mode-line-fail ((((class color) (min-colors 89)) (:foreground "#f2777a"))))
 '(compilation-mode-line-run ((((class color) (min-colors 89)) (:foreground "#6699cc"))))
 '(js2-warning ((((class color) (min-colors 89)) (:underline "#f99157"))))
 '(js2-error ((((class color) (min-colors 89)) (:foreground nil :underline "#f2777a"))))
 '(js2-external-variable ((((class color) (min-colors 89)) (:foreground "#cc99cc"))))
 '(js2-function-param ((((class color) (min-colors 89)) (:foreground "#6699cc"))))
 '(js2-instance-member ((((class color) (min-colors 89)) (:foreground "#6699cc"))))
 '(js2-private-function-call ((((class color) (min-colors 89)) (:foreground "#f2777a"))))
 '(outline-1 ((((class color) (min-colors 89)) (:inherit nil :foreground "#6699cc"))))
 '(outline-2 ((((class color) (min-colors 89)) (:inherit nil :foreground "#ffcc66"))))
 '(outline-3 ((((class color) (min-colors 89)) (:inherit nil :foreground "#cc99cc"))))
 '(outline-4 ((((class color) (min-colors 89)) (:inherit nil :foreground "#66cccc"))))
 '(outline-5 ((((class color) (min-colors 89)) (:inherit nil :foreground "#f99157"))))
 '(outline-6 ((((class color) (min-colors 89)) (:inherit nil :foreground "#6699cc"))))
 '(outline-7 ((((class color) (min-colors 89)) (:inherit nil :foreground "#ffcc66"))))
 '(outline-8 ((((class color) (min-colors 89)) (:inherit nil :foreground "#cc99cc"))))
 '(gnus-summary-normal-unread ((((class color) (min-colors 89)) (:foreground "#6699cc" :weight normal))))
 '(gnus-summary-normal-read ((((class color) (min-colors 89)) (:foreground "#cccccc" :weight normal))))
 '(gnus-summary-normal-ancient ((((class color) (min-colors 89)) (:foreground "#66cccc" :weight normal))))
 '(gnus-summary-normal-ticked ((((class color) (min-colors 89)) (:foreground "#f99157" :weight normal))))
 '(gnus-summary-low-unread ((((class color) (min-colors 89)) (:foreground "#999999" :weight normal))))
 '(gnus-summary-low-read ((((class color) (min-colors 89)) (:foreground "#999999" :weight normal))))
 '(gnus-summary-low-ancient ((((class color) (min-colors 89)) (:foreground "#999999" :weight normal))))
 '(gnus-summary-high-unread ((((class color) (min-colors 89)) (:foreground "#ffcc66" :weight normal))))
 '(gnus-summary-high-read ((((class color) (min-colors 89)) (:foreground "#99cc99" :weight normal))))
 '(gnus-summary-high-ancient ((((class color) (min-colors 89)) (:foreground "#99cc99" :weight normal))))
 '(gnus-summary-high-ticked ((((class color) (min-colors 89)) (:foreground "#f99157" :weight normal))))
 '(gnus-summary-cancelled ((((class color) (min-colors 89)) (:foreground "#f2777a" :background nil :weight normal))))
 '(gnus-group-mail-low ((((class color) (min-colors 89)) (:foreground "#999999"))))
 '(gnus-group-mail-low-empty ((((class color) (min-colors 89)) (:foreground "#999999"))))
 '(gnus-group-mail-1 ((((class color) (min-colors 89)) (:foreground nil :weight normal :inherit outline-1))))
 '(gnus-group-mail-2 ((((class color) (min-colors 89)) (:foreground nil :weight normal :inherit outline-2))))
 '(gnus-group-mail-3 ((((class color) (min-colors 89)) (:foreground nil :weight normal :inherit outline-3))))
 '(gnus-group-mail-1-empty ((((class color) (min-colors 89)) (:inherit gnus-group-mail-1 :foreground "#999999"))))
 '(gnus-group-mail-2-empty ((((class color) (min-colors 89)) (:inherit gnus-group-mail-2 :foreground "#999999"))))
 '(gnus-group-mail-3-empty ((((class color) (min-colors 89)) (:inherit gnus-group-mail-3 :foreground "#999999"))))
 '(gnus-group-news-1 ((((class color) (min-colors 89)) (:foreground nil :weight normal :inherit outline-5))))
 '(gnus-group-news-2 ((((class color) (min-colors 89)) (:foreground nil :weight normal :inherit outline-6))))
 '(gnus-group-news-3 ((((class color) (min-colors 89)) (:foreground nil :weight normal :inherit outline-7))))
 '(gnus-group-news-4 ((((class color) (min-colors 89)) (:foreground nil :weight normal :inherit outline-8))))
 '(gnus-group-news-5 ((((class color) (min-colors 89)) (:foreground nil :weight normal :inherit outline-1))))
 '(gnus-group-news-6 ((((class color) (min-colors 89)) (:foreground nil :weight normal :inherit outline-2))))
 '(gnus-group-news-1-empty ((((class color) (min-colors 89)) (:inherit gnus-group-news-1 :foreground "#999999"))))
 '(gnus-group-news-2-empty ((((class color) (min-colors 89)) (:inherit gnus-group-news-2 :foreground "#999999"))))
 '(gnus-group-news-3-empty ((((class color) (min-colors 89)) (:inherit gnus-group-news-3 :foreground "#999999"))))
 '(gnus-group-news-4-empty ((((class color) (min-colors 89)) (:inherit gnus-group-news-4 :foreground "#999999"))))
 '(gnus-group-news-5-empty ((((class color) (min-colors 89)) (:inherit gnus-group-news-5 :foreground "#999999"))))
 '(gnus-group-news-6-empty ((((class color) (min-colors 89)) (:inherit gnus-group-news-6 :foreground "#999999"))))
 '(custom-variable-tag ((((class color) (min-colors 89)) (:foreground "#6699cc"))))
 '(custom-group-tag ((((class color) (min-colors 89)) (:foreground "#6699cc"))))
 '(custom-state ((((class color) (min-colors 89)) (:foreground "#99cc99"))))
 '(default ((t (:background "#000" :foreground "#ddd")))))

(provide-theme 'sanityinc-tomorrow-eighties)
