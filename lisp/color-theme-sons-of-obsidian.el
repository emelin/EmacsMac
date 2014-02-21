
(eval-when-compile
  (require 'color-theme))

(defun color-theme-sons-of-obsidian ()
  "Famous Visual Studio Color Theme Sons of Obsidian by Troydm 2011-05"
  (interactive)
  (color-theme-install
   '(color-theme-sons-of-obsidian
      ((background-color . "#22282A")
      (background-mode . dark)
      (border-color . "#1a1a1a")
      (cursor-color . "white")
      (foreground-color . "#eeeeec")
      (mouse-color . "white"))
     (fringe ((t (:background "#293134" :bold t))))
     (mode-line ((t (:foreground "#eeeeec" :background "#575757"))))
     (region ((t (:background "#303A3B" :bold t))))
     (font-lock-builtin-face ((t (:foreground "#8482BD"))))
     (font-lock-constant-face ((t (:foreground "#ffc0ff"))))
     (font-lock-comment-face ((t (:foreground "#59747b"))))
     (font-lock-doc-string-face ((t (:foreground "#9b859d"))))
     (font-lock-function-name-face ((t (:foreground "#ff8080"))))
     (font-lock-keyword-face ((t (:foreground "#76c763"))))
     (font-lock-string-face ((t (:foreground "#e57600"))))
     (font-lock-type-face ((t (:foreground "#76c763"))))
     (font-lock-reference-face ((t (:foreground "#4F8CB1"))))
     (font-lock-variable-name-face ((t (:foreground "#F5DA81"))))
     (show-paren-match ((t (:background "#555753"))))
     (minibuffer-prompt ((t (:foreground "#a5c8ee" :bold t))))
     (font-lock-warning-face ((t (:foreground "#FFCD22" :bold t))))


     ;;;;;;;;;;from wob;;;;;;;;;;;;     
     (bold-italic ((t (:italic t :bold t :foreground "cyan"))))		
     (calendar-today-face ((t (:underline t :foreground "white"))))     
     (custom-button-face ((t (nil))))
     (custom-changed-face ((t (:background "#7ab847" :foreground "white"))))
     (custom-documentation-face ((t (nil))))
     (custom-face-tag-face ((t (:underline t))))
     (custom-group-tag-face ((t (:underline t))))
     (custom-group-tag-face-1 ((t (:underline t))))
     (custom-invalid-face ((t (:background "dark red" :foreground "white"))))
     (custom-modified-face ((t (:background "steel blue" :foreground "white"))))
     (custom-rogue-face ((t (:background "black" :foreground "pink"))))
     (custom-saved-face ((t (:underline t))))
     (custom-set-face ((t (:background "white" :foreground "steel blue"))))
     (custom-state-face ((t (nil))))
     (custom-variable-button-face ((t (:underline t :bold t))))
     (custom-variable-tag-face ((t (:underline t))))
     (highlight ((t (:background "#963A46" :foreground "white" :bold t))))
     (highline-face ((t (:background "dark slate gray" :foreground "white"))))
     (holiday-face ((t (:background "dark red" :foreground "white"))))
     (info-menu-5 ((t (:underline t))))
     (info-node ((t (:italic t :bold t :foreground "white"))))
     (info-xref ((t (:bold t :foreground "light gray"))))
     (italic ((t (:italic t :foreground "cyan"))))
     (makefile-space-face ((t (:background "hotpink" :foreground "white"))))
     (message-cited-text-face ((t (:foreground "green"))))
     (message-header-cc-face ((t (:bold t :foreground "Aquamarine"))))
     (message-header-name-face ((t (:foreground "Gold"))))
     (message-header-newsgroups-face ((t (:italic t :bold t :foreground "gold"))))
     (message-header-other-face ((t (:foreground "lightGray"))))
     (message-header-subject-face ((t (:foreground "Yellow"))))
     (message-header-to-face ((t (:bold t :foreground "green2"))))
     (message-header-xheader-face ((t (:foreground "sky blue"))))
     (message-mml-face ((t (:bold t :foreground "khaki"))))
     (message-separator-face ((t (:background "aquamarine" :foreground "black"))))
     (modeline ((t (:background "dark gray" :foreground "black"))))
     (modeline-buffer-id ((t (:background "dark gray" :foreground "black"))))
     (modeline-mousable ((t (:background "dark gray" :foreground "black"))))
     (modeline-mousable-minor-mode ((t (:background "dark gray" :foreground "black"))))
     (paren-mismatch-face ((t (:bold t :background "white" :foreground "#ff8080"))))
     (paren-no-match-face ((t (:bold t :background "white" :foreground "#ff8080"))))
     (region ((t (:background "MediumSlateBlue" :foreground "white"))))
     (secondary-selection ((t (:background "Sienna" :foreground "white"))))
     (show-paren-match-face ((t (:background "purple" :foreground "white"))))
     (show-paren-mismatch-face ((t (:bold t :background "white" :foreground "#ff8080"))))
     (speedbar-button-face ((t (nil))))
     (speedbar-directory-face ((t (nil))))
     (speedbar-file-face ((t (:bold t))))
     (speedbar-highlight-face ((t (nil))))
     (speedbar-selected-face ((t (:underline t))))
     (speedbar-tag-face ((t (nil))))
     (swbuff-current-buffer-face ((t (:bold t :foreground "#ff8080"))))
     (underline ((t (:underline t :foreground "white"))))
     (widget-button-face ((t (:bold t :foreground "coral"))))
     (widget-button-pressed-face ((t (:foreground "#ff8080"))))
     (widget-documentation-face ((t (:foreground "lime green"))))
     (widget-field-face ((t (:background "dim gray" :foreground "white"))))
     (widget-inactive-face ((t (:foreground "light gray"))))
     (widget-single-line-field-face ((t (:background "dim gray" :foreground "white"))))     
     ;;;;;;;;;;from wob;;;;;;;;;;;;     
     
     )))

(add-to-list 'color-themes
             `(color-theme-sons-of-obsidian
               "Sons of Obsidian",
	       "Dmitry Geurkov <dmitry_627@mail.ru>"))

(provide 'color-theme-sons-of-obsidian)