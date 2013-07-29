(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  )

(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))

;; (set-fontset-font
;;     (frame-parameter nil 'font)
;;     'han
;;     (font-spec :family "Hiragino Sans GB" ))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode '(0 . 0))

(setq make-backup-files nil)
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-c l") 'set-mark-command)
(global-set-key (kbd "C-c C-v") 'copy-region-as-kill)

(global-auto-revert-mode t)
(setq column-number-mode t)

(setq line-number-mode t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)
(global-set-key [C-tab] "\C-q\t")
(setq message-log-max nil)
(setq ring-bell-function 'ignore)

(load "desktop") 
(desktop-save-mode)
(show-paren-mode)
(require 'linum)

(require 'ido)
(ido-mode t)

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)


(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullscreen))
  (if (frame-parameter nil 'fullscreen)
      (display-time-mode 1))
  (if (not (frame-parameter nil 'fullscreen))
      (display-time-mode 0))
  )

(toggle-fullscreen)

(load-file "~/.emacs.d/anything.el")
;(load-file "~/.emacs.d/anything-match-plugin.el")
(require 'anything-complete)
;; Bind C-o to complete shell history
(anything-complete-shell-history-setup-key "\C-o")

(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-x f") 'ido-find-file)
(global-set-key (kbd "C-x m") 'anything-execute-extended-command)
;;(global-set-key (kbd "C-x m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'anything-execute-extended-command)
(global-set-key (kbd "C-c m")   'anything-execute-extended-command)


(setq auto-mode-alist
      (append '(("\\.cpp$" . c++-mode)
		("\\.hpp$" . c++-mode)
		("\\.h$" . c++-mode)
		("\\.hh$" . c++-mode)
		("\\.lsp$" . lisp-mode)
		("\\.lisp$" . lisp-mode)
		("\\.py$" . python-mode)
		("\\.scm$" . scheme-mode)
		("\\.pl$" . perl-mode)
		("\\.hs$". haskell-mode)
		("\\.vhd$" . text-mode)
		("\\.rb$"  . ruby-mode)
		("\\.rake$" . ruby-mode)
		("Gemfile" . ruby-mode)
		("\\.scss$" . css-mode)
		("Rakefile" . ruby-mode)
		("rakefile" . ruby-mode)
		) auto-mode-alist))


(if (functionp 'global-hi-lock-mode)
    (global-hi-lock-mode 1)
  (hi-lock-mode 1))

(require 'highlight-symbol)
(require 'sgml-mode)
(setq highlight-symbol-idle-delay 0.5)
(highlight-symbol-mode 1)
(defun highlight-symbol-mode-on ()
  "Turn on function `highlight-symbol-mode'."
  (highlight-symbol-mode 1))
(defun highlight-symbol-mode-off ()
  "Turn off function `highlight-symbol-mode'."
  (highlight-symbol-mode -1))
(dolist (hook '(emacs-lisp-mode-hook lisp-interaction-mode-hook java-mode-hook
	c++-mode-hook  c-mode-common-hook text-mode-hook ruby-mode-hook html-mode-hook))
  (add-hook hook 'highlight-symbol-mode-on))

(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-prev)
(global-set-key (kbd "C-c r"  ) 'highlight-symbol-query-replace)
(global-set-key (kbd "C-c M-N") 'highlight-symbol-next-in-defun)
(global-set-key (kbd "C-c M-P") 'highlight-symbol-prev-in-defun)

;(define-globalized-minor-mode global-highlight-symbol-mode highlight-symbol-mode highlight-symbol-mode)
;(global-highlight-symbol-mode 1)
(custom-set-faces
 '(highlight-symbol-face ((((class color) (background dark)) (:background "MediumPurple1")))))


(require 'highlight-current-line)
(highlight-current-line-minor-mode)
(highlight-current-line-on t)
;; To customize the background color
(set-face-background 'highlight-current-line-face "gray30")

(require 'highlight-parentheses)
(defun turn-on-highlight-parentheses-mode ()
(highlight-parentheses-mode t))
(define-global-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  turn-on-highlight-parentheses-mode)
(global-highlight-parentheses-mode)
(setq hl-paren-background-colors '("green"))
(global-set-key (kbd "C-'") 'highlight-symbol-next)

;;(load-file "~/.emacs.d/viewer.el")

(defun open-eshell-now ()
  "Open eshell"
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
  (other-window 1)
  (shell))
(global-set-key (kbd "C-c e") 'open-eshell-now)

(defun go-to-other-window-and-close-this-one()
  "Go to other window and close current window"
  (interactive)
  (kill-buffer (current-buffer))
  (next-buffer)
  (delete-other-windows))

(global-set-key (kbd "C-c k") 'go-to-other-window-and-close-this-one)

(require 'tabbar)
;;(tabbar-mode)
(global-set-key (kbd "C-c b") 'tabbar-backward-group)
(global-set-key (kbd "C-c f") 'tabbar-forward-group)
(global-set-key (kbd "C-x [") 'tabbar-backward)
(global-set-key (kbd "C-x ]") 'tabbar-forward)
(global-set-key (kbd "C-c ]") 'previous-buffer)
(global-set-key (kbd "C-,") 'scroll-up-command)
(global-set-key (kbd "C-.") 'scroll-down-command)
(global-set-key (kbd "C-c [") 'next-buffer)

(setq tabbar-buffer-groups-function
    (lambda (b) (list "All Buffers")))

(setq tabbar-buffer-list-function
    (lambda ()
        (remove-if
          (lambda(buffer)
             (find (aref (buffer-name buffer) 0) " *"))
          (buffer-list))))

(set-face-attribute 'tabbar-default-face nil  
                    :family "DejaVu Sans Mono"  
                    :background "#3f3f3f"  
                    :foreground "gray30"  
                    :height 0.1
                    )

(set-face-attribute 'tabbar-button-face nil  
                    :inherit 'tabbar-default  
                    :box '(:line-width 1 :color "yellow70")  
                    )
  
(set-face-attribute 'tabbar-selected-face nil  
                    :inherit 'tabbar-default  
                    :foreground "DarkGreen"  
                    :background "LightGoldenrod"  
                    :box '(:line-width 1 :color "DarkGoldenrod")  
                    :overline "rosy brown"  
                    :underline "rosy brown"  
                    :weight 'bold  
                    )
(set-face-attribute 'tabbar-unselected-face nil  
                    :inherit 'tabbar-default  
                    :box '(:line-width 1 :color "#00B2BF")  
                    )


(require 'undo-tree)
(global-undo-tree-mode)


(defun get-continue-string ()
  (interactive)
  (skip-chars-backward "^ \t\n\"\'\(\)\<\>\!\&\;\\\[\]")
  (setq low (point))
  (skip-chars-forward "^ \t\n\"\'\(\)\<\>\!\&\;\\\[\]")
  (setq high (point))
  (copy-region-as-kill low high)
  (message (buffer-substring low high)))

(global-set-key (kbd "C-x y") 'get-continue-string)

;; (defface font-lock-function-call-face
;;   '((t (:foreground "cyan")))
;;   "Font Lock mode face used to highlight function calls."
;;   :group 'font-lock-highlighting-faces)

;; (defvar font-lock-function-call-face 'font-lock-function-call-face)
;; (add-hook 'c-mode-hook
;;           (lambda ()
;;             (font-lock-add-keywords
;;              nil
;;              '(("\\<\\(\\sw+\\) ?(" 1 font-lock-function-call-face)) t)))

(flymake-mode)

(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict/")
(setq ac-auto-start 3)
(global-auto-complete-mode t)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

(add-to-list 'load-path "~/.emacs.d/auto-complete-clang")
(require 'auto-complete-clang)

(define-key ac-mode-map  [(control tab)] 'auto-complete)
(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)

(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (split-string
               "
/usr/include/c++/4.6
/usr/include/c++/4.6/x86_64-linux-gnu/.
/usr/include/c++/4.6/backward
/usr/lib/gcc/x86_64-linux-gnu/4.6/include
/usr/local/include
/usr/lib/gcc/x86_64-linux-gnu/4.6/include-fixed
/usr/include/x86_64-linux-gnu
/usr/include
/home/yukang/2bugscope/hbt/src
/home/yukang/2bugscope/front/src
/home/yukang/2bugscope/core/src
/home/yukang/2bugscope/trace/src
/home/yukang/2bugscope/printer/src
/home/yukang/2bugscope/formal/src
/home/yukang/2bugscope/expression/src
/home/yukang/2bugscope/zlib/src
/home/yukang/2bugscope/multicore/src
"
               )))
;; ac-source-gtags
(my-ac-config)

(defun with-line-copy-file-name()
  (interactive)
  (let ((file (buffer-name))
        (line (line-number-at-pos)))
    (setq string (format "%s:%s" file line))
    (kill-new string)
    (message "String '%s' copied." string)))

(defun file-name-of-this-buffer ()
  (interactive)
  (message (buffer-file-name)))

(global-set-key [f2] 'file-name-of-this-buffer)

(defun open-this-file()
  (interactive)
  (skip-chars-backward "^ \t\n\"\'\(\)\<\>\!\&\;\\\[\]")
  (setq low (point))
  (skip-chars-forward "^ \t\n\"\'\(\)\<\>\!\&\;\\\[\]")
  (setq high (point))
  (setq filename (buffer-substring low high))
  (message (format "visitig file: %s" filename))
  (find-file filename))


(require 'flymake-clang-c)
(require 'flymake-clang-c++)

(require 'clang-lookup)
(defun my-c-mode-common-hook()
  (setq tab-width 4 indent-tabs-mode nil)
  (hs-minor-mode t)
  ;; hungry-delete and auto-newline
  ;;(c-toggle-auto-hungry-state 1)
  (c-set-style "stroustrup")	
  (setq c-basic-offset 4) 
  ;(flymake-clang-c-load)
  (define-key c-mode-base-map [(control \`)] 'hs-toggle-hiding)
  (define-key c-mode-base-map [(return)] 'newline-and-indent)
  (define-key c-mode-base-map [(f7)] 'compile)
  (define-key c-mode-base-map [(f8)] 'ff-get-other-file)
  (define-key c-mode-base-map [(meta \`)] 'c-indent-command)
  (setq c-macro-shrink-window-flag t)
  (setq c-macro-preprocessor "cpp")
  (setq c-macro-cppflags " ")
  (setq c-macro-prompt-flag t)
  (setq abbrev-mode t)
  (hs-minor-mode)
  ;;(setq ac-auto-start nil)
  (setq ac-expand-on-auto-complete nil)
  (setq ac-quick-help-delay 0.5)
  (define-key c-mode-base-map (kbd "M-/") 'ac-complete-clang)
  (define-key c-mode-base-map (kbd "C-j") 'ac-complete-clang)
  )

(defface font-lock-function-call-face
  '((t (:foreground "sky blue")))
  "Font Lock mode face used to highlight function calls."
  :group 'font-lock-highlighting-faces)

(defvar font-lock-function-call-face 'font-lock-function-call-face)
(add-hook 'my-c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(\\sw+\\) ?(" 1 font-lock-function-call-face)) t)))

(add-hook 'c++-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(\\sw+\\) ?(" 1 font-lock-function-call-face)) t)))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(setq c-basic-offset 4)
(setq c++-basic-offset 4)

(c-add-style "mycodingstyle"
             '((c-basic-offset . 4)
               (c-comment-only-line-offset . 0)
               (c-hanging-braces-alist . ((substatement-open before after)))
               (c-offsets-alist . ((topmost-intro        . 0)
                                   (topmost-intro-cont   . 0)
                                   (substatement         . 4)
                                   (substatement-open    . 0)
                                   (statement-case-open  . 4)
                                   (statement-cont       . 4)
                                   (access-label         . -4)
                                   (inclass              . 4)
                                   (inline-open          . 4)
                                   (innamespace          . 0)
                                   ))))


(defun my-c++-mode-hook()
  (setq c++-basic-offset 4) 
  (hs-minor-mode)
  (c-set-style "mycodingstyle")
  ;;(flymake-clang-c++-load)
  (define-key c-mode-base-map [(return)] 'newline-and-indent))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-common-hook)
(add-hook 'python-mode-hook '(lambda ()
			       (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'ruby-mode-hook '(lambda ()
			     (local-set-key (kbd "RET") 'newline-and-indent)))


(setq abbrev-mode t)
(global-set-key (kbd "C-=") 'dabbrev-expand)

;; (setq ansi-color-names-vector
;;       ["black" "red" "green" "yellow" "sky blue" "magenta" "cyan" "white"])
;; (ansi-color-for-comint-mode-on)

(add-to-list 'load-path "~/.emacs.d/themes")
(require 'zenburn-theme)
;(require 'color-theme)
;(require 'color-theme-zenburn)
;(color-theme-zenburn)
;;(require 'molokai-theme)
;;(require 'molokai-theme-kit)
;;(require 'color-theme-sons-of-obsidian)
;;(color-theme-sons-of-obsidian)
;(set-face-background 'modeline "#3f3f3f")


(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)


(setq which-func-modes t)
(which-func-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-gui-warnings-enabled nil)
 '(projectile-enable-caching nil)
 '(projectile-global-mode t)
 '(projectile-require-project-root nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(which-func ((t (:foreground "dark cyan")))))

(setq which-func-cleanup-function
      (lambda (s) (set-text-properties 0 (length s) nil s) s))

(require 'xcscope)
(add-hook 'c-mode-common-hook
	  '(lambda ()
	     (require 'xcscope)))
(add-hook 'c++-mode-common-hook
	  '(lambda() 
	     (require 'xcscope)))



(load-file "~/.emacs.d/gtags.el")
(gtags-mode)

(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
        (buffer-substring (point-min) (1- (point-max)))
      nil)))

(defun gtags-update-single(filename)
  "Update Gtags database for changes in a single file"
  (interactive)
  (start-process "update-gtags" nil "bash" "-c"
                 (concat "cd " (gtags-root-dir) " ; gtags --single-update " filename )))

(defun gtags-update-current-file()
  (interactive)
  (defvar filename)
  (setq filename (replace-regexp-in-string (gtags-root-dir) "." (buffer-file-name (current-buffer))))
  (gtags-update-single filename)
  (message "Gtags updated for %s" filename))

(defun gtags-update-hook()
  "Update GTAGS file incrementally upon saving a file"
  (when gtags-mode
    (when (gtags-root-dir)
      (gtags-update-current-file))))

(add-hook 'after-save-hook 'gtags-update-hook)

(global-set-key (kbd "C-,") 'gtags-find-tag)
(global-set-key (kbd "C-.") 'gtags-pop-stack)
(global-set-key [f9] 'gtags-find-symbol)
(global-set-key [(shift f9)] 'gtags-pop-stack)
(global-set-key (kbd "C-x j f") 'gtags-find-file)

(global-set-key (kbd "C-;") 'gtags-find-file)

(require 'xgtags)
(xgtags-mode 1)
(global-set-key (kbd "C-,") 'xgtags-find-tag)
(global-set-key (kbd "C-x j t") 'xgtags-find-tag)
(global-set-key (kbd "C-.") 'xgtags-pop-stack)
(global-set-key (kbd "C-x j b") 'xgtags-pop-stack)
(global-set-key [f9] 'xgtags-find-symbol)
(global-set-key [(shift f9)] 'xgtags-find-rtag)
(global-set-key (kbd "C-x j f") 'gtags-find-file)
(global-set-key (kbd "C-;") 'gtags-find-file)
(global-set-key (kbd "M-n") 'xgtags-select-next-tag)
(global-set-key (kbd "M-p") 'xgtags-select-prev-tag)

(autoload 'markdown-mode "markdown-mode.el"
    "Major mode for editing Markdown files" t)
(setq auto-mode-alist
    (cons '("\\.md" . markdown-mode) auto-mode-alist))

(global-set-key (kbd "s-p") 'eshell-previous-matching-input-from-input)

(require 'lua-mode)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


;; (add-to-list 'load-path "~/.emacs.d/projectile/")
;; (require 'projectile)
;; (require 'helm-projectile)
;; (projectile-global-mode)
;; (setq projectile-enable-caching nil)
;; (setq projectile-require-project-root nil)

;; (add-to-list 'load-path "~/.emacs.d/helm")
;; (require 'helm-mode)
;; (require 'helm-config)
;; (global-set-key (kbd "C-c h") 'helm-mini)
;; (global-set-key (kbd "C-;") 'xgtags-find-file)
;; (helm-mode 1)
;; (require 'helm-files)
;; (add-hook 'eshell-mode-hook
;;           #'(lambda ()
;;               (define-key eshell-mode-map 
;;                 [remap pcomplete]
;;                 'helm-esh-pcomplete)))

;; (eval-after-load "helm-regexp"
;;   '(helm-attrset 'follow 1 helm-source-moccur))

;; (defun my-helm-multi-all ()
;;   "multi-occur in all buffers backed by files."
;;   (interactive)
;;   (helm-multi-occur
;;    (delq nil
;;          (mapcar (lambda (b)
;;                    (when (buffer-file-name b) (buffer-name b)))
;;                  (buffer-list)))))

(require 'auto-indent-mode)
;(add-hook 'emacs-lisp-mode-hook 'auto-indent-minor-mode)
(add-hook 'javascript-mode-hook 'auto-indent-minor-mode)
(add-hook 'ruby-mode-hook 'auto-indent-minor-mode)

;;(load-file "~/.emacs.d/python-mode.el")
(load-file "~/.emacs.d/set-indent.el")

(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(setq-default cursor-type '(hbar . 3))

(setq default-frame-alist
      '((cursor-color . "SteelBlue1")))

(require 'dash-at-point)
(autoload 'dash-at-point "dash-at-point"
  "Search the word at point with Dash." t nil)
(global-set-key "\C-cd" 'dash-at-point)
(add-hook 'ruby-mode-hook
	  (lambda () (setq dash-at-point-docset "rails")))


(add-to-list 'load-path "~/.emacs.d/iedit")
(require 'iedit)
(global-set-key "\C-ci" 'iedit-mode)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "hj" 'undo)
(key-chord-define-global "[]" 'indent-region)

;;
;; ace jump mode major function
;; 
(require 'ace-jump-mode)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(defun hbin-remove-mm-lighter (mm)
  "Remove minor lighter from the mode line."
  (setcar (cdr (assq mm minor-mode-alist)) nil))


(require 'smart-mode-line)
(if after-init-time (sml/setup)
  (add-hook 'after-init-hook 'sml/setup))
(set-face-background 'mode-line "gray23")

(load-file "~/.emacs.d/auto/autoconfig.el")
;;;###autoload
(defun switch-source-file ()
  (interactive)
  (setq file-name (buffer-file-name))
  (cond ((string-match "\\.cpp" file-name)
         (find-file (replace-regexp-in-string "\\.cpp" "\.h" file-name)))
        ((string-match "\\.cc" file-name)
         (find-file (replace-regexp-in-string "\\.cc" "\.hh" file-name)))
        ((string-match "\\.hh" file-name)
         (find-file (replace-regexp-in-string "\\.hh" "\.cc" file-name)))
        ((string-match "\\.h" file-name)
         (find-file (replace-regexp-in-string "\\.h" "\.c" file-name)))
        ((string-match "\\.c" file-name)
         (find-file (replace-regexp-in-string "\\.c" "\.h" file-name)))
        ((string-match "\\.h" file-name)
         (find-file (replace-regexp-in-string "\\.h" "\.cpp" file-name)))))

(global-set-key [f11] 'switch-source-file)


;; (add-to-list 'load-path "~/.emacs.d/Pymacs")
;; (load-file "~/.emacs.d/Pymacs/pymacs.el")
;; (require 'pymacs)

;; ;; Initialize Pymacs
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)

;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)
;; (require 'pycomplete)
;; (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;; (autoload 'python-mode "python-mode" "Python editing mode." t)
;; (setq interpreter-mode-alist(cons '("python" . python-mode)
;;                                   interpreter-mode-alist))
;; Initialize Rope                                                                                            
;(pymacs-load "ropemacs" "rope-")
;(setq ropemacs-enable-autoimport t)

;; (package-initialize)
;; (elpy-enable)
;; (elpy-use-ipython)
;; (elpy-clean-modeline)
;; (setq python-indent-offset 4)
;;(eldoc-mode nil)
