(setq load-path
      (cons (expand-file-name "~/.emacs.d/lisp") load-path))

(defsubst package-desc-vers (desc)
  "Extract version from a package description vector."
  (aref desc 0))

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

(require 'package)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode '(0 . 0))
(setq make-backup-files nil)
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-c l") 'set-mark-command)
(global-set-key (kbd "C-x c") 'copy-region-as-kill)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c k") 'kill-region)
(global-set-key (kbd "C-c -") 'beginning-of-buffer)
(global-set-key (kbd "C-c =") 'end-of-buffer)
(global-set-key (kbd "C-c t") 'undo-tree-visualize)
(global-set-key (kbd "C-c j") 'ido-switch-buffer)
(global-set-key (kbd "C-c o") 'execute-extended-command)
(global-set-key (kbd "C-c g") 'goto-line)

(global-auto-revert-mode t)
(setq column-number-mode t)
(setq line-number-mode t)
(setq display-time-24hr-format t)
(display-time)

;;(setq message-log-max nil)
(setq ring-bell-function 'ignore)

(show-paren-mode)

(set-default-font "Source Code Pro-13")
(set-fontset-font "fontset-default"
		  'gb18030' ("STHeiti" . "unicode-bmp"))

(require 'linum)
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; This tab override shouldn't be necessary given ido's default
;; configuration, but minibuffer-complete otherwise dominates the
;; tab binding because of my custom tab-completion-everywhere
;; configuration.
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map [tab] 'ido-complete)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

(require 'anything-complete)
;; Bind C-o to complete shell history
(anything-complete-shell-history-setup-key "\C-o")

(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-x f") 'ido-find-file)
(global-set-key (kbd "C-x m") 'anything-execute-extended-command)
(global-set-key (kbd "C-c C-m") 'anything-execute-extended-command)
(global-set-key (kbd "C-c m")   'anything-execute-extended-command)


(require 'smex)
(smex-initialize)
(global-set-key (kbd "C-j") 'smex)
(global-set-key (kbd "C-l") 'smex)

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
		("\\.go$" . go-mode)
		("\\.gohtml$" . html-mode)
		("\\.thrift$" . trhift-mode)
		("\\.js$" . js2-mode)
		) auto-mode-alist))


(if (functionp 'global-hi-lock-mode)
    (global-hi-lock-mode 1)
  (hi-lock-mode 1))

(require 'highlight-symbol)
(require 'sgml-mode)

(setq highlight-symbol-idle-delay 0.5)
(highlight-symbol-mode)

(defun highlight-symbol-mode-on ()
  "Turn on function `highlight-symbol-mode'."
  (highlight-symbol-mode 1))
(defun highlight-symbol-mode-off ()
  "Turn off function `highlight-symbol-mode'."
  (highlight-symbol-mode -1))

(dolist (hook '(emacs-lisp-mode-hook
		lisp-interaction-mode-hook
		java-mode-hook
		c++-mode-hook
		c-mode-common-hook
		text-mode-hook
		ruby-mode-hook
		html-mode-hook
		scheme-mode go-mode))

  (add-hook hook 'highlight-symbol-mode-on))

(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key (kbd "C-c r"  ) 'highlight-symbol-query-replace)
(global-set-key (kbd "C-c M-N") 'highlight-symbol-next-in-defun)
(global-set-key (kbd "C-c M-P") 'highlight-symbol-prev-in-defun)
(global-set-key (kbd "C-c f") 'highlight-symbol-next)
(global-set-key (kbd "C-c b") 'highlight-symbol-prev)
(global-set-key (kbd "C-<return>") 'other-window)


(require 'highlight-current-line)
(highlight-current-line)
(highlight-current-line-on t)
;; To customize the background color
(set-face-background 'highlight-current-line-face "gray29")

(require 'highlight-parentheses)
(defun turn-on-highlight-parentheses-mode ()
(highlight-parentheses-mode t))
(define-global-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  turn-on-highlight-parentheses-mode)
(global-highlight-parentheses-mode)
(setq hl-paren-background-colors '("green"))


(load-file "~/.emacs.d/lisp/c-config.el")
;;(load-file "~/.emacs.d/lisp/viewer.el")

;; (defun disable-highlight-current-line()
;;(highlight-current-line-on nil))

;;(add-hook 'eshell-mode-hook 'disable-highlight-current-line)

(defun open-eshell-now ()
  "Open eshell"
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (eshell))
(global-set-key (kbd "C-c e") 'open-eshell-now)

(defun go-to-other-window-and-close-this-one()
  "Go to other window and close current window"
  (interactive)
  (other-window 1)
  (delete-other-windows))

(global-set-key (kbd "C-c k") 'go-to-other-window-and-close-this-one)

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

(flymake-mode)


;; (require 'company)
;; (eval-after-load 'company
;;   '(progn
;;      (define-key company-active-map (kbd "C-n") 'company-select-next)
;;      (define-key company-active-map (kbd "C-p") 'company-select-previous)))

;; (add-hook 'after-init-hook 'global-company-mode)

;;(define-key company-mode-map "\C-n" 'company-select-next)
;;(define-key company-mode-map "\C-p" 'company-select-previous)

(add-to-list 'load-path "~/.emacs.d/auto-complete")

(setq auto-save-default nil)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start 1)
(setq ac-auto-show-menu t)
(setq ac-use-fuzzy t)
(setq ac-ignore-case 'smart)
(setq ac-ignore-case nil)

(require 'ac-company)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
(add-to-list 'ac-user-dictionary-files "~/.emacs.d/lisp/ac-dict")

(add-to-list 'load-path "~/.emacs.d/auto-complete-clang")
(require 'auto-complete-clang)
(define-key ac-mode-map  [(control tab)] 'auto-complete)


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


(defun search-symbol-occur-at-point()
  (interactive)
  (let ((search-target (symbol-name (symbol-at-point))))
    (isearch-occur search-target)))

(global-set-key (kbd "C-c 0") 'search-symbol-occur-at-point)

(require 'smart-compile+)
(global-set-key  (kbd "C-c ;") 'smart-compile)


(require 'python-mode)

(setq abbrev-mode t)
(global-set-key (kbd "C-=") 'dabbrev-expand)
(global-set-key (kbd "C-o") 'dabbrev-expand)

(setq ansi-color-names-vector
      ["black" "red" "green" "yellow" "sky blue" "magenta" "cyan" "white"])
(ansi-color-for-comint-mode-on)

;;(add-to-list 'load-path "~/.emacs.d/themes")
(require 'zenburn-theme)
;;(require 'zen-and-art-theme)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)


;;(setq which-func-modes t)
;;(which-func-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clean-buffer-list-delay-general 1)
 '(clean-buffer-list-delay-special 0)
 '(clean-buffer-list-kill-buffer-names
   (quote
    ("*Help*" "*Apropos*" "*Buffer List*" "*Compile-Log*"
     "*info*" "*vc*" "*vc-diff*" "*diff*" "*anything*" "*ag")))
 '(clean-buffer-list-kill-never-buffer-names nil)
 '(clean-buffer-list-kill-regexps
   (quote ("^\\*Man " "^\\*scratch*" "^\\*GNU*")))
 ;;'(company-minimum-prefix-length 1)
 '(flymake-gui-warnings-enabled nil)
 ;;'(global-company-mode t)
 '(projectile-global-mode t)
 '(projectile-require-project-root nil))

(setq projectile-enable-caching t)
(setq which-func-cleanup-function
      (lambda (s) (set-text-properties 0 (length s) nil s) s))


(autoload 'markdown-mode "markdown-mode.el"
    "Major mode for editing Markdown files" t)
(setq auto-mode-alist
    (cons '("\\.md" . markdown-mode) auto-mode-alist))

(global-set-key (kbd "s-p")
		'eshell-previous-matching-input-from-input)

(require 'lua-mode)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(require 'auto-indent-mode)
(add-hook 'emacs-lisp-mode-hook 'auto-indent-minor-mode)
(add-hook 'javascript-mode-hook 'auto-indent-minor-mode)
(add-hook 'ruby-mode-hook 'auto-indent-minor-mode)

(add-hook 'python-mode-hook (lambda ()
			      (guess-indentation-style)))

(load-file "~/.emacs.d/lisp/set-indent.el")
(load-file "~/.emacs.d/lisp/guess-tab.el")


(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;; we want dired not not make always a new buffer if visiting a directory
;; but using only one dired buffer for all directories.
(defadvice dired-advertised-find-file (around dired-subst-directory activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let ((orig (current-buffer))
        (filename (dired-get-filename)))
    ad-do-it
    (when (and (file-directory-p filename)
               (not (eq (current-buffer) orig)))
      (kill-buffer orig))))

(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
          (lambda ()
            ;;(define-key dired-mode-map (kbd "<return>")
            ;;'dired-find-alternate-file) ; was dired-advertised-find-file
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file "..")))
            (define-key dired-mode-map (kbd "r")
              (lambda () (interactive) (find-alternate-file "..")))

            ))


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
(set-face-background 'mode-line "gray20")

(load-file "~/.emacs.d/auto/autoconfig.el")

;;(require 'cmuscheme)
(add-to-list 'load-path "~/.emacs.d/slime")
(add-to-list 'load-path "~/.emacs.d/swank-chicken")
(add-to-list 'load-path "/usr/local/lib/chicken/6/")
(setq swank-chicken-path "~/.emacs.d/swank-chicken/swank-chicken.scm")
(autoload 'chicken-slime "chicken-slime" "SWANK backend for Chicken" t)
(require 'slime)
(slime-setup '(slime-fancy slime-banner))
(add-hook 'scheme-mode-hook
          (lambda ()
            (slime-mode t)))

(setq slime-csi-path "/usr/local/bin/csi")
(set-variable (quote scheme-program-name) "csi")

(put 'set-goal-column 'disabled nil)

(add-to-list 'load-path "~/.emacs.d/erlang")
(require 'erlang)
(require 'erlang-start)
(require 'ag)
(require 'midnight)

(defun find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
   root-privileges (using tramp/sudo), if the file is not writable by user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

;; or some other keybinding...
(global-set-key (kbd "C-x F") 'find-file-as-root)

;; golang
(add-to-list 'load-path "~/.emacs.d/go-mode")
(require 'go-mode)
(require 'go-mode-load)
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;; (eval-after-load "go-mode"
;;   '(require 'flymake-go))

(defun open-eshell-run-go()
  "Open eshell and run this file"
  (interactive)
  (let ((name (file-name-nondirectory (buffer-file-name))))
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (eshell)
    (goto-char (point-max))
    (insert-string (concat "go run ./" name))
    (message "prepare run Go: %s" name)))


(add-hook 'go-mode-hook (lambda ()
			  (local-set-key (kbd "C-c C-k") 'godef-jump)
			  (local-set-key (kbd "C-c C-j") 'pop-tag-mark)
			  (local-set-key (kbd "C-c e") 'open-eshell-run-go)
			  (local-set-key (kbd "C-c f") 'helm-imenu)))

(load-file "~/.emacs.d/go-autocomplete.el")
(require 'go-autocomplete)

(add-hook 'go-mode-hook (lambda ()
			  (auto-complete-mode)
			  (highlight-symbol-mode)))

(require 'ess)
(defun run-cover ()
  "Run go coverage in tmp dir"
  (interactive)
  (let ((cmd (format "gocover %s" (buffer-file-name))))
    (shell-command cmd)))


(global-set-key (kbd "C-x g") 'run-cover)
(global-set-key (kbd "C-;") 'helm-projectile)


(load-file "~/.emacs.d/yaml-mode.el")
(require 'yaml-mode)

(load-file "~/.emacs.d/thrift-mode.el")
(require 'thrift-mode)

(require 'web-mode)
(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-script-padding 4)
  (setq web-mode-style-padding 4)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.gohtml\\'" . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(setq web-mode-engines-alist
      '(("razor" . "\\.gohtml\\'")))

(require 'yasnippet)
(delq 'ac-source-yasnippet ac-sources)
(require 'ido-select-window)
(defalias 'idw 'ido-select-window)
(defalias 'qrr 'query-replace)
(defalias 'lml 'list-matching-lines)
(defalias 'hi 'helm-imenu)
(defalias 'hf 'helm-projectile)
(defalias 'hr 'helm-recentf)
(defalias 'h 'helm-mini)
(defalias 'cp 'copy-region-as-kill)
(defalias 'bk 'helm-bookmarks)
(defalias 'kr 'kill-region)
(defalias 'gp 'grep)
(defalias 'cm 'comment-region)
(defalias 'uc 'uncomment-region)
(defalias 'gf 'grep-find)
(defalias 'fd 'find-dired)
(defalias 'e 'eshell)
(defalias 'ha 'helm-ag)
(defalias 'hat 'helm-ag-this-file)
(defalias 'i 'iy-go-to-char)
(defalias 'ib 'iy-go-to-char-backward)
(defalias 'w 'windmove-up)
(defalias 's 'windmove-down)
(defalias 'd 'windmove-right)
(defalias 'a 'windmove-left)
(defalias 'ep 'er/expand-region)
(defalias 'mh 'my-helm-multi-all)
(defalias 'f 'helm-buffers-list)
(defalias 'g 'god-mode-all)

(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-c C-c") 'copy-region-as-kill)

;; make buffer names unique even if the files have the same names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'window-numbering)
(window-numbering-mode)

(require 'smex)

(setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
(setq helm-ag-command-option "--all-text")
(setq helm-ag-thing-at-point 'symbol)

(add-hook 'emacs-startup-hook #'(lambda ()
                                  (let ((default-directory (getenv "HOME")))
                                    (command-execute 'eshell)
                                    (bury-buffer))))

(defun clear-by-mode()
  (interactive)
  (let ((all-buffers (buffer-list)))
    (dolist (buffer all-buffers)
      (when (string-equal (symbol-name (buffer-mode buffer))
			  "emacs-lisp-mode")
	 (message "%s" buffer)))))

(defun buffer-mode (&optional buffer-or-name)
  "Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
  (buffer-local-value 'major-mode
                      (if buffer-or-name (get-buffer buffer-or-name) (current-buffer))))

(require 'midnight)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-symbol-face ((t (:background "medium slate blue")))))

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)


(eval-after-load "helm-regexp"
  '(helm-attrset 'follow 1 helm-source-moccur))

(defun my-helm-multi-all ()
  "multi-occur in all buffers backed by files."
  (interactive)
  (helm-multi-occur
   (delq nil
         (mapcar (lambda (b)
                   (when (buffer-file-name b) (buffer-name b)))
                 (buffer-list)))))

(defun nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

(global-set-key (kbd "C-x K") 'nuke-all-buffers)


(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)

(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

(defun clear-and-send-input()
  (interactive)
  (if (> (count-lines 1 (point)) 800)
      (let ((inhibit-read-only t))
        (message "Clear the eshell now !")
	(erase-buffer)))
  (eshell-send-input))

(add-hook 'eshell-mode-hook
	  (lambda ()
	    (local-set-key (kbd "<return>") 'clear-and-send-input)))

(define-key god-local-mode-map (kbd ".") 'repeat)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

(add-to-list 'god-exempt-major-modes 'dired-mode)
(add-to-list 'god-exempt-major-modes 'eshell-mode)
(add-to-list 'god-exempt-major-modes 'shell-mode)

(require 'ac-js2)
(require 'js2-mode)
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;; Haskell mode
(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)


(require 'fill-column-indicator)
(set-fill-column 80)
(fci-mode)
