(require 'auto-complete-clang)
(define-key ac-mode-map  [(control tab)] 'auto-complete)

(defun my-ac-config ()
  ;;(setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-all-buffer))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup))
(my-ac-config)

(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (split-string
               "
 /usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include
 /usr/include/c++/4.2.1
 /usr/include/c++/4.2.1/backward
 /usr/local/include
 /Applications/Xcode.app/Contents/Developer/usr/llvm-gcc-4.2/lib/gcc/i686-apple-darwin11/4.2.1/include
 /usr/include
 /Users/kang/code/Panda/inc
 /System/Library/Frameworks
 /Library/Frameworks
"
               )))


(require 'flymake-clang-c)
(require 'flymake-clang-c++)

(require 'clang-lookup)
(defun my-c-mode-common-hook()
  (setq tab-width 4 indent-tabs-mode nil)
  (hs-minor-mode t)
  hungry-delete and auto-newline
  (c-toggle-auto-hungry-state 1)
  (c-set-style "stroustrup")
  (setq c-basic-offset 4)
  (flymake-clang-c-load)
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
  (setq ac-auto-start nil)
  (setq ac-expand-on-auto-complete nil)
  (setq ac-quick-help-delay 0.5)
  (define-key c-mode-base-map (kbd "M-/") 'ac-complete-clang)
  (define-key c-mode-base-map (kbd "C-j") 'ac-complete-clang)
  )

(defface font-lock-function-call-face
  '((t (:foreground "LightSkyBlue2")))
  "Font Lock mode face used to highlight function calls."
  :group 'font-lock-highlighting-faces)

(defvar font-lock-function-call-face 'font-lock-function-call-face)
(add-hook 'my-c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(\\sw+\\) ?(" 1 font-lock-function-call-face)) t)))

(add-hook 'c-mode-hook
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
  (flymake-clang-c++-load)
  (define-key c-mode-base-map [(return)] 'newline-and-indent))


(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-common-hook)
(add-hook 'python-mode-hook '(lambda ()
                               (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'ruby-mode-hook '(lambda ()
                             (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'lua-mode-hook '(lambda ()
                            (local-set-key (kbd "RET") 'newline-and-indent)))

(require 'xcscope)
(add-hook 'c-mode-common-hook
          '(lambda ()
             (require 'xcscope)))
(add-hook 'c++-mode-common-hook
          '(lambda()
             (require 'xcscope)))



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

;;(global-set-key (kbd "C-,") 'gtags-find-tag)
;;(global-set-key (kbd "C-.") 'gtags-pop-stack)
(global-set-key [f9] 'gtags-find-symbol)
(global-set-key [(shift f9)] 'gtags-pop-stack)
(global-set-key (kbd "C-x j f") 'gtags-find-file)

(global-set-key (kbd "C-;") 'gtags-find-file)

(require 'xgtags)
(xgtags-mode 1)

(global-set-key (kbd "C-x j t") 'xgtags-find-tag)
(global-set-key (kbd "C-.") 'xgtags-pop-stack)
(global-set-key (kbd "C-x j b") 'xgtags-pop-stack)
(global-set-key (kbd "C-x j f") 'gtags-find-file)
(global-set-key (kbd "C-;") 'gtags-find-file)
(global-set-key (kbd "M-n") 'xgtags-select-next-tag)
(global-set-key (kbd "M-p") 'xgtags-select-prev-tag)

(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8))

(setq auto-mode-alist
      (cons
       '("/home/kang/code/linux.*/.*\\.[ch]$" . linux-c-mode)
       auto-mode-alist))


(defun open-eshell-run-this-file()
  "Open eshell and run this file"
  (interactive)
  (let ((name (file-name-base (buffer-file-name))))
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (eshell)
    (goto-char (point-max))
    (insert-string (concat "./" name))
    (message "prepare run: %s" name)))

(global-set-key (kbd "C-c '") 'open-eshell-run-this-file)
