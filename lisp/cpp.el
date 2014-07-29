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

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c/c++ header include guard
(defun insert-include-guard ()
  "insert include guard for c and c++ header file.
for file filename.ext will generate:
#ifndef FILENAME_EXT_
#define FILENAME_EXT_

original buffer content

#endif//FILENAME_EXT_
"
  (interactive)
  (setq file-macro
	(concat "_" (replace-regexp-in-string "\\." "_"
					      (upcase (file-name-nondirectory buffer-file-name))) "_"))
  (setq guard-begin (concat "#ifndef " file-macro "\n"
			    "#define " file-macro "\n\n"))
  (setq guard-end
	(concat "\n\n#endif//" file-macro "\n"))
  (setq position (point))
  (goto-char (point-min))
  (insert guard-begin)
  (goto-char (point-max))
  (insert guard-end)
  (goto-char (+ position (length guard-begin))))
