;;; ag-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ag" "ag.el" (21254 7515 0 0))
;;; Generated autoloads from ag.el

(autoload 'ag "ag" "\
Search using ag in a given DIRECTORY for a given search STRING,
with STRING defaulting to the symbol under point.

\(fn STRING DIRECTORY)" t nil)

(autoload 'ag-files "ag" "\
Search using ag in a given DIRECTORY and file type regex FILE-REGEX
for a given search STRING, with STRING defaulting to the symbol under point.

\(fn STRING FILE-REGEX DIRECTORY)" t nil)

(autoload 'ag-regexp "ag" "\
Search using ag in a given directory for a given regexp.

\(fn STRING DIRECTORY)" t nil)

(autoload 'ag-project "ag" "\
Guess the root of the current project and search it with ag
for the given string.

\(fn STRING)" t nil)

(autoload 'ag-project-files "ag" "\
Search using ag in a given DIRECTORY and file type regex FILE-REGEX
for a given search STRING, with STRING defaulting to the symbol under point.

\(fn STRING FILE-REGEX)" t nil)

(autoload 'ag-project-regexp "ag" "\
Guess the root of the current project and search it with ag
for the given regexp.

\(fn REGEXP)" t nil)

(defalias 'ag-project-at-point 'ag-project)

(autoload 'ag-regexp-project-at-point "ag" "\
Same as ``ag-regexp-project'', but with the search regexp defaulting
to the symbol under point.

\(fn REGEXP)" t nil)

(autoload 'ag-kill-buffers "ag" "\
Kill all ag-mode buffers.

\(fn)" t nil)

(autoload 'ag-kill-other-buffers "ag" "\
Kill all ag-mode buffers other than the current buffer.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ag-autoloads.el ends here
