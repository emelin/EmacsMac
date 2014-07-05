(require 'tabbar)
(tabbar-mode)
(global-set-key (kbd "C-c b") 'tabbar-backward-group)
(global-set-key (kbd "C-c f") 'tabbar-forward-group)
(global-set-key (kbd "C-x [") 'tabbar-backward)
(global-set-key (kbd "C-x ]") 'tabbar-forward)
(global-set-key (kbd "C-c ]") 'previous-buffer)
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

