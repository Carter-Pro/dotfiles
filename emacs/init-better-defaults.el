;; enable auto revert
(global-auto-revert-mode t)

;; turn on linum-mode
(global-linum-mode t)

;; highlight current line
(global-hl-line-mode t)


;; enable and config abbrev mode
(abbrev-mode t)
(define-abbrev-table 'global-abbrev-table '(
					    ;; Macrosoft
					    ("8ms" "Macrosoft")
					    ))

;; close file backups and auto-save
(setq make-backup-files nil)
(setq auto-save-default nil)

;; turn on recentf-mode
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; show match parents
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; add delete-selection-mode
(delete-selection-mode t)

;;let mac could find the excuable
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; turn on electric-indent-mode.
;; and we will use ";;"
(electric-indent-mode t)

;; config default coding system
;; set the default reading coding system
(prefer-coding-system 'utf-8)
;; set the default write coding system
(setq default-buffer-file-coding-system 'utf-8)

;; (setq linum-format 'linum-highlight-current-line)
(provide 'init-better-defaults)
