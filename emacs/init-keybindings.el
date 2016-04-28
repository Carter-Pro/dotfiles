
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(global-set-key "\C-s" 'swiper)

(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)

(global-set-key (kbd "<f2>") 'carter/open-init-file)
(global-set-key (kbd "<f3>") 'carter/open-gtd-file)
(global-set-key (kbd "<f4>") 'carter/open-refnotes-file)
(global-set-key (kbd "<f5>") 'carter/open-reference-file)

(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

(global-set-key (kbd "C-c a") 'org-agenda)

(global-set-key (kbd "C-c p s") 'my-org-screenshot)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-c p p") 'org-pomodoro)

;; key binding for open note
(global-set-key (kbd "C-c n") 'org-ref-open-bibtex-notes)

(global-set-key (kbd "C-c p f") 'counsel-git)

;; org-capture
(global-set-key (kbd "C-c r") 'org-capture)

(provide 'init-keybindings)
