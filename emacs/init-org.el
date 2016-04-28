;; turn on syntax high light org src
(require 'org)
(setq org-src-fontify-natively t)

;; config org
;; add wrap for org-mode
(add-hook 'org-mode-hook (lambda()(setq truncate-lines nil)))
;; org agenda
(setq org-agenda-files (quote ("~/Documents/OneDrive/org-notes")))

;; org-capture
(global-set-key (kbd "C-c r") 'org-capture)
(setq org-capture-templates
      '(("t" "Inbox" entry (file+headline "~/Documents/OneDrive/org-notes/gtd.org" "Inbox")
	 "* TODO [#B] %?\n  %i\n"
	 :empty-lines 1)
	("e" "Emacs" entry (file+headline "~/Documents/OneDrive/org-notes/gtd.org" "Emacs")
	 "* TODO [#B] %?\n  %i\n"
	 :empty-lines 1)
	("p" "Progeam" entry (file+headline "~/Documents/OneDrive/org-notes/gtd.org" "Programming")
	 "* TODO [#B] %?\n  %i\n"
	 :empty-lines 1)
	("a" "Academic" entry (file+headline "~/Documents/OneDrive/org-notes/gtd.org" "Academic")
	 "* TODO [#B] %?\n  %i\n"
	 :empty-lines 1)
	("j" "Jobs" entry (file+headline "~/Documents/OneDrive/org-notes/gtd.org" "Find Jobs")
	 "* TODO [#B] %?\n  %i\n"
	 :empty-lines 1)
	("r" "Read" entry (file+headline "~/Documents/OneDrive/org-notes/gtd.org" "Reading List")
	 "* TODO [#B] %?\n  %i\n"
	 :empty-lines 1)
	("b" "Body" entry (file+headline "~/Documents/OneDrive/org-notes/gtd.org" "Body Building")
	 "* TODO [#B] %?\n  %i\n"
	 :empty-lines 1)))







(provide 'init-org)
