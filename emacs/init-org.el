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

;; replace \emsp to \__ in org-clock-report
;;http://emacs.stackexchange.com/questions/9528/is-it-possible-to-remove-emsp-from-clock-report-but-preserve-indentation
(defun my-org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "\\"))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "_")))
      (concat str "_ "))))

(advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)

;; org clock
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)


;; org mode archive
(defun carter/org-archive-tasks (prefix)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   (format "/%s" prefix) 'file))

(defun carter/org-archive-all-tasks ()
  (interactive)
  (carter/org-archive-tasks "DONE")
  (carter/org-archive-tasks "CANCELLED")
  (carter/org-archive-tasks "FIXED")
  )

;; org-archive-subtree-hierarchical.el
;; modified from https://lists.gnu.org/archive/html/emacs-orgmode/2014-08/msg00109.html

;; In orgmode
;; * A
;; ** AA
;; *** AAA
;; ** AB
;; *** ABA
;; Archiving AA will remove the subtree from the original file and create
;; it like that in archive target:

;; * AA
;; ** AAA

;; And this give you
;; * A
;; ** AA
;; *** AAA


(require 'org-archive)

(defun org-archive-subtree-hierarchical--line-content-as-string ()
  "Returns the content of the current line as a string"
  (save-excursion
    (beginning-of-line)
    (buffer-substring-no-properties
     (line-beginning-position) (line-end-position))))

(defun org-archive-subtree-hierarchical--org-child-list ()
  "This function returns all children of a heading as a list. "
  (interactive)
  (save-excursion
    ;; this only works with org-version > 8.0, since in previous
    ;; org-mode versions the function (org-outline-level) returns
    ;; gargabe when the point is not on a heading.
    (if (= (org-outline-level) 0)
        (outline-next-visible-heading 1)
      (org-goto-first-child))
    (let ((child-list (list (org-archive-subtree-hierarchical--line-content-as-string))))
      (while (org-goto-sibling)
        (setq child-list (cons (org-archive-subtree-hierarchical--line-content-as-string) child-list)))
      child-list)))

(defun org-archive-subtree-hierarchical--org-struct-subtree ()
  "This function returns the tree structure in which a subtree
belongs as a list."
  (interactive)
  (let ((archive-tree nil))
    (save-excursion
      (while (org-up-heading-safe)
        (let ((heading
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))))
          (if (eq archive-tree nil)
              (setq archive-tree (list heading))
            (setq archive-tree (cons heading archive-tree))))))
    archive-tree))

(defun org-archive-subtree-hierarchical ()
  "This function archives a subtree hierarchical"
  (interactive)
  (let ((org-tree (org-archive-subtree-hierarchical--org-struct-subtree))
        (this-buffer (current-buffer))
        (file (abbreviate-file-name
               (or (buffer-file-name (buffer-base-buffer))
                   (error "No file associated to buffer")))))
    (save-excursion
      (setq location (org-get-local-archive-location)
            afile (org-extract-archive-file location)
            heading (org-extract-archive-heading location)
            infile-p (equal file (abbreviate-file-name (or afile ""))))
      (unless afile
        (error "Invalid `org-archive-location'"))
      (if (> (length afile) 0)
          (setq newfile-p (not (file-exists-p afile))
                visiting (find-buffer-visiting afile)
                buffer (or visiting (find-file-noselect afile)))
        (setq buffer (current-buffer)))
      (unless buffer
        (error "Cannot access file \"%s\"" afile))
      (org-cut-subtree)
      (set-buffer buffer)
      (org-mode)
      (goto-char (point-min))
      (while (not (equal org-tree nil))
        (let ((child-list (org-archive-subtree-hierarchical--org-child-list)))
          (if (member (car org-tree) child-list)
              (progn
                (search-forward (car org-tree) nil t)
                (setq org-tree (cdr org-tree)))
            (progn
              (goto-char (point-max))
              (newline)
              (org-insert-struct org-tree)
              (setq org-tree nil)))))
      (newline)
      (org-yank)
      (when (not (eq this-buffer buffer))
        (save-buffer))
      (message "Subtree archived %s"
               (concat "in file: " (abbreviate-file-name afile))))))

(defun org-insert-struct (struct)
  "TODO"
  (interactive)
  (when struct
    (insert (car struct))
    (newline)
    (org-insert-struct (cdr struct))))

(defun org-archive-subtree ()
  (org-archive-subtree-hierarchical)
  )

;; org-pomodoro notification
;;(add-hook 'org-pomodoro-finished-hook
;;           (lambda ()
;;             (carter/notify "Pomodoro completed!" "Time for a break.")))
;;(add-hook 'org-pomodoro-break-finished-hook
;;           (lambda ()
;;             (carter/notify "Pomodoro Short Break Finished" "Ready for Another?")))
;;(add-hook 'org-pomodoro-long-break-finished-hook
;;           (lambda ()
;;             (carter/notify "Pomodoro Long Break Finished" "Ready for Another?")))
;;(add-hook 'org-pomodoro-killed-hook
;;           (lambda ()
;;             (carter/notify "Pomodoro Killed" "One does not simply kill a pomodoro!")))
(defun carter/notify (title message)
  (let ((terminal-notifier-command (executable-find "terminal-notifier")))))

;; when subtasks finished, task reset
(setq org-default-properties (cons "RESET_SUBTASKS" org-default-properties))

(defun org-reset-subtask-state-subtree ()
  "Reset all subtasks in an entry subtree."
  (interactive "*")
  (if (org-before-first-heading-p)
      (error "Not inside a tree")
    (save-excursion
      (save-restriction
    (org-narrow-to-subtree)
    (org-show-subtree)
    (goto-char (point-min))
        (beginning-of-line 2)
        (narrow-to-region (point) (point-max))
        (org-map-entries
         '(when (member (org-get-todo-state) org-done-keywords)
            (org-todo (car org-todo-keywords))))
        ))))

(defun org-reset-subtask-state-maybe ()
  "Reset all subtasks in an entry if the `RESET_SUBTASKS' property is set"
  (interactive "*")
  (if (org-entry-get (point) "RESET_SUBTASKS")
      (org-reset-subtask-state-subtree)))

(defun org-subtask-reset ()
  (when (member org-state org-done-keywords) ;; org-state dynamically bound in org.el/org-todo
    (org-reset-subtask-state-maybe)
    (org-update-statistics-cookies t)))

(add-hook 'org-after-todo-state-change-hook 'org-subtask-reset)

;;
(defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)  ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO")))) 

(add-hook'org-after-todo-statistics-hook 'org-summary-todo)






(provide 'init-org)
