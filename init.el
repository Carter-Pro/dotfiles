;; It's my Emacs init file
;; Love Emacs!
;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/") t)
  )

;; require common lisp
(require 'cl)

;; add whatever packages you want here
;; define a package list.
(defvar carter/packages '(
                          company
			  zenburn-theme
			  solarized-theme
			  spacemacs-theme
			  monokai-theme
			  hungry-delete
			  swiper
			  counsel
			  smartparens
			  exec-path-from-shell
			  org-ref
			  helm-bibtex
			  evil
			  fcitx
			  dash
			  chinese-fonts-setup
			  magit
			  org-pomodoro
			  emacs-eclim
			  )  "Default packages")

;; prevent package-autoremove delete the packages we installed.
(setq package-selected-packages carter/packages)

;; look if the packages were installed.
(defun carter/packages-installed-p ()
  (loop for pkg in carter/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (carter/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg carter/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; choose a theme to load
(load-theme 'spacemacs-light t)

;; turn off tool-bar
(tool-bar-mode -1)

;; turn off scroll-bar
(scroll-bar-mode -1)

;; turn on electric-indent-mode.
;; and we will use ";;"
(electric-indent-mode t)

;; add delete-selection-mode
(delete-selection-mode t)

;; turn on recentf-mode
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; enable hungry delete mode
(require 'hungry-delete)
(global-hungry-delete-mode)

;; enable swiper
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)

;; enable smartparens
(require 'smartparens-config)
;;(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(smartparens-global-mode t)

;; turn on linum-mode
(global-linum-mode t)

;; turn on global-company-mode
(global-company-mode t)
;; turn off company-mode for org-mode
(setq company-global-modes '(not org-mode))

;; change font
;; Setting English Font
(set-face-attribute 'default nil :font "Monaco 18")

;; Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset (font-spec :family "STFangsong"
                                       :size 22)))
;; close the start screen
(setq inhibit-splash-screen t)

;; close file backups
(setq make-backup-files nil)

;; change cursor to bar
;; Notice: cursor-type is a buffer-local variable
;; And The difference between setq and setq-default in Emacs Lisp
;; http://stackoverflow.com/questions/18172728/the-difference-between-setq-and-setq-default-in-emacs-lisp
(setq-default cursor-type 'bar)

;; open emacs full screen
;;(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; show match parents
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; highlight current line
(global-hl-line-mode t)

;;let mac could find the excuable
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; turn on syntax high light org src
(require 'org)
(setq org-src-fontify-natively t)

;;define a function to open the init file
(defun carter/open-init-file()
  (interactive)
  (find-file "~/Github/dotfiles/init.el"))
(global-set-key (kbd "<f2>") 'carter/open-init-file)

;; define a function to open the gtd.org
(defun carter/open-gtd-file()
  (interactive)
  (find-file "/Users/carter/Documents/OneDrive/org-notes/gtd.org"))
(global-set-key (kbd "<f3>") 'carter/open-gtd-file)

;; define a function to open the refnotes.org
(defun carter/open-refnotes-file()
  (interactive)
  (find-file "/Users/carter/Documents/OneDrive/Papers/refnotes.org"))
(global-set-key (kbd "<f4>") 'carter/open-refnotes-file)

(defun carter/open-reference-file()
  (interactive)
  (find-file "/Users/carter/Documents/OneDrive/Papers/references.bib"))
(global-set-key (kbd "<f5>") 'carter/open-reference-file)

;; key binding
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

;; config org
(setq org-agenda-files (quote ("~/Documents/OneDrive/org-notes")))
(global-set-key (kbd "C-c a") 'org-agenda)

;; use org-mode to manage reference.
(require 'org-ref)
(setq reftex-default-bibliography '("/Users/carter/Documents/OneDrive/Papers/references.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "/Users/carter/Documents/OneDrive/Papers/refnotes.org"
      org-ref-default-bibliography '("/Users/carter/Documents/OneDrive/Papers/references.bib")
      org-ref-pdf-directory "/Users/carter/Documents/OneDrive/Papers/Collections/")

;; config helm-bibtex
(autoload 'helm-bibtex "helm-bibtex" "" t)
(setq helm-bibtex-bibliography "/Users/carter/Documents/OneDrive/Papers/references.bib")
(setq helm-bibtex-library-path "/Users/carter/Documents/OneDrive/Papers/Collections")

;; open pdf with system pdf viewer (works on mac)
(setq helm-bibtex-pdf-open-function
  (lambda (fpath)
    (start-process "open" "*open*" "open" fpath)))

;; alternative
;; (setq helm-bibtex-pdf-open-function 'org-open-file)

(setq helm-bibtex-notes-path "/Users/carter/Documents/OneDrive/Papers/helm-bibtex-notes")


;; equals setq-default xxx
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cfs--current-profile-name "word" t)
 '(company-idle-delay 0.08)
 '(company-minimum-prefix-length 3)
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(org-agenda-files
   (quote
    ("/Users/carter/Documents/OneDrive/org-notes/gtd.org" "/Users/carter/Documents/OneDrive/org-notes/org-ref.org")))
 '(org-ref-note-title-format
   "** TODO [#C] %y - %t
 :PROPERTIES:
  :Custom_ID: %k
  :AUTHOR: %9a
  :JOURNAL: %j
  :YEAR: %y
  :VOLUME: %v
  :PAGES: %p
  :DOI: %D
  :URL: %U
 :END:
")
 '(package-selected-packages
   (quote
    (emacs-eclim magit company zenburn-theme solarized-theme spacemacs-theme monokai-theme hungry-delete swiper counsel smartparens exec-path-from-shell org-ref helm-bibtex evil fcitx dash chinese-fonts-setup))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; config org-ref
;;(add-hook 'org-ref-clean-bibtex-entry-hook 'org-ref-replace-nonascii)
(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

;; key binding for open note
(global-set-key (kbd "C-c n") 'org-ref-open-bibtex-notes)
;; add wrap for org-mode
(add-hook 'org-mode-hook (lambda()(setq truncate-lines nil)))

;; config evil
(require 'evil)
;;(evil-mode 1)

;; screenshot
;; http://stackoverflow.com/questions/17435995/paste-an-image-on-clipboard-to-emacs-org-mode-file-without-saving-it
(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)
  (setq filename
        (concat
         (make-temp-name
	  "use file-name-sans-extension to ignore file extension."
          (concat (file-name-nondirectory (file-name-sans-extension buffer-file-name))
                  "/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
  ; take screenshot
  (if (eq system-type 'darwin)
      (call-process "screencapture" nil nil nil "-i" filename))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))
  ; insert into file if correctly taken
  (if (file-exists-p filename)
      (insert (concat "[[file:" filename "]]"))))
(global-set-key (kbd "C-p") 'my-org-screenshot)

;; config fcitx
(add-to-list 'load-path "/Users/carter/.emacs.d/elpa/fcitx-20160320.2220/fcitx.el")
(require 'fcitx)
;; sort org-mode by priority
(require 'dash)

(defun todo-to-int (todo)
    (first (-non-nil
            (mapcar (lambda (keywords)
                      (let ((todo-seq
                             (-map (lambda (x) (first (split-string  x "(")))
                                   (rest keywords)))) 
                        (cl-position-if (lambda (x) (string= x todo)) todo-seq)))
                    org-todo-keywords))))

(defun my/org-sort-key ()
  (let* ((todo-max (apply #'max (mapcar #'length org-todo-keywords)))
         (todo (org-entry-get (point) "TODO"))
         (todo-int (if todo (todo-to-int todo) todo-max))
         (priority (org-entry-get (point) "PRIORITY"))
         (priority-int (if priority (string-to-char priority) org-default-priority)))
    (format "%03d %03d" todo-int priority-int)
    ))

(defun carter/org-sort-entries ()
  (interactive)
  (org-sort-entries nil ?f #'carter/org-sort-key))

;;设置默认读入文件编码

(prefer-coding-system 'utf-8)

;;设置写入文件编码

(setq default-buffer-file-coding-system 'utf-8)

;; config chinese-fonts-setup
;;(require 'chinese-fonts-setup)
;;(setq cfs-profiles
;;    '("program" "org-mode" "word"))
;; config magit
(global-set-key (kbd "C-x g") 'magit-status)

;; config org-pomodoro
(global-set-key (kbd "C-p") 'org-pomodoro)

;; org clock
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; org agenda

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
(add-hook 'org-pomodoro-finished-hook
           (lambda ()
             (carter/notify "Pomodoro completed!" "Time for a break.")))
(add-hook 'org-pomodoro-break-finished-hook
           (lambda ()
             (carter/notify "Pomodoro Short Break Finished" "Ready for Another?")))
(add-hook 'org-pomodoro-long-break-finished-hook
           (lambda ()
             (carter/notify "Pomodoro Long Break Finished" "Ready for Another?")))
(add-hook 'org-pomodoro-killed-hook
           (lambda ()
             (carter/notify "Pomodoro Killed" "One does not simply kill a pomodoro!")))
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


;; config emacs-eclim
(require 'eclim)
(global-eclim-mode)
(custom-set-variables
  '(eclim-eclipse-dirs '("~/Applications/Eclipse.app/Contents/MacOS/eclipse"))
  '(eclim-executable "~/Applications/Eclipse.app/Contents/MacOS/eclipse/eclim"))
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)
