;; require common lisp
(require 'cl)

(when (>= emacs-major-version 24)
  (package-initialize)
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/") t)
  )

;; add whatever packages you want here
;; define a package list.
(defvar carter/packages '(
                          company
			  org
			  ctable
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
			  dash
			  magit
			  org-pomodoro
			  popwin
			  evil
			  fcitx
			  names
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


;; enable hungry delete mode
(require 'hungry-delete)
(global-hungry-delete-mode)

;; swiper
(ivy-mode 1)

(setq ivy-use-virtual-buffers t)

;; enable smartparens
(smartparens-global-mode t)


;; turn on global-company-mode
(global-company-mode t)
(setq company-global-modes '(not org-mode))

;; choose a theme to load
(load-theme 'spacemacs-light t)
;;(load-theme 'monokai t)

;; evil
(require 'evil)
(evil-mode 1)

;; config popwin
(require 'popwin)
(popwin-mode t)

;; use emacs to write blog
(add-to-list 'load-path "/Users/carter/Softwares/blog-admin")
(require 'blog-admin)
(setq blog-admin-backend-path "/Users/carter/blog")
(setq blog-admin-backend-type 'hexo)
(setq blog-admin-backend-new-post-in-drafts t) 
(setq blog-admin-backend-new-post-with-same-name-dir t) 
(add-hook 'blog-admin-backend-after-new-post-hook 'find-file)

;; use org-mode to manage reference.
(require 'org-ref)
;; see org-ref for use of these variables
(setq reftex-default-bibliography '("/Users/carter/Documents/OneDrive/Papers/references.bib"))
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
;; config org-ref
;;(add-hook 'org-ref-clean-bibtex-entry-hook 'org-ref-replace-nonascii)
(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

(require 'fcitx)




(provide 'init-packages)
