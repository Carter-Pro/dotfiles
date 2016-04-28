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
			  popwin
			  evil
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
(require 'smartparens-config)

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





(provide 'init-packages)
