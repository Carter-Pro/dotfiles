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

;; change font
(set-frame-font "Source Code Pro" t)
(set-face-attribute 'default nil :height 150)

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
(defun open-my-init-file()
  (interactive)
  (find-file "~/Github/dotfiles/init.el"))

;; key bind map
(global-set-key (kbd "<f2>") 'open-my-init-file)


(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

;; config org
(setq org-agenda-files (quote ("~/Documents/OneDrive/org-notes")))
(global-set-key (kbd "C-c a") 'org-agenda)
;; equals setq-default xxx
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.08)
 '(company-minimum-prefix-length 1)
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
