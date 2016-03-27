;; It's my Emacs init file
;; Love Emacs!
;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; turn off tool-bar
(tool-bar-mode -1)

;; turn off scroll-bar
(scroll-bar-mode -1)

;; turn off electric-indent-mode
(electric-indent-mode -1)

;; turn on recentf-mode
(recentf-mode t)

;; turn on linum-mode
(global-linum-mode t)

;; turn on global-company-mode
(global-company-mode t)

;; change font
(set-frame-font "Source Code Pro" t)
(set-face-attribute 'default nil :height 160)

;; close the start screen
(setq inhibit-splash-screen t)

;;define a function to open the init file
(defun open-my-init-file()
  (interactive)
  (find-file "~/Github/dotfiles/init.el"))

;; key bind map
(global-set-key (kbd "<f2>") 'open-my-init-file)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (company))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
