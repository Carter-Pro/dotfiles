;; turn off toool-bar
(tool-bar-mode -1)
;; turn off scroll-bar
(scroll-bar-mode -1)
;; close the start screen
(setq inhibit-splash-screen t)
;; change cursor to bar
;; Notice: cursor-type is a buffer-local variable
;; And The difference between setq and setq-default in Emacs Lisp
;; http://stackoverflow.com/questions/18172728/the-difference-between-setq-and-setq-default-in-emacs-lisp
(setq-default cursor-type 'bar)

;; full screen
;;(setq initial-frame-alist (quote ((fullscreen . maximized))))
(provide 'init-ui)
