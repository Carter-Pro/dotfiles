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


;; change font
;; Setting English Font
;; what i use Monaco14/STFangsong 16
(set-face-attribute 'default nil :font "Monaco 14")

;; Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset (font-spec :family "STFangsong"
                                       :size 16)))
;; choose a theme to load
(load-theme 'spacemacs-light t)
;; (load-theme 'monokai t)

(provide 'init-ui)
