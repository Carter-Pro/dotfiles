
;;define a function to open the init file
(defun carter/open-init-file()
  (interactive)
  (find-file "~/Github/dotfiles/emacs/init.el"))

;; define a function to open the gtd.org
(defun carter/open-gtd-file()
  (interactive)
  (find-file "/Users/carter/Documents/OneDrive/org-notes/gtd.org"))

;; define a function to open the refnotes.org
(defun carter/open-refnotes-file()
  (interactive)
  (find-file "/Users/carter/Documents/OneDrive/Papers/refnotes.org"))

(defun carter/open-reference-file()
  (interactive)
  (find-file "/Users/carter/Documents/OneDrive/Papers/references.bib"))
(provide 'init-function)

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
          (concat (file-name-nondirectory (buffer-file-name))
                  "_imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
					; take screenshot
  (if (eq system-type 'darwin)
      (progn
	(call-process-shell-command "screencapture" nil nil nil nil " -s " (concat
									    "\"" filename "\"" ))
	(call-process-shell-command "convert" nil nil nil nil (concat "\"" filename "\" -resize  \"50%\"" ) (concat "\"" filename "\"" ))
	))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))
					; insert into file if correctly taken
  (if (file-exists-p filename)
      (insert (concat "[[file:" filename "]]")))
  (org-display-inline-images))
