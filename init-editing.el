;;When you visit a file, point goes to the last place where it was when you previously visited
;;Save file is set to "~/.emacs.d/tmp/places
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "tmp/places"))

;; create autosaves and backups tmp dirs if necessary
(make-directory (concat user-emacs-directory "tmp/autosaves") t)
(make-directory (concat user-emacs-directory "tmp/backups") t)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/tmp/
(setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "tmp/autosaves/\\1") t)))
(setq backup-directory-alist `((".*" . ,(concat user-emacs-directory "tmp/backups"))))
(setq auto-save-list-file-name (concat user-emacs-directory "tmp/autosaves/autosave-list"))

(global-set-key (kbd "C-x g") 'magit-status)
