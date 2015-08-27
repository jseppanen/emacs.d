;; jarno.seppanen@iki.fi

;; Create helper fns for loading dotfile paths and files

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun add-lib-path (p)
  (add-to-list 'load-path (concat user-emacs-directory "lib/" p)))

;; Ensure the lib directory is on the load path
(add-to-list 'load-path (concat user-emacs-directory "lib"))

;; install packages
(load (concat user-emacs-directory "init-packages.el"))

;; configure editing
(load (concat user-emacs-directory "init-editing.el"))

;; configure navigation
(load (concat user-emacs-directory "init-navigation.el"))

;; configure UI
(load (concat user-emacs-directory "init-ui.el"))

;; configure clojure mode
(load (concat user-emacs-directory "init-clojure.el"))

(require 'typopunct)
(typopunct-change-language 'english t)

; Git
;(require 'git)
;(autoload 'git-status "git" "Enter git-status mode." t)
;(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)

; Matlab-mode

;; (autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
;; (setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
;; (autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)

;; (setq matlab-mode-install-path (list "~/matlab"))
;; (setq matlab-mode-install-path (append matlab-mode-install-path (list "c:/APPS/MATLAB6p5/toolbox")))

; Python stuff

;; (setq py-install-directory (concat user-emacs-directory "lib/python-mode.el-6.1.3"))
;; (add-to-list 'load-path py-install-directory)
;; (require 'python-mode)
;; (when (featurep 'python) (unload-feature 'python t))

(defun js-python-mode-hook ()
  (setq indent-tabs-mode nil)
  )
(add-hook 'python-mode-hook 'js-python-mode-hook)

; ipython
;; (setq
;;  python-shell-interpreter "ipython"
;;  python-shell-interpreter-args ""
;;  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;  python-shell-completion-setup-code
;;    "from IPython.core.completerlib import module_completion"
;;  python-shell-completion-module-string-code
;;    "';'.join(module_completion('''%s'''))\n"
;;  python-shell-completion-string-code
;;    "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")

;; ;; disable flymake popups
;; (defadvice flymake-display-warning (before minibuf-warning (warning) activate compile)
;;   "Display a warning to the user, in the mini-buffer"
;;   (message warning))

;; ;; flymake-pyflakes integration
;; ;; http://www.yilmazhuseyin.com/blog/dev/emacs-setup-python-development/
;; ;; http://www.plope.com/Members/chrism/flymake-mode
;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "pyflakes" (list local-file))))

;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pyflakes-init)))

;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;; (defun my-flymake-show-help ()
;;   (when (get-char-property (point) 'flymake-overlay)
;;    (let ((help (get-char-property (point) 'help-echo)))
;;     (if help (message "%s" help)))))

;; (add-hook 'post-command-hook 'my-flymake-show-help)

; emacs-ipython-notebook
;(require 'ein)

; Haskell-mode

;; (load-dotfile "lib/haskell-mode-2.8.0/haskell-site-file.el")
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;
;; User Level customizations (You need not use them all):
;; (defun my-matlab-mode-hook ()
;;   (setq fill-column 75)		; where auto-fill should wrap
;;   (setq matlab-verify-on-save-flag nil) ; turn off auto-verify on save
;; ;;   (setq matlab-indent-function t)	; if you want function bodies indented
;;   )
;; (add-hook 'matlab-mode-hook 'my-matlab-mode-hook)
;;   (defun my-matlab-shell-mode-hook ()
;;	'())
;;   (add-hook 'matlab-shell-mode-hook 'my-matlab-shell-mode-hook)
;;
;; Please read the mode help for matlab-mode for additional
;; configuration options.
;;


; C-mode




;; c-moden käynnistyksen yhteydessä tehtävät toimet

;(defun oma-c-mode-hook ()
;  (setq-default c-basic-offset 4)       ; sisennys 4
;  (setq tab-width 4)                    ; tabulaattori 4
;  (setq indent-tabs-mode nil)           ; käytetään välilyöntejä tabulaattorin asemesta
;  (c-set-offset 'substatement-open 0)   ; {}-merkkien sisennys 0
;  (c-set-offset 'case-label 4)          ; casejen sisennys switcheissä
;  (setq font-lock-maximum-decoration t)
;  (turn-on-font-lock)                   ; syntaksin erottelu päälle
;  (define-key c-mode-map [(control right)] 'c-forward-into-nomenclature) ; YksiSanaEteenpäinPitkässäKetjussa
;  (define-key c-mode-map [(control left)] 'c-backward-into-nomenclature) ; YksiSanaTaaksepäinPitkässäKetjussa
;  (c-toggle-auto-state 1)
;  (define-key c-mode-map "\C-m" 'newline-and-indent) ; automaattinen sisennys
;  (require 'compile)                    ; tahdotaan kääntääkin ohjelmia

; c-comment-mode

;  (define-key c-mode-map "\C-j" 'c-reindent-then-newline-and-indent)
;  (define-key c-mode-map "\015" 'c-newline)
;;  (define-key c-mode-map "\e:" 'c-comment)
;  (define-key c-mode-map [?\M-_] 'c-comment) ; M-_ aloittaa kommentin
;  (require 'c-comment)
;  (require 'c-newline)

; auto-fill-mode päälle

;  (auto-fill-mode t)
;  (setq current-fill-column 80)
;  )

;(add-hook 'c-mode-common-hook 'oma-c-mode-hook)

; for Herbie/Symbian work
;; (defun my-c-mode-hook ()
;;   ; indentation 4
;;   (setq-default c-basic-offset 4)
;;   ; indent with spaces, no tabs
;;   (setq indent-tabs-mode nil)
;;   ; backspace will untabify
;;   (setq backward-delete-char-untabify-method (quote hungry))
;;   ; braces {} at indentation 4 together with the block of code
;;   (c-set-offset 'substatement-open 4)
;;   (c-set-offset 'statement-block-intro 4)
;;   ; case indentation 4 within a switch
;;   (c-set-offset 'case-label 0)
;;   ; maximize speed
;;   (setq c-recognize-knr-p nil)
;;   ; coloured text
;;   (setq font-lock-maximum-decoration t)
;;   (turn-on-font-lock)
;;   ; YksiSanaEteenpäinPitkässäKetjussa
;;   (define-key c-mode-map [(control right)] 'c-forward-into-nomenclature)
;;   ; YksiSanaTaaksepäinPitkässäKetjussa
;;   (define-key c-mode-map [(control left)] 'c-backward-into-nomenclature)
;;   ; wrap lines
;;   (auto-fill-mode t)
;;   ; wrap after 80 characters
;;   (setq fill-column 80)
;;   (setq current-fill-column 80)
;;   )
;; (add-hook 'c-mode-common-hook 'my-c-mode-hook)
;; ; recognize .inl files as C/C++
;; (setq auto-mode-alist (cons '("\\.inl$" . c-mode) auto-mode-alist))

;; ; count-words-region function

;; (defun count-words-region (beginning end)
;;   "Print number of words in the region."
;;   (interactive "r")
;;   (message "Counting words in region ... ")

;; ;;; 1. Set up appropriate conditions.
;;   (save-excursion
;;     (let ((count 0))
;;       (goto-char beginning)

;; ;;; 2. Run the while loop.
;;       (while (and (< (point) end)
;;                   (re-search-forward "\\w+\\W*" end t))
;;         (setq count (1+ count)))

;; ;;; 3. Send a message to the user.
;;       (cond ((zerop count)
;;              (message
;;               "The region does NOT have any words."))
;;             ((= 1 count)
;;              (message
;;               "The region has 1 word."))
;;             (t
;;              (message
;;               "The region has %d words." count))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit volatile-highlights tagedit smex scala-mode rainbow-delimiters projectile pig-mode paredit minimap magit-popup lua-mode ido-ubiquitous ess ein clojure-mode-extra-font-locking cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'narrow-to-region 'disabled nil)

(put 'upcase-region 'disabled nil)

;; (defun my-c-mode-hook ()
;;   ; indentation 4
;;   (setq-default c-basic-offset 4)
;;   ; indent with spaces, no tabs
;;   (setq indent-tabs-mode nil)
;;   ; backspace will untabify
;;   (setq backward-delete-char-untabify-method (quote hungry))
;;   ; maximize speed
;;   (setq c-recognize-knr-p nil)
;;   ; coloured text
;;   (setq font-lock-maximum-decoration t)
;;   (turn-on-font-lock)
;;   ; wrap lines
;;   (auto-fill-mode t)
;;   ; wrap after 80 characters
;;   (setq fill-column 80)
;;   (setq current-fill-column 80)
;;   )
;; (add-hook 'c-mode-common-hook 'my-c-mode-hook)

;; clojure/cascalog

;; (unless (package-installed-p 'clojure-mode)
;;   (package-refresh-contents)
;;   (package-install 'clojure-mode))

;; (autoload 'paredit-mode "paredit"
;; "Minor mode for pseudo-structurally editing Lisp code." t)
;; (add-hook 'clojure-mode-hook          (lambda () (paredit-mode +1)))
;; (add-hook 'slime-mode-hook            (lambda () (paredit-mode +1)))
;; (add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
;; (add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
;; (add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
;; (add-hook 'slime-mode-hook            (lambda () (rainbow-delimiters-mode +1)))
;; (add-hook 'clojure-mode-hook          (lambda () (rainbow-delimiters-mode +1)))
;; (require 'clojure-mode)

;; (setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist))

;; (require 'dircolors)
;; (require 'rainbow-delimiters)
;; (require 'mwe-log-commands)
;; (require 'ace-jump-mode)
;; (require 'key-chord)
;; (key-chord-mode 1)

;; JSON
(setq auto-mode-alist (cons '("\\.json$" . javascript-mode) auto-mode-alist))
(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

;(define-key js-mode-map (kbd "C-c C-f") 'beautify-json)

;; ;;CoffeeScript
;; (add-lib-path "coffee-mode")
;; (require 'coffee-mode)

;; ;;Mustache/Handlebars
;; (require 'mustache-mode)
;; (setq auto-mode-alist (cons '("\\.hbs$" . mustache-mode) auto-mode-alist))

;; ;;React JSX
;; (setq auto-mode-alist (cons '("\\.jsx$" . javascript-mode) auto-mode-alist))

;; ;;Markdown
;; (add-lib-path "markdown-mode")
;; (require 'markdown-mode)
;; (setq auto-mode-alist
;;    (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; ;; enable typopunct for markdown-mode
;; (add-hook 'markdown-mode-hook 'my-markdown-init)
;; (defun my-markdown-init ()
;;   (require 'typopunct)
;;   (typopunct-change-language 'english)
;;   (typopunct-mode 1))

;; (require 'ansi-color)

;; org-mode
(require 'org-install)
;(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/Dropbox/org/testi.org"
                             "~/Dropbox/org/work.org"))

;; ;; R
;; ;; (package-install ess)
(add-to-list 'load-path "/usr/share/emacs24/site-lisp/ess")
(require 'ess-site)
(define-key ess-mode-map "_" 'self-insert-command)
(define-key inferior-ess-mode-map "_" 'self-insert-command)

;; ;; alternative to options(width=999)
;; (defun my-ess-post-run-hook ()
;;   (ess-execute-screen-options)
;;   (local-set-key "\C-cw" 'ess-execute-screen-options))
;; (add-hook 'ess-post-run-hook 'my-ess-post-run-hook)

;; ;; YAML
;; (load-file (concat user-emacs-directory "lib/yaml-mode.el"))
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; ;; Scala
;; (require 'scala-mode)
;; (add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))

;; ;; minimap
;; (require 'minimap)

;; ;; Pig Latin
;; (unless (package-installed-p 'pig-mode)
;;   (package-refresh-contents)
;;   (package-install 'pig-mode))
;; (require 'pig-mode)
;; (setq pig-indent-level 2)
