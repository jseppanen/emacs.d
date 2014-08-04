;; jarno.seppanen@iki.fi

(setq inhibit-splash-screen t
      inhibit-startup-message t)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(column-number-mode t)
(transient-mark-mode t)

(setq display-time-24hr-format t)
(display-time-mode t)

(set-default-font "Consolas-10") ; 96 DPI

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(defun toggle-fullscreen ()
  (interactive)
  (if (frame-parameter nil 'fullscreen)
      (progn (set-frame-parameter nil 'fullscreen nil))
      (progn (set-frame-parameter nil 'fullscreen 'fullboth))))
(global-set-key [f11] 'toggle-fullscreen)

; line wrapping
(setq default-truncate-lines t)
(setq truncate-partial-width-windows 80)
(global-set-key [f12] 'toggle-truncate-lines)

; highlight overlong lines
;(require 'whitespace)
;(global-whitespace-mode t)
;(setq whitespace-line-column 79)
; (setq whitespace-style
;       '(face lines-tail))

; Unicode?
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
;(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Create a variable to store the path to this dotfile directory
;; (Usually ~/.emacs.d)
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Create variables to store the path to this dotfile dir's lib etc and tmp directories
(setq dotfiles-lib-dir (concat dotfiles-dir "lib/"))
(setq dotfiles-tmp-dir (concat dotfiles-dir "tmp/"))
(setq dotfiles-etc-dir (concat dotfiles-dir "etc/"))

;; Create helper fns for loading dotfile paths and files
(defun add-dotfile-path (p)
  (add-to-list 'load-path (concat dotfiles-dir p)))

(defun add-lib-path (p)
  (add-to-list 'load-path (concat dotfiles-lib-dir p)))

(defun load-dotfile (f)
  (load-file (concat dotfiles-dir f)))

;; Ensure the lib directory is on the load path
(add-dotfile-path "lib")

; Git
;(require 'git)
;(autoload 'git-status "git" "Enter git-status mode." t)
;(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)

; Matlab-mode

(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)

(setq matlab-mode-install-path (list "~/matlab"))
(setq matlab-mode-install-path (append matlab-mode-install-path (list "c:/APPS/MATLAB6p5/toolbox")))

; Python stuff

;(autoload 'python-mode "python-mode" "Python editing mode." t)
;(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;(setq auto-mode-alist (cons '("\\.pyw$" . python-mode) auto-mode-alist))
;(setq interpreter-mode-alist (cons '("python" . python-mode) interpreter-mode-alist))

(defun js-python-mode-hook ()
  (setq indent-tabs-mode nil)
  )
(add-hook 'python-mode-hook 'js-python-mode-hook)

(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

;; disable flymake popups
(defadvice flymake-display-warning (before minibuf-warning (warning) activate compile)
  "Display a warning to the user, in the mini-buffer"
  (message warning))

;; flymake-pyflakes integration
;; http://www.yilmazhuseyin.com/blog/dev/emacs-setup-python-development/
;; http://www.plope.com/Members/chrism/flymake-mode
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

(defun my-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
   (let ((help (get-char-property (point) 'help-echo)))
    (if help (message "%s" help)))))

(add-hook 'post-command-hook 'my-flymake-show-help)

; emacs-ipython-notebook
;(require 'ein)

; Haskell-mode

(load-dotfile "lib/haskell-mode-2.8.0/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;
;; User Level customizations (You need not use them all):
(defun my-matlab-mode-hook ()
  (setq fill-column 75)		; where auto-fill should wrap
  (setq matlab-verify-on-save-flag nil) ; turn off auto-verify on save
;;   (setq matlab-indent-function t)	; if you want function bodies indented
  )
(add-hook 'matlab-mode-hook 'my-matlab-mode-hook)
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

(setq fill-column 80)
(setq current-fill-column 80)

; for Herbie/Symbian work
(defun my-c-mode-hook ()
  ; indentation 4
  (setq-default c-basic-offset 4)
  ; indent with spaces, no tabs
  (setq indent-tabs-mode nil)
  ; backspace will untabify
  (setq backward-delete-char-untabify-method (quote hungry))
  ; braces {} at indentation 4 together with the block of code
  (c-set-offset 'substatement-open 4)
  (c-set-offset 'statement-block-intro 4)
  ; case indentation 4 within a switch
  (c-set-offset 'case-label 0)
  ; maximize speed
  (setq c-recognize-knr-p nil)
  ; coloured text
  (setq font-lock-maximum-decoration t)
  (turn-on-font-lock)
  ; YksiSanaEteenpäinPitkässäKetjussa
  (define-key c-mode-map [(control right)] 'c-forward-into-nomenclature)
  ; YksiSanaTaaksepäinPitkässäKetjussa
  (define-key c-mode-map [(control left)] 'c-backward-into-nomenclature)
  ; wrap lines
  (auto-fill-mode t)
  ; wrap after 80 characters
  (setq fill-column 80)
  (setq current-fill-column 80)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-hook)
; recognize .inl files as C/C++
(setq auto-mode-alist (cons '("\\.inl$" . c-mode) auto-mode-alist))

; count-words-region function

(defun count-words-region (beginning end)
  "Print number of words in the region."
  (interactive "r")
  (message "Counting words in region ... ")

;;; 1. Set up appropriate conditions.
  (save-excursion
    (let ((count 0))
      (goto-char beginning)

;;; 2. Run the while loop.
      (while (and (< (point) end)
                  (re-search-forward "\\w+\\W*" end t))
        (setq count (1+ count)))

;;; 3. Send a message to the user.
      (cond ((zerop count)
             (message
              "The region does NOT have any words."))
            ((= 1 count)
             (message
              "The region has 1 word."))
            (t
             (message
              "The region has %d words." count))))))

; FIXME mode: highlight all FIXME tags on screen
(setq fixme-modes '(c-mode c++-mode java-mode python-mode matlab-mode emacs-lisp-mode scheme-mode ess-mode))
(make-face 'font-lock-fixme-face)
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(FIXME\\)" 1 'font-lock-fixme-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "Red" "Yellow" nil t nil t nil nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(current-language-environment "utf-8")
 '(default-input-method "rfc1345")
 '(emacsw32-style-frame-title t)
 '(global-font-lock-mode t nil (font-lock))
 '(transient-mark-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(put 'narrow-to-region 'disabled nil)

(put 'upcase-region 'disabled nil)

(defun my-c-mode-hook ()
  ; indentation 4
  (setq-default c-basic-offset 4)
  ; indent with spaces, no tabs
  (setq indent-tabs-mode nil)
  ; backspace will untabify
  (setq backward-delete-char-untabify-method (quote hungry))
  ; maximize speed
  (setq c-recognize-knr-p nil)
  ; coloured text
  (setq font-lock-maximum-decoration t)
  (turn-on-font-lock)
  ; wrap lines
  (auto-fill-mode t)
  ; wrap after 80 characters
  (setq fill-column 80)
  (setq current-fill-column 80)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

(require 'package)
;(add-to-list 'package-archives
;	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; clojure/cascalog

(unless (package-installed-p 'clojure-mode)
  (package-refresh-contents)
  (package-install 'clojure-mode))

(autoload 'paredit-mode "paredit"
"Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'clojure-mode-hook          (lambda () (paredit-mode +1)))
(add-hook 'slime-mode-hook            (lambda () (paredit-mode +1)))
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'slime-mode-hook            (lambda () (rainbow-delimiters-mode +1)))
(add-hook 'clojure-mode-hook          (lambda () (rainbow-delimiters-mode +1)))
(require 'clojure-mode)

(setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist))

(add-to-list 'kill-emacs-query-functions '(lambda () (yes-or-no-p "Exit for sure? ")))

(require 'dircolors)
(require 'rainbow-delimiters)
(require 'mwe-log-commands)
(require 'ace-jump-mode)
(require 'key-chord)
(key-chord-mode 1)

;;Color-theme
(add-lib-path "color-theme")
(require 'color-theme)

;;CoffeeScript
(add-lib-path "coffee-mode")
(require 'coffee-mode)

;;Mustache/Handlebars
(require 'mustache-mode)
(setq auto-mode-alist (cons '("\\.hbs$" . mustache-mode) auto-mode-alist))

;;React JSX
(setq auto-mode-alist (cons '("\\.jsx$" . javascript-mode) auto-mode-alist))

;;Markdown
(add-lib-path "markdown-mode")
(require 'markdown-mode)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

(load-file (concat dotfiles-lib-dir "blackbored.el"))
(color-theme-blackbored)

;;make sure ansi colour character escapes are honoured
(ansi-color-for-comint-mode-on)

(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; create autosaves and backups tmp dirs if necessary
(make-directory (concat dotfiles-tmp-dir "autosaves") t)
(make-directory (concat dotfiles-tmp-dir "backups") t)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/tmp/
(setq auto-save-file-name-transforms `((".*" ,(concat dotfiles-tmp-dir "autosaves/\\1") t)))
(setq backup-directory-alist `((".*" . ,(concat dotfiles-tmp-dir "backups"))))
(setq auto-save-list-file-name (concat dotfiles-tmp-dir "autosaves/autosave-list"))

;;When you visit a file, point goes to the last place where it was when you previously visited
;;Save file is set to dotfiles-tmp-dir/places
(require 'saveplace)
(setq-default save-place t)

;;enable winner mode for C-c-(<left>|<right>) to navigate the history
;;of buffer changes i.e. undo a split screen
(when (fboundp 'winner-mode)
      (winner-mode 1))

(setq redisplay-dont-pause t
      echo-keystrokes 0.02
      color-theme-is-global t
      shift-select-mode nil
      mouse-yank-at-point nil
      delete-by-moving-to-trash nil
      uniquify-buffer-name-style 'forward
      ediff-window-setup-function 'ediff-setup-windows-plain
      xterm-mouse-mode t
      save-place-file (concat dotfiles-tmp-dir "places")
      delete-active-region nil
      )

(set-default 'indent-tabs-mode nil)
(auto-compression-mode t)
(show-paren-mode 1)

(setq confirm-nonexistent-file-or-buffer nil)

;;remove all trailing whitespace and trailing blank lines before saving the file
;;(add-hook 'before-save-hook 'whitespace-cleanup)

;; The amazing undo tree
;;(add-lib-path "undo-tree")
;;(require 'undo-tree)
;;(global-undo-tree-mode)

;; momentarily highlight changes made by commands such as undo, yank-pop, etc.
(add-lib-path "volatile-highlights")
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; org-mode
(require 'org-install)
;(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/Dropbox/org/testi.org"
                             "~/Dropbox/org/work.org"))

;; highlight expression on eval
(require 'highlight)
(require 'eval-sexp-fu)
(setq eval-sexp-fu-flash-duration 0.5)

;; ido-mode
;(require 'ido)
;(ido-mode t)


;; R
;; sudo apt-get install ess
(add-to-list 'load-path "/usr/share/emacs24/site-lisp/ess")
(require 'ess-site)
(define-key ess-mode-map "_" 'self-insert-command)
(define-key inferior-ess-mode-map "_" 'self-insert-command)
;; alternative to options(width=999)
(defun my-ess-post-run-hook ()
  (ess-execute-screen-options)
  (local-set-key "\C-cw" 'ess-execute-screen-options))
(add-hook 'ess-post-run-hook 'my-ess-post-run-hook)

;; YAML
(load-file (concat dotfiles-lib-dir "yaml-mode.el"))
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Scala
(require 'scala-mode)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))

;; minimap
(require 'minimap)

;; Pig Latin
(unless (package-installed-p 'pig-mode)
  (package-refresh-contents)
  (package-install 'pig-mode))
(require 'pig-mode)
(setq pig-indent-level 2)
