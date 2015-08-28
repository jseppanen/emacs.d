(setq inhibit-splash-screen t
      inhibit-startup-message t)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(column-number-mode t)
(transient-mark-mode t)

(require 'rainbow-delimiters)
;(global-rainbow-delimiters-mode t)

(blink-cursor-mode 0)

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

; Fix "<dead-tilde> is undefined" on Ubuntu 14.04
(require 'iso-transl)

(setq fill-column 80)
(setq current-fill-column 80)

; FIXME mode: highlight all FIXME tags on screen
(setq fixme-modes '(c-mode c++-mode java-mode python-mode matlab-mode emacs-lisp-mode scheme-mode ess-mode))
(make-face 'font-lock-fixme-face)
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(FIXME\\)" 1 'font-lock-fixme-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "Red" "Yellow" nil t nil t nil nil)

;; prevent accidental exits
(add-to-list 'kill-emacs-query-functions '(lambda () (yes-or-no-p "Exit for sure? ")))

;; ;;Color-theme
(add-lib-path "color-theme")
(require 'color-theme)

(load-file (concat user-emacs-directory "lib/blackbored.el"))
(color-theme-blackbored)

;;make sure ansi colour character escapes are honoured
(ansi-color-for-comint-mode-on)

;;enable winner mode for C-c-(<left>|<right>) to navigate the history
;;of buffer changes i.e. undo a split screen
;; (when (fboundp 'winner-mode)
;;       (winner-mode 1))

(setq redisplay-dont-pause t
      echo-keystrokes 0.02
      color-theme-is-global t
      shift-select-mode nil
      mouse-yank-at-point nil
      delete-by-moving-to-trash nil
      uniquify-buffer-name-style 'forward
      ediff-window-setup-function 'ediff-setup-windows-plain
      xterm-mouse-mode t
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
;(add-lib-path "volatile-highlights")
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; highlight expression on eval
(require 'highlight)
(require 'eval-sexp-fu)
(setq eval-sexp-fu-flash-duration 0.5)

;; full path in title bar: useful for ulogme
(setq-default frame-title-format "%b (%f) - Emacs")
