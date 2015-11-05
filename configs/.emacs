;; --------------- RECOMPILE DIRECTORY IF NECESSARY --------------
(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;; -------------------- MELPA PACKAGES CONFIG --------------------
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ac-auto-show-menu 0.5)
;;  '(custom-safe-themes (quote ("05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" "3ed645b3c08080a43a2a15e5768b893c27f6a02ca3282576e3bc09f3d9fa3aaa" default)))
;;  '(global-auto-complete-mode t)
;;  '(org-todo-keyword-faces (quote (("TODO" . "wheat3") ("WAITING" . "gold") ("CANCELED" . "dim gray") ("IN PROGRESS" . "light sky blue"))))
;;  '(org-todo-keywords (quote ((sequence "TODO" "IN PROGRESS" "WAITING" "CANCELED" "DONE"))))
;;  '(smartparens-global-mode t))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(cursor ((t (:background "brightyellow" :foreground "black"))))
;;  '(font-lock-comment-delimiter-face ((t (:foreground "#CC8C3C"))))
;;  '(font-lock-comment-face ((t (:foreground "#CC8C3C"))))
;;  '(font-lock-function-name-face ((t (:foreground "#ff6644"))))
;;  '(font-lock-keyword-face ((t (:foreground "#ffdd33"))))
;;  '(font-lock-negation-char-face ((t (:foreground "IndianRed1"))))
;;  '(font-lock-type-face ((t (:foreground "white"))))
;;  '(font-lock-variable-name-face ((t (:inherit default))))
;;  '(guide-key/key-face ((t (:inherit font-lock-warning-face))) t)
;;  '(guide-key/prefix-command-face ((t (:inherit font-lock-keyword-face :background "green"))) t)
;;  '(header-line ((t (:inherit mode-line :inverse-video nil :underline t))))
;;  '(ido-first-match ((t (:foreground "brightgreen" :weight bold))))
;;  '(ido-only-match ((t (:foreground "brightgreen"))))
;;  '(ido-subdir ((t (:foreground "LightSalmon3"))))
;;  '(js2-private-member ((t (:foreground "brightyellow"))))
;;  '(js2-warning ((t (:underline "orange"))))
;;  '(linum ((t (:background "color-234" :foreground "color-237" :underline nil :weight normal))))
;;  '(linum-relative-current-face ((t nil)))
;;  '(minibuffer-prompt ((t (:foreground "SkyBlue3"))))
;;  '(mode-line ((t (:background "grey30" :foreground "brightwhite" :box (:line-width 1 :style released-button)))))
;;  '(mode-line-inactive ((t (:inherit mode-line :background "grey30" :foreground "grey80" :box (:line-width -1 :color "grey40") :weight light))))
;;  '(nobreak-space ((t (:inherit escape-glyph :underline t))))
;;  '(org-agenda-done ((t (:foreground "brightgreen"))) t)
;;  '(org-done ((t (:foreground "brightgreen" :weight bold))) t)
;;  '(org-level-1 ((t (:foreground "#CF4647"))) t)
;;  '(org-level-2 ((t (:foreground "white"))) t)
;;  '(org-level-3 ((t (:foreground "dim gray"))) t)
;;  '(org-todo ((t (:foreground "aquamarine3" :weight bold))) t)
;;  '(region ((t (:background "cyan" :foreground "brightwhite"))))
;;  '(speedbar-directory-face ((t (:inherit font-lock-keyword-face))) t)
;;  '(speedbar-selected-face ((t (:background "#4182C4" :foreground "#FFFFFF"))) t)
;;  '(widget-field ((t (:background "color-243" :foreground "black")))))

;; -------------------- REQUIREMENTS --------------------
(require 'redo+)
(require 'expand-region)
(require 'ace-jump-mode)
(require 'smex)
(require 'ido)
(require 'yasnippet)
(require 'js-doc)
(require 'js2-mode)
(require 'ac-js2)
(require 'projectile)
(require 'smartparens)
(require 'auto-complete)
(require 'multiple-cursors)
;; (require 'linum-relative) ;; ???

;; -------------------- GENERAL CONFIG --------------------
;; Enable xclip:
;; (xclip-mode 1)

;; Run Interactively Do Things:
(ido-mode t)

;; Run projectile on startup:
(projectile-global-mode)

;; Disable emacs top menu bar:
(menu-bar-mode -1)

;; Disable side scrollbars:
(scroll-bar-mode -1)

;; Enable line numbers on the left windows side:
(global-linum-mode t)

;; Disable initial splash screen
(setq inhibit-splash-screen t)

;; Load theme:
(load-theme 'atom-one-dark t)

;; Setup format of the line numering:
(setq linum-format " %2d  ")

;; Disable text wrapping:
(setq-default truncate-lines 1)

;; Automatically activate js-mode for JSON files:
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Automatically activate autocomplete:
(ac-config-default)

;; Function for creating comments when cursor is on the beginin or end of line:
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;; -------------------- KEYBOARD SHORTCUTS --------------------
;; Comments:
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)

;; Mark previous/next lines like this:
(global-set-key (kbd "M-.") 'mc/mark-next-like-this)
(global-set-key (kbd "M-,") 'mc/mark-previous-like-this)

;; Cut selected region:
(global-set-key (kbd "C-x C-k") 'kill-region)

;; Use arrow keys for the history:
(global-set-key (kbd "<left>") 'undo)
(global-set-key (kbd "<right>") 'redo)

(global-set-key (kbd "M-s") 'er/expand-region)

;; Use Ctrl+n and Ctrl+p to select item from the autocomplete list:
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; Bind ace jump schortcut:
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Setup smex
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; (add-to-list 'load-path
;; 	     "~/.emacs.d/plugins/yasnippet")

;; (yas-global-mode 1)

(require 'smartparens-config)
(smartparens-mode t)


;; (global-set-key (kbd "C-c i") 'js-doc-insert-function-doc)

;; Use y/n instead of yes/no when ask for save etc. in the minibuffer:
(defalias 'yes-or-no-p 'y-or-n-p)
