
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                RECOMPILE DIRECTORY AND ALL FILES IF NECESSARY              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               PACKAGES SETTINGS                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 REQUIREMENTS                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(require 'magit)
(require 'highlight-parentheses)
(require 'yasnippet)
(require 'js2-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                GENERAL CONFIG                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load theme:
(load-theme 'atom-one-dark t)

;; Enable line numbers on the left windows side:
(global-linum-mode t)

;; Disable emacs top menu bar:
(menu-bar-mode -1)

;; Disable emacs top toolbar:
(tool-bar-mode -1)

;; Disable side scrollbars:
(scroll-bar-mode -1)

;; Setup format of the line numering:
(setq linum-format " %2d  ")

;; Disable initial splash screen:
(setq inhibit-splash-screen t)

;; Disable text wrapping:
(setq-default truncate-lines 1)

;; Load theme:
(column-number-mode)

;; Yasnippet config:
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(yas-global-mode 1)

;; Set tab indent:
(setq-default indent-tabs-mode nil)
(setq standard-indent 2)
(setq js-indent-level 2)
(setq js2-indent-level 2)
(setq js2-basic-offset 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   AUTORUN                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Brackets related config:
(smartparens-global-mode)
(global-highlight-parentheses-mode)

;; Automatically activate autocomplete:
(ac-config-default)

;; Run Interactively Do Things:
(ido-mode t)

;; Run projectile on startup:
(projectile-global-mode)

;; Run smex:
(smex-initialize)

;; Automatically activate js-mode for JSON files:
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           ADDITIONAL KEYBINDINGS                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Selecting text region:
(global-set-key (kbd "M-s") 'er/expand-region)

;; Use Ctrl+n and Ctrl+p to select item from the autocomplete list:
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; Ace Jump keybindings:
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-c l") 'ace-jump-line-mode)
(define-key global-map (kbd "C-c w") 'ace-jump-word-mode)

;; Smex keybindings:
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            ADDITIONAL FUNCTIONS                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Function for creating comments when cursor is on the begining or end of line:
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   ALIASES                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use y/n instead of yes/no when ask for save etc. in the minibuffer:
(defalias 'yes-or-no-p 'y-or-n-p)
