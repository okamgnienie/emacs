
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
(require 'nyan-mode)
(require 'hlinum)
(require 'syntax-subword)
(require 'less-css-mode)
(require 'undo-tree)
(require 'helm)
(require 'scss-mode)
(require 'god-mode)
(require 'use-package)
(require 'avy)
(require 'markdown-mode)


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
(setq linum-format " %2d ")

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

;; Configure backups:
(setq backup-directory-alist `(("." . "~/.Saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Set default font size:
(set-face-attribute 'default nil :height 140)

;; Remove trailing whitespaces on save:
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Change cursor and linum color in god mode:
(add-hook 'god-mode-enabled-hook 'activate-yellow-cursor)
(add-hook 'god-mode-disabled-hook 'activate-blue-cursor)

;; Set font to Adobe Source Code Pro:
;; (add-to-list 'default-frame-alist '(font .  "Source Code Pro-13" ))
;; (set-face-attribute 'default t :font  "Source Code Pro-13")


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

;; Activate highlighting current line on linum:
(hlinum-activate)

;; Activate camelCase sense:
(global-subword-mode)

;; Activate Undo Tree:
(global-undo-tree-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           ADDITIONAL KEYBINDINGS                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Comments:
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)

;; Mark previous/next lines like this:
(bind-key* "M-." 'mc/mark-next-like-this)
(bind-key* "M-," 'mc/mark-previous-like-this)

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

;; Avy keybindings:
(define-key global-map (kbd "C-c SPC") 'avy-goto-char-2)
(define-key global-map (kbd "C-c l") 'avy-goto-line)
(define-key global-map (kbd "C-c w") 'avy-goto-word-1)
(define-key global-map (kbd "C-c c") 'avy-goto-char)

;; Smex keybindings:
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Magit:
(global-set-key (kbd "C-c m") 'magit-status)

;; God mode:
(global-set-key (kbd "M-g") 'god-mode-all)


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

(defun activate-yellow-cursor ()
  (set-face-foreground 'linum-highlight-face "#FFDD33")
  (set-cursor-color "#FFDD33"))


(defun activate-blue-cursor ()
  (set-face-foreground 'linum-highlight-face "#528BFF")
  (set-cursor-color "#528BFF"))

;; Change bell to mode-line highlight:
(defun my-terminal-visible-bell ()
   "A friendlier visual bell effect."
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line))

 (setq visible-bell nil
       ring-bell-function 'my-terminal-visible-bell)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   ALIASES                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use y/n instead of yes/no when ask for save etc. in the minibuffer:
(defalias 'yes-or-no-p 'y-or-n-p)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                CUSTOM THEMING                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-jump-face-background ((t (:background nil :foreground "#5C6370" :inverse-video nil))))
 '(ace-jump-face-foreground ((t (:background nil :foreground "#FFDD33" :inverse-video nil :weight bold))))
 '(avy-lead-face ((t (:foreground "#FFDD33" :weight normal))))
 '(avy-lead-face-0 ((t (:foreground "#E06C75" :weight normal))))
 '(avy-lead-face-1 ((t (:foreground "#7FFF00" :weight normal))))
 '(avy-lead-face-2 ((t (:foreground "#56B6C2" :weight normal))))
 '(ido-first-match ((t (:foreground "#E06C75" :weight bold))))
 '(ido-only-match ((t (:foreground "#528BFF" :weight bold))))
 '(ido-subdir ((t (:foreground "#E5C07B"))))
 '(linum ((t (:stipple nil :background "#282C34" :distant-foreground "#5C6370" :foreground "#5C6370" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal))))
 '(linum-highlight-face ((t (:stipple nil :background "#282C34" :distant-foreground "#5C6370" :foreground "#528BFF" :weight bold :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-background t)
 '(avy-keys
   (quote
    (97 115 100 102 103 104 106 107 108 113 119 101 114 116 121 117 105 111 112 122 120 99 118 98 110 109))))
