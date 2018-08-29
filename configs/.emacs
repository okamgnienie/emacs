
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     PACKAGES SETTINGS AND AUTOINSTALL                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

(defun ensure-package-installed (&rest packages)
  "Ensure package is installed and if so, require it."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
				 (require package)
       (package-install package)))
   packages))

;; Load local packages
(add-to-list 'load-path
             "~/.emacs.d/elisp")

;; Make sure to have downloaded archive description,
;; or use package-archive-contents
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed 'atom-one-dark-theme
                          'redo+
                          'expand-region
                          'smex
                          'ido
                          'yasnippet
                          'js-doc
                          'js2-mode
                          'ac-js2
                          'projectile
                          'smartparens
                          'auto-complete
                          'multiple-cursors
                          'magit
                          'nyan-mode
                          'hlinum
                          'syntax-subword
                          'less-css-mode
                          'undo-tree
                          'helm
                          'scss-mode
                          'god-mode
                          'use-package
                          'avy
                          'markdown-mode
                          'smart-forward
                          'smooth-scroll
                          'column-enforce-mode
                          'web-mode
                          'yaml-mode
                          'clojure-mode
                          'anaconda-mode
                          'parinfer
                          'flycheck
                          'json-mode
                          'coffee-mode
                          'ztree
                          'typescript-mode
                          'ng2-mode
                          'tide
                          'company
                          'string-utils)

;; activate installed packages
(package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                GENERAL CONFIG                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove all the content from the window title:
(setq frame-title-format "")

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
(setq web-mode-markup-indent-offset 2)

;; Set CoffeeScript indent:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-background t)
 '(avy-keys
   (quote
    (97 115 100 102 103 104 106 107 108 113 119 101 114 116 121 117 105 111 112 122 120 99 118 98 110 109)))
 '(coffee-tab-width 2)
 '(flycheck-temp-prefix ".flycheck")
 '(global-flycheck-mode t)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(js-doc-description-line " * @desc
")
 '(js-indent-level 2)
 '(js2-global-externs
   (quote
    ("module" "require" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "console" "JSON" "angular" "toastr" "$" "moment" "_")))
 '(js2-highlight-level 3)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(linum-format " %2d ")
 '(mode-line-format
   (quote
    ("%e "
     (:eval
      (cond
       ((buffer-modified-p)
        (propertize "✖ modified"
                    (quote face)
                    (quote error)))
       (t
        (propertize "✔ saved"
                    (quote face)
                    (quote success)))))
     " • "
     (:propertize "%02l" face
                  ((t
                    (:foreground "#C678DD" :weight normal))))
     ","
     (:propertize "%02c" face
                  ((t
                    (:foreground "#C678DD" :weight normal))))
     " • " (:eval (list (nyan-create)))
     " • " mode-name " • "
     (:propertize mode-line-buffer-identification face
                  ((t
                    (:foreground "#61AFEF" :weight normal))))
     mode-line-end-spaces)))
 '(nyan-bar-length 16)
 '(nyan-mode t)
 '(package-selected-packages
   (quote
    (company tide ng2-mode typescript-mode ztree coffee-mode json-mode helm-flycheck exec-path-from-shell flycheck parinfer multiple-cursors smart-forward expand-region php-mode yasnippet yaml-mode xkcd web-mode use-package undo-tree syntax-subword smooth-scroll smex smartparens scss-mode redo+ nyan-mode move-text markdown-mode magit less-css-mode js-doc jedi ido-ubiquitous hlinum helm-projectile god-mode csv-mode column-enforce-mode clojure-mode avy atom-one-dark-theme atom-dark-theme anaconda-mode ac-js2)))
 '(show-smartparens-global-mode t)
 '(size-indication-mode t)
 '(tab-width 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 4)
 '(web-mode-enable-current-element-highlight t)
 '(web-mode-markup-indent-offset 2))

;; Set default PHP indent:
(add-hook 'php-mode-hook 'php-indent)
(defun php-indent ()
  (setq indent-tabs-mode nil
        tab-width 2
        c-basic-offset 2))

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

;; Use anaconda-mode by default for python editing:
(add-hook 'python-mode-hook 'anaconda-mode)

;; Set font to Adobe Source Code Pro:
;; (add-to-list 'default-frame-alist '(font .  "Source Code Pro-13" ))
;; (set-face-attribute 'default t :font  "Source Code Pro-13")

;; Set helm to be opened in current frame:
(setq helm-split-window-default-side 'same)

;; Open JavaScript files with js2-mode:
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Open reac jsx files with web-mode:
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; Open styles and templates with web-mode:
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; Open Angular files using ng2-mode:
(setq auto-mode-alist
      (append '(("\\.component.ts\\'" . ng2-mode)
                ("\\.component.html\\'" . ng2-mode)
                ("\\.container.ts\\'" . ng2-mode)
                ("\\.container.html\\'" . ng2-mode)
                ("\\.effect.ts\\'" . ng2-mode)
                ("\\.effects.ts\\'" . ng2-mode)
                ("\\.service.ts\\'" . ng2-mode)
                ("\\.module.ts\\'" . ng2-mode))
              auto-mode-alist))

;; Open CSV files with csv-mode:
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;; Set cursor color:
(set-cursor-color "#528BFF")

(define-fringe-bitmap 'flycheck-fringe-indicator
  (vector #b00111000
          #b00111000
          #b00111000
          #b00111000
          #b00111000
          #b00111000
          #b00111000
          #b00111000
          #b00111000
          #b00111000
          #b00111000
          #b00111000
          #b00111000
          #b00111000
          #b00111000
          #b00111000
          #b00111000))

(flycheck-define-error-level 'error
  :severity 100
  :compilation-level 2
  :overlay-category 'flycheck-error-overlay
  :fringe-bitmap 'flycheck-fringe-indicator
  :fringe-face 'flycheck-fringe-error
  :error-list-face 'flycheck-error-list-error)

(flycheck-define-error-level 'warning
  :severity 10
  :compilation-level 1
  :overlay-category 'flycheck-warning-overlay
  :fringe-bitmap 'flycheck-fringe-indicator
  :fringe-face 'flycheck-fringe-warning
  :error-list-face 'flycheck-error-list-warning)

(flycheck-define-error-level 'info
  :severity -10
  :compilation-level 0
  :overlay-category 'flycheck-info-overlay
  :fringe-bitmap 'flycheck-fringe-indicator
  :fringe-face 'flycheck-fringe-info
  :error-list-face 'flycheck-error-list-info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  FLYCHECK                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn on flychecking globally:
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Disable jshint
(setq-default flycheck-disabled-checkers
							(append flycheck-disabled-checkers
											'(javascript-jshint)))

;; Use eslint with web-mode for jsx files:
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; Customize flycheck temp file prefix:
(setq-default flycheck-temp-prefix ".flycheck")

;; Disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
							(append flycheck-disabled-checkers
											'(json-jsonlist)))

;; This hopefully sets up path and other vars better:
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             TYPESCRIPT & TIDE                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(add-hook 'typescript-mode-hook #'setup-tide-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   AUTORUN                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parentheses related config:
(smartparens-global-mode)

;; Automatically activate autocomplete:
(ac-config-default)

;; Run Interactively Do Things:
(ido-mode t)

;; Run projectile on startup:
(projectile-mode)

;; Run smex:
(smex-initialize)

;; Activate highlighting current line on linum:
(hlinum-activate)

;; Activate camelCase sense:
(global-subword-mode)

;; Activate Undo Tree:
(global-undo-tree-mode)

;; Activate smooth scrolling:
(smooth-scroll-mode t)

;; Scroll line by line:
(setq scroll-step            1
      scroll-conservatively  10000)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           ADDITIONAL KEYBINDINGS                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Comments:
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)
(define-key web-mode-map (kbd "M-;") 'comment-or-uncomment-region-or-line)

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

;; Fix the expand region causing mark mode problems:
(setq shift-select-mode nil)

;; Use Ctrl+n and Ctrl+p to select item from the autocomplete list:
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; Avy keybindings:
(define-key global-map (kbd "C-c SPC") 'avy-goto-word-1)
(define-key global-map (kbd "C-c c") 'avy-goto-char)
(define-key global-map (kbd "C-c l") 'avy-goto-line)

;; Avy keybindings for god-mode:
(define-key global-map (kbd "C-c C-SPC") 'avy-goto-word-1)
(define-key global-map (kbd "C-c C-c") 'avy-goto-char)
(define-key global-map (kbd "C-c C-l") 'avy-goto-line)

;; Tide - jump to definition:
(define-key tide-mode-map (kbd "C-c j") 'tide-jump-to-definition)

;; ng2-mode keybindings:
(define-key global-map (kbd "C-c C-c") 'ng2-open-counterpart)
(define-key ng2-html-map (kbd "C-c j") 'ng2-html-goto-binding)

;; Tide - rename symbol:
(define-key tide-mode-map (kbd "C-c r") 'tide-rename-symbol)

;; String-utils - cycle:
(global-set-key (kbd "C-c C-s") 'string-inflection-all-cycle)

;; Smex keybindings:
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Magit:
(global-set-key (kbd "C-c m") 'magit-status)

;; God mode:
(global-set-key (kbd "M-g") 'god-mode-all)

;; Recursive grep:
(global-set-key (kbd "M-r") 'rgrep)

;; Comment JavaScript function:
(define-key js2-mode-map (kbd "C-c i") 'js-doc-insert-function-doc)

;; Search project files (helm):
(global-set-key (kbd "C-l") 'helm-projectile)

;; Search buffers (helm):
(global-set-key (kbd "M-l") 'helm-buffers-list)

;; Search commands (helm):
(global-set-key (kbd "M-c") 'helm-M-x)

;; Smart forward:
(global-set-key (kbd "M-<up>") 'smart-up)
(global-set-key (kbd "M-<down>") 'smart-down)
(global-set-key (kbd "M-<left>") 'smart-backward)
(global-set-key (kbd "M-<right>") 'smart-forward)

;; Remove keybindings:
(global-set-key (kbd "M-]") nil)
(global-set-key (kbd "M-c") nil)

;; Quick shortcut for Magit:
(global-set-key (kbd "M-m") 'magit-status)

;; Bind better scrolling functions:
(global-set-key (kbd "C-v") 'scroll-up-one-fifth)
(global-set-key (kbd "M-v") 'scroll-down-one-fifth)

;; Move region up/down:
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;; Goto line:
(global-set-key (kbd "<up>") 'goto-line)

;; Count lines, words and characters:
(global-set-key (kbd "<down>") 'count-words)

;; Rotate window buffers (tmux like):
(global-set-key (kbd "C-x C-o") 'rotate-window-buffers)

;; Omit merge commits in magit log:
(magit-define-popup-switch 'magit-log-popup
  ?m "Omit merge commits" "--no-merges")

;; Show all commits in the history of branches etc:
(magit-define-popup-switch 'magit-log-popup
  ?m "Show all" "--all")

;; Override the author date:
(magit-define-popup-option 'magit-commit-popup
  ?D "Override the author date" "--date=" #'read-from-minibuffer)

;; Time range:
(autoload 'org-read-date "org")

(defun magit-org-read-date (prompt &optional _default)
  (org-read-date 'with-time nil nil prompt))

(magit-define-popup-option 'magit-log-popup
  ?s "Since date" "--since=" #'magit-org-read-date)

(magit-define-popup-option 'magit-log-popup
  ?u "Until date" "--until=" #'magit-org-read-date)

;; Company autocomplete:
(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)


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

;; Set magit status to open in the current window:
(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer (if (and (derived-mode-p 'magit-mode)
                         (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-diff-mode
                                 magit-stash-mode
                                 magit-status-mode)))
                    nil
                  '(display-buffer-same-window)))))

;; Hide ^M marks:
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; Better scrolling functions:
(defun window-one-fifth-height ()
  (max 1 (/ (1- (window-height (selected-window))) 5)))

(defun scroll-up-one-fifth ()
  (interactive)
  (scroll-up (window-one-fifth-height)))

(defun scroll-down-one-fifth ()
  (interactive)
  (scroll-down (window-one-fifth-height)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not '(lambda (x)
                                (or (buffer-file-name x)
                                    (eq 'dired-mode (buffer-local-value 'major-mode x))))
                             (buffer-list)))))

(defun rotate-window-buffers ()
  (interactive)
  (let* ((windows (window-list))
         (buffers (mapcar #'window-buffer windows))
         (wpoints (mapcar #'window-point windows))
         (w (pop windows)))
    (setq windows (append windows `(,w)))
    (mapc (lambda(w)
            (let ((b (pop buffers))
                  (p (pop wpoints)))
              (set-window-buffer w b)
              (set-window-point w p)))
          windows)))


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
 '(flycheck-error ((t (:underline "#E06C75"))))
 '(flycheck-error-list-error ((t (:inherit error))))
 '(flycheck-info ((t (:underline "#528BFF"))))
 '(flycheck-warning ((t (:underline "#E5C07B"))))
 '(helm-match ((t (:foreground "#E06C75" :weight bold))))
 '(ido-first-match ((t (:foreground "#E06C75" :weight bold))))
 '(ido-only-match ((t (:foreground "#528BFF" :weight bold))))
 '(ido-subdir ((t (:foreground "#E5C07B"))))
 '(js2-error ((t (:underline "Red1"))))
 '(linum ((t (:stipple nil :background "#282C34" :distant-foreground "#5C6370" :foreground "#5C6370" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal))))
 '(linum-highlight-face ((t (:stipple nil :background "#2F343D" :distant-foreground "#5C6370" :foreground "#528BFF" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold))))
 '(mode-line ((t (:background "#323232" :foreground "#AAAAAA"))))
 '(mode-line-inactive ((t (:background "#444444" :foreground "#AAAAAA"))))
 '(sp-show-pair-match-face ((t (:foreground "#528BFF" :weight normal))))
 '(sp-show-pair-mismatch-face ((t (:foreground "#E06C75" :weight normal))))
 '(web-mode-current-element-highlight-face ((t (:inherit web-mode-variable-name-face)))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
