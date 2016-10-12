;;; expand-region-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "expand-region" "../../../../../../.emacs.d/elpa/expand-region-20160729.131/expand-region.el"
;;;;;;  "8ab9f19242b201916e5e818d59b32661")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/expand-region-20160729.131/expand-region.el

(autoload 'er/expand-region "expand-region" "\
Increase selected region by semantic units.

With prefix argument expands the region that many times.
If prefix argument is negative calls `er/contract-region'.
If prefix argument is 0 it resets point and mark to their state
before calling `er/expand-region' for the first time.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "expand-region-custom" "../../../../../../.emacs.d/elpa/expand-region-20160729.131/expand-region-custom.el"
;;;;;;  "65696caf6c61d5bb31444a192c5119b5")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/expand-region-20160729.131/expand-region-custom.el

(let ((loads (get 'expand-region 'custom-loads))) (if (member '"expand-region-custom" loads) nil (put 'expand-region 'custom-loads (cons '"expand-region-custom" loads))))

(defvar expand-region-preferred-python-mode 'python "\
The name of your preferred python mode")

(custom-autoload 'expand-region-preferred-python-mode "expand-region-custom" t)

(defvar expand-region-guess-python-mode t "\
If expand-region should attempt to guess your preferred python mode")

(custom-autoload 'expand-region-guess-python-mode "expand-region-custom" t)

(defvar expand-region-autocopy-register "" "\
If set to a string of a single character (try \"e\"), then the
contents of the most recent expand or contract command will
always be copied to the register named after that character.")

(custom-autoload 'expand-region-autocopy-register "expand-region-custom" t)

(defvar expand-region-skip-whitespace t "\
If expand-region should skip past whitespace on initial expansion")

(custom-autoload 'expand-region-skip-whitespace "expand-region-custom" t)

(defvar expand-region-fast-keys-enabled t "\
If expand-region should bind fast keys after initial expand/contract")

(custom-autoload 'expand-region-fast-keys-enabled "expand-region-custom" t)

(defvar expand-region-contract-fast-key "-" "\
Key to use after an initial expand/contract to contract once more.")

(custom-autoload 'expand-region-contract-fast-key "expand-region-custom" t)

(defvar expand-region-reset-fast-key "0" "\
Key to use after an initial expand/contract to undo.")

(custom-autoload 'expand-region-reset-fast-key "expand-region-custom" t)

(defvar expand-region-exclude-text-mode-expansions '(html-mode nxml-mode) "\
List of modes which derive from `text-mode' for which text mode expansions are not appropriate.")

(custom-autoload 'expand-region-exclude-text-mode-expansions "expand-region-custom" t)

(defvar expand-region-smart-cursor nil "\
Defines whether the cursor should be placed intelligently after expansion.

If set to t, and the cursor is already at the beginning of the new region,
keep it there; otherwise, put it at the end of the region.

If set to nil, always place the cursor at the beginning of the region.")

(custom-autoload 'expand-region-smart-cursor "expand-region-custom" t)

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/elpa/expand-region-20160729.131/cc-mode-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/clojure-mode-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/cperl-mode-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/css-mode-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/enh-ruby-mode-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/er-basic-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/erlang-mode-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/expand-region-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/expand-region-core.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/expand-region-custom.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/expand-region-pkg.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/expand-region.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/feature-mode-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/html-mode-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/js-mode-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/js2-mode-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/jsp-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/latex-mode-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/nxml-mode-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/octave-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/python-el-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/python-el-fgallina-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/python-mode-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/ruby-mode-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/sml-mode-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/subword-mode-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/text-mode-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/the-org-mode-expansions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/expand-region-20160729.131/web-mode-expansions.el")
;;;;;;  (22526 9923 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; expand-region-autoloads.el ends here
