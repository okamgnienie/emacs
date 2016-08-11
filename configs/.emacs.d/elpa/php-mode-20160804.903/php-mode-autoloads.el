;;; php-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "php-mode" "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-mode.el"
;;;;;;  "b69c8197aefc0df15fcee55864f344d1")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-mode.el

(let ((loads (get 'php 'custom-loads))) (if (member '"php-mode" loads) nil (put 'php 'custom-loads (cons '"php-mode" loads))))

(defvar php-extra-constants 'nil "\
A list of additional strings to treat as PHP constants.")

(custom-autoload 'php-extra-constants "php-mode" nil)

(add-to-list 'interpreter-mode-alist (cons "php" 'php-mode))

(autoload 'php-mode "php-mode" "\
Major mode for editing PHP code.

\\{php-mode-map}

\(fn)" t nil)

(dolist (pattern '("\\.php[s345t]?\\'" "\\.phtml\\'" "/Amkfile\\'" "\\.amk\\'")) (add-to-list 'auto-mode-alist `(,pattern . php-mode) t))

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-array.el"
;;;;;;  "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-classobj.el"
;;;;;;  "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-control-structures.el"
;;;;;;  "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-crack.el"
;;;;;;  "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-dio.el"
;;;;;;  "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-dom.el"
;;;;;;  "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-exceptions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-exif.el"
;;;;;;  "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-ext.el"
;;;;;;  "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-filesystem.el"
;;;;;;  "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-gd.el"
;;;;;;  "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-math.el"
;;;;;;  "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-mode-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-mode-pkg.el"
;;;;;;  "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-mode.el"
;;;;;;  "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-pcre.el"
;;;;;;  "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-regex.el"
;;;;;;  "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-simplexml.el"
;;;;;;  "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-strings.el"
;;;;;;  "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-var.el"
;;;;;;  "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-xmlparser.el"
;;;;;;  "../../../../../../.emacs.d/elpa/php-mode-20160804.903/php-xmlreader.el")
;;;;;;  (22444 33275 809337 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; php-mode-autoloads.el ends here
