;;; eucalypt-mode.el --- Major mode for Eucalypt code

;; -*- lexical-binding: t; -*-
;;
;; Copyright © 2019 Greg Hawkins

(defgroup eucalypt nil
  "Major mode for editing Eucalypt native syntax files."
  :prefix "eucalypt-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/curvelogic/eucalypt-mode.el"))

(defcustom eucalypt-eu-command
  "eu"
  "The command used to execute eucalypt command line program."
  :type 'string
  :group 'eucalypt)

(defcustom eucalypt-eu-global-opts
  ""
  "Default options to pass to `eu'"
  :type 'string
  :group 'eucalypt)

(defvar eucalypt-mode-hook nil)

(defvar eucalypt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-k" 'eucalypt-render-buffer)
    map)
  "Keymap for Eucalypt major mode")

(defvar eucalypt-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?=  "_" table)
    (modify-syntax-entry ?∧  "_" table)
    (modify-syntax-entry ?∨  "_" table)
    (modify-syntax-entry ?∘  "_" table)
    (modify-syntax-entry ?`  "." table)
    (modify-syntax-entry ?:  "." table)
    (modify-syntax-entry ?,  "." table)
    (modify-syntax-entry ?_  "w" table)
    (modify-syntax-entry ?•  "w" table)
    (modify-syntax-entry ?-  "w" table)
    (modify-syntax-entry ?!  "w" table)
    (modify-syntax-entry ??  "w" table)
    (modify-syntax-entry ?.  "." table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\r ">" table)
    table)
  "Syntax table to use in eucalypt mode buffers.")

;; generate with:
;; eu prelude=resource:prelude -x text -e 'prelude keys map(str.of) filter(str.matches?("[\w-?]+")) map("{ch.dq}{}{ch.dq}")'
(defvar eucalypt--prelude-names (regexp-opt '("eu" "io" "panic"
  "assert" "null" "true" "false" "cat" "if" "then" "when" "cons"
  "head" "nil?" "head-or" "tail" "tail-or" "nil" "first" "second"
  "second-or" "sym" "merge" "deep-merge" "elements" "block" "has"
  "lookup" "lookup-in" "lookup-or" "lookup-or-in" "lookup-alts" "not"
  "and" "or" "-" "inc" "dec" "negate" "zero?" "pos?" "neg?" "num"
  "floor" "ceiling" "max" "max-of" "min" "min-of" "ch" "str"
  "identity" "const" "->" "compose" "apply" "flip" "complement"
  "curry" "uncurry" "cond" "juxt" "with-meta" "meta" "assertions"
  "//=?" "//!?" "take" "drop" "take-while" "take-until" "drop-while"
  "drop-until" "nth" "repeat" "foldl" "foldr" "scanl" "scanr"
  "iterate" "ints-from" "range" "count" "last" "cycle" "map" "map2"
  "zip-with" "zip" "filter" "remove" "append" "prepend" "concat"
  "mapcat" "zip-apply" "reverse" "all-true?" "all" "any-true?" "any"
  "window" "partition" "over-sliding-pairs" "differences" "merge-all"
  "key" "value" "keys" "values" "bimap" "map-first" "map-second"
  "map-kv" "map-as-block" "pair" "zip-kv" "with-keys" "map-values"
  "map-keys" "filter-items" "by-key" "by-key-name" "by-key-match"
  "by-value" "match-filter-values" "filter-values" "_block"
  "alter-value" "update-value" "alter" "update" "update-value-or"
  "set-value" "tongue" "merge-at" "cal" "iosm") 'symbols))

(defvar eucalypt-font-lock-keywords
  `(;; declaration metadata lead-in
    ("`" . font-lock-preprocessor-face)
    ;; binary op declaration
    ("([[:space:]]*\\sw+[[:space:]]*\\(\\s_+\\)[[:space:]]*\\sw+[[:space:]]*)[[:space:]]*:[[:space:]]" 1 font-lock-function-name-face)
    ;; unary prefix op declaration
    ("([[:space:]]*\\(\\s_+\\)[[:space:]]*\\sw+[[:space:]]*)[[:space:]]*:[[:space:]]" 1 font-lock-function-name-face)
    ;; unary postfix op declaration
    ("([[:space:]]*\\sw+[[:space:]]*\\(\\s_+\\)[[:space:]]*)[[:space:]]*:[[:space:]]" 1 font-lock-function-name-face)
    ;; function declaration
    ("\\(\\sw+\\)[[:space:]]*(.+)[[:space:]]*:[[:space:]]" 1 font-lock-function-name-face)
    ;; property declaration
    ("\\(\\sw+\\)[[:space:]]*:[[:space:]]" 1 font-lock-variable-name-face)
    ;; operators
    ("\\s_+" . font-lock-keyword-face)
    ;; intrinsics
    ("__\\sw+" . font-lock-builtin-face)
    ;; symbols
    (":\\sw+" . font-lock-constant-face)
    ;; expression anaphora
    ("\\_<_[[:digit:]]*\\_>" . font-lock-type-face)
    ;; block anaphora
    ("\\_<•[[:digit:]]*\\_>" . font-lock-type-face)
    (,eucalypt--prelude-names 1 font-lock-keyword-face))
  "Keywords patterns to highlight in Eucalypt mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.eu\\'" . eucalypt-mode))
(add-to-list 'auto-mode-alist '("\\.eucalypt\\'" . eucalypt-mode))
(add-to-list 'auto-mode-alist '("Eufile\\'" . eucalypt-mode))

(define-derived-mode eucalypt-mode prog-mode "Eucalypt"
  "Major mode for editing Eucalypt syntax files.

\\{eucalypt-mode-map}"
  (setq font-lock-defaults '(eucalypt-font-lock-keywords nil))
  (setq comment-start "# ")
  (setq comment-end "")
  (setq indent-tabs-mode nil)
  (setq tab-width 2))

(defconst eucalypt--command-output-buffer
  "* Eucalypt Command Output *"
  "Name of buffer to use for `eu' command output")

(defconst eucalypt--command-error-buffer
  "* Eucalypt Command Error *"
  "Name of buffer to use for `eu' command error output")

(defun eucalypt--form-command (opts)
  "Formulate a command line call to `eu' with the specified string options"
  (let* ((exe (executable-find eucalypt-eu-command)))
    (format "%s %s %s" exe eucalypt-eu-global-opts opts)))

(defun eucalypt--process-region (min max command)
  "Process region with `eu' and display in special buffer"
  (let* ((output-format (eucalypt--infer-output-format command))
	 (output-mode (eucalypt--select-output-mode output-format)))
    (progn
      (shell-command-on-region min max command
			       eucalypt--command-output-buffer
			       nil
			       eucalypt--command-error-buffer
			       t)
      (with-current-buffer (get-buffer eucalypt--command-output-buffer)
	(funcall output-mode)))))

(defun eucalypt-render-region (prefix)
  "Process the region by passing contents as stdin to `eu'"
  (interactive "P")
  (let* ((format (eucalypt-buffer-input-format))
	 (cmd (eucalypt--form-command (concat format "@-")))
	 (command (if prefix (read-shell-command "Command: " cmd) cmd)))
    (eucalypt--process-region (region-beginning) (region-end) command)))

(defun eucalypt-render-buffer (prefix)
  "Process the entire buffer by passing contents as stdin to `eu'"
  (interactive "P")
  (let* ((format (eucalypt-buffer-input-format))
	 (cmd (eucalypt--form-command (concat format "@-")))
	 (command (if prefix (read-shell-command "Command: " cmd) cmd)))
    (eucalypt--process-region (point-min) (point-max) command)))

(defun eucalypt--infer-output-format (command)
  (cond
   ((string-match "-j" command) 'json)
   ((string-match "-x[[:space:]]+\\(\\w+\\)" command) (intern (match-string 1 command)))
   (t 'yaml)))

(defun eucalypt--select-output-mode (format)
  (cond
   ((and (eq format 'yaml) (commandp 'yaml-mode)) 'yaml-mode)
   ((and (eq format 'json) (commandp 'json-mode)) 'json-mode)
   ((and (eq format 'json) (commandp 'js2-mode)) 'js2-mode)
   ((and (eq format 'json) (commandp 'js2-mode)) 'js-mode)
   (t 'text-mode)))

(defun eucalypt-buffer-input-format ()
  "Determine the appropriate input format to use for the current buffer"
  (eucalypt--extension-to-format (file-name-extension buffer-file-name)))

(defun eucalypt--extension-to-format (extension)
  "Infer an input format from the specified extension."
  (cond
   ((string= extension "yaml") "yaml")
   ((string= extension "csv") "csv")
   ((string= extension "txt") "txt")
   (t "eu")))

(provide 'eucalypt-mode)
