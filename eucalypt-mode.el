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
  "Syntax table in use in Windows style `conf-mode' buffers.")

(defvar eucalypt--prelude-names
  (regexp-opt '("str"
		"eu"
		"io"
		"panic"
		"assert"
		"null"
		"true"
		"false"
		"cat"
		"if"
		"then"
		"cons"
		"head"
		"head-or"
		"tail"
		"nil"
		"first"
		"second"
		"sym"
		"merge"
		"deep-merge"
		"elements"
		"block"
		"has"
		"lookup"
		"lookup-in"
		"lookup-or"
		"lookup-or-in"
		"lookup-alts"
		"not"
		"and"
		"or"
		"inc"
		"dec"
		"zero?"
		"pos?"
		"neg?"
		"num"
		"floor"
		"ceiling"
		"max"
		"max-of"
		"min"
		"min-of"
		"ch"
		"identity"
		"const"
		"compose"
		"apply"
		"flip"
		"complement"
		"curry"
		"uncurry"
		"cond"
		"juxt"
		"with-meta"
		"assertions"
		"nil?"
		"take"
		"drop"
		"take-while"
		"take-until"
		"drop-while"
		"drop-until"
		"nth"
		"repeat"
		"foldl"
		"foldr"
		"scanl"
		"scanr"
		"iterate"
		"count"
		"last"
		"cycle"
		"map"
		"map2"
		"zip-with"
		"zip"
		"filter"
		"remove"
		"append"
		"prepend"
		"concat"
		"zip-apply"
		"reverse"
		"all-true?"
		"all"
		"any-true?"
		"any"
		"window"
		"partition"
		"over-sliding-pairs"
		"differences"
		"merge-all"
		"key"
		"value"
		"keys"
		"values"
		"bimap"
		"map-first"
		"map-second"
		"map-kv"
		"pair"
		"zip-kv"
		"with-keys"
		"map-values"
		"map-keys"
		"filter-items"
		"by-key"
		"by-key-name"
		"by-key-match"
		"by-value"
		"match-filter-values"
		"filter-values"
		"alter-value"
		"update-value"
		"alter"
		"update"
		"update-value-of"
		"set-value"
		"tongue"
		"merge-at"
		"set"
		"dict") 'symbols))

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
  (setq comment-end ""))

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

(defun eucalypt--process-region (min max)
  "Process region with `eu' and display in special buffer"
  (shell-command-on-region min
			   max
			   command
			   eucalypt--command-output-buffer
			   nil
			   eucalypt--command-error-buffer
			   t)
  (if (commandp 'yaml-mode)
      (with-current-buffer (get-buffer eucalypt--command-output-buffer)
	(funcall 'yaml-mode))))

(defun eucalypt-render-region (prefix)
  "Process the region by passing contents as stdin to `eu'"
  (interactive "P")
  (let* ((cmd (eucalypt--form-command "eu@-"))
	 (command (if prefix (read-shell-command "Command: " cmd) cmd)))
    (eucalypt--process-region (region-beginning) (region-end))))

(defun eucalypt-render-buffer (prefix)
  "Process the entire buffer by passing contents as stdin to `eu'"
  (interactive "P")
  (let* ((cmd (eucalypt--form-command "eu@-"))
	 (command (if prefix (read-shell-command "Command: " cmd) cmd)))
    (eucalypt--process-region (point-min) (point-max))))

(provide 'eucalypt-mode)
