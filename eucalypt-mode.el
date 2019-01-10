;;; eucalypt-mode.el --- Major mode for Eucalypt code

;; -*- lexical-binding: t; -*-
;;
;; Copyright © 2019 Greg Hawkins

(defvar eucalypt-mode-hook nil)

(defvar eucalypt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
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

(defvar prelude-names
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
  `(("`" . font-lock-preprocessor-face)
    ;; any operator and all prelude fns
    ("\\s_+" . font-lock-keyword-face)
    ("__\\sw+" . font-lock-builtin-face)
    ;; symbols
    (":\\sw+" . font-lock-constant-face)
    ("\\(\\sw+\\)[[:space:]]*(.+)[[:space:]]*:[[:space:]]" 1 font-lock-function-name-face)
    ("\\(\\sw+\\)[[:space:]]*:[[:space:]]" 1 font-lock-variable-name-face)
    (,prelude-names 1 font-lock-keyword-face))
  "Keywords patterns to highlight in Eucalypt mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.eu\\'" . eucalypt-mode))

(defun eucalypt-mode ()
  "Major mode for editing Eucalypt syntax files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table eucalypt-mode-syntax-table)
  (use-local-map eucalypt-mode-map)
  (setq font-lock-defaults '(eucalypt-font-lock-keywords nil))
  (setq comment-start "# ")
  (setq comment-end "")
  (setq major-mode 'eucalypt-mode)
  (setq mode-name "Eucalypt")
  (run-hooks 'eucalypt-mode-hook))

(provide 'eucalypt-mode)
