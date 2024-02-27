(require 'generic-x) ;; we need this

(define-generic-mode 'cobalt-mode   ;; name of the mode to create
  '("//")                           ;; comments start with '--'
  '("type" "and" "class" "this" "linear" "object" "new" "done" "in" "let" "case" "of" "true" "false" "if" "then" "else") ;; keywords
  '(("#[[:alnum:]]+" . 'font-lock-type-face)
    ("[[:digit:]]+" . 'font-lock-constant-face)
    ("]" . 'font-lock-function-name-face)
    ("[[{}|!:.&;\u00AC\u25B8\u2308\u230B]" . 'font-lock-function-name-face)
    ("?" . 'font-lock-preprocessor-face)
    )
  '("\\.cob") ;; files for which to activate this mode
  nil                              ;; other functions to call
  "A mode for Cobalt files"        ;; doc string for this mode
  )
