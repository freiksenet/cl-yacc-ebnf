(in-package :yacc-ebnf)

(defun production-action (symbol derives)
  (make-ebnf-production symbol derives))

(defun make-grammar-grammar (&key name additional-terminals productions)
  (make-ebnf-grammar
   :name name
   :start-symbol 'grammar
   :terminals (cons 'id additional-terminals)
   :precedence '()
   :productions (append
                 (make-ebnf-production 'literal '(id) :action #'identity)
                 (make-ebnf-production 'rule-name '(id) :action #'identity)
                 productions)))

(defun make-grammar-maker (grammar-grammar)
  (lambda (lexer &key name (start-symbol (required-argument)) terminals precedence)
    (make-ebnf-grammar
     :name name
     :start-symbol start-symbol
     :terminals terminals
     :precedence precedence
     :productions (parse-with-lexer lexer (make-ebnf-parser grammar-grammar)))))


