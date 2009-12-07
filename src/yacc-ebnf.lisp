;;; cl-yacc-ebnf - an Extended Backus–Naur Form for cl-yacc parser generator.
;;;
;;; (c) 2009 Mikhail Novikov
;;;
;;; This file is part of cl-yacc-ebnf.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;; THE SOFTWARE.

(in-package :yacc-ebnf)

(defstruct ebnf-prod symbol derives)

(defun make-or (symbol derives)
  (mapcan
   #'(lambda (derive)
       (expand-ebnf symbol (if (listp derive) derive (list derive))))
   derives))

(defun make-plus (symbol derives)
  (expand-ebnf symbol (list derives (append derives (list symbol))) :operation :or))

(defun make-repeat (symbol derives)
  (expand-ebnf symbol (list (append derives (list symbol)) '()) :operation :or))

(defun make-option (symbol derives)
  (expand-ebnf symbol (list derives '()) :operation :or))

(defun expand-ebnf (symbol derives &key (operation '()))
  (let ((add-prods '()))
    (append
      (case operation
        (:repeat
         (make-repeat symbol derives))
        (:option
         (make-option symbol derives))
        (:or
         (make-or symbol derives))
        (:plus
         (make-plus symbol derives))
        (otherwise
         (list
          (make-ebnf-prod
           :symbol symbol
           :derives
           (mapcar #'(lambda (el)
                       (if (consp el)
                           (let ((gensym (make-gensym "production")))
                             (appendf add-prods (expand-ebnf gensym (cdr el) :operation (car el)))
                             gensym)
                           el))
                   derives)))))
     add-prods)))

(defun make-ebnf-production (symbol derives &key (action #'list) (action-form '()))
  "Creates a list of cl-yacc bnf productions from lispy ebnf notation"
  (mapcar #'(lambda (prod)
              (make-production
               (ebnf-prod-symbol prod)
               (ebnf-prod-derives prod)
               :action action
               :action-form action-form))
          (expand-ebnf symbol derives)))

(defun make-ebnf-grammar (&key name (start-symbol (required-argument)) terminals precedence productions)
  (make-grammar :name name :start-symbol start-symbol :terminals terminals :precedence precedence :productions productions))

(defun make-ebnf-parser (grammar
                    &key (discard-memos t) (muffle-conflicts nil)
                    (print-derives-epsilon nil) (print-first-terminals nil)
                    (print-states nil)
                    (print-goto-graph nil) (print-lookaheads nil))
  (make-parser grammar
               :discard-memos discard-memos :muffle-conflicts muffle-conflicts
               :print-derives-epsilon print-derives-epsilon
               :print-first-terminals print-first-terminals
               :print-states print-states
               :print-goto-graph print-goto-graph
               :print-lookaheads print-lookaheads))

(defun parse-ebnf-production (form)
  (let ((symbol (car form))
        (productions '()))
    (dolist (stuff (cdr form))
      (cond
        ((and (symbolp stuff) (not (null stuff)))
         (appendf productions (make-ebnf-production symbol (list stuff)
                                       :action #'identity :action-form '#'identity)))
        ((listp stuff)
         (let ((l (car (last stuff))))
           ;; Plato Wu,2009/12/05: function in other package is list
           (let ((rhs (if (or (symbolp l) (not (eq (car l) 'function))) stuff (butlast stuff)))
                 (action (if (or (symbolp l) (not (eq (car l) 'function))) '#'list l)))
             (appendf productions (make-ebnf-production symbol rhs
                     :action (eval action)
                     :action-form action)))))
        (t (error "Unexpected production ~S" stuff))))
    productions))

(defun parse-ebnf-grammar (forms)
  (let ((options '()) (make-options '()) (productions '()))
    (dolist (form forms)
      (cond
        ((member (car form)
                 '(:muffle-conflicts
                   :print-derives-epsilon :print-first-terminals
                   :print-states :print-goto-graph :print-lookaheads))
         (unless (null (cddr form))
           (error "Malformed option ~S" form))
         (push (car form) make-options)
         (push (cadr form) make-options))
        ((keywordp (car form))
         (unless (null (cddr form))
           (error "Malformed option ~S" form))
         (push (car form) options)
         (push (cadr form) options))
        ((symbolp (car form))
         (setq productions (nconc (parse-ebnf-production form) productions)))
        (t
         (error "Unexpected grammar production ~S" form))))
    (values (nreverse options) (nreverse make-options)
            (nreverse productions))))

(defmacro define-ebnf-parser (name &body body)
  "DEFINE-GRAMMAR NAME OPTION... PRODUCTION...
PRODUCTION ::= (SYMBOL RHS...)
RHS ::= SYMBOL | (SYMBOL... [ACTION])
Defines the special variable NAME to be a parser.  Options are as in
MAKE-GRAMMAR and MAKE-PARSER."
  (multiple-value-bind (options make-options productions) (parse-ebnf-grammar body)
    `(defparameter ,name
       ',(apply #'make-parser
                (apply #'make-ebnf-grammar
                       :name name
                       :productions productions
                       options)
                make-options))))
