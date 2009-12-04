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

(defun make-repeat (symbol derives)
  (expand-ebnf symbol (list derives symbol '()) :operation :or))

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