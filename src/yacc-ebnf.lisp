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

(defun process-option (derive)
  (apply #'map-product #'(lambda (&rest args) (remove '() args))
         (labels ((process-option-rec (derive)
                    (if (null derive)
                        '()
                        (cons
                         (if (and (consp (car derive)) (eq :option (caar derive)))
                             (append (cdar derive) '(()))
                             (list (car derive)))
                         (process-option-rec (cdr derive))))))
           (process-option-rec derive))))

(defun make-repeat (derive)
  (let ((repeat (make-gensym "repeat")))
    (values
     (list
      (list repeat derive)
      (list repeat (append derive (list repeat))))
    repeat)))

(defun make-optional (derive)
  (list :option derive))

(defun process-repeat (symbol derive)
  (let* ((result '())
         (original
          (mapcar #'(lambda (el)
                      (if (and (consp el) (eq :repeat (car el)))
                          (multiple-value-bind (derives repeat-symb) (make-repeat (cdr el))
                            (appendf result derives)
                            (make-optional repeat-symb))
                          el))
                  derive)))
    (cons (list symbol original) result)))

(defun make-ebnf-production (symbol derives &key (action #'list) (action-form '()))
  "Creates a list of cl-yacc bnf productions from lispy ebnf notation"
  (cond
    ((find :repeat (remove-if-not #'listp derives) :key #'car)
     (iter (for prod in (process-repeat symbol derives))
           (appending (funcall #'make-ebnf-production
                             (car prod)
                             (cadr prod)
                             :action action
                             :action-form action-form))))
    ((find :option (remove-if-not #'listp derives) :key #'car)
     (mapcan #'(lambda (x) (funcall #'make-ebnf-production
                           symbol x
                           :action action
                           :action-form action-form))
            (process-option derives)))
    (t
     (list (make-production symbol derives :action action :action-form action-form)))))