(defpackage #:yacc-ebnf-example
  (:export #:ebnf-example)
  (:use #:cl #:yacc #:yacc-ebnf))

(in-package #:yacc-ebnf-example)

;;; The lexer

(define-condition lexer-error (yacc-runtime-error)
  ((character :initarg :character :reader lexer-error-character))
  (:report (lambda (e stream)
             (format stream "Lexing failed~@[: unexpected character ~S~]"
                     (lexer-error-character e)))))

(defun lexer-error (char)
  (error (make-condition 'lexer-error :character char)))

(defun lexer (&optional (stream *standard-input*))
  (loop
     (let ((c (read-char stream nil nil)))
       (cond
         ((member c '(nil #\Newline)) (return-from lexer (values nil nil)))
         ((member c '(#\- #\.))
          (let ((symbol (intern (string c) '#.*package*)))
            (return-from lexer (values symbol symbol))))
         ((digit-char-p c)
          (return-from lexer (values 'digit c)))
         (t
          (lexer-error c))))))

;;; The parser

(define-ebnf-parser *expression-ebnf-parser*
  (:start-symbol S)
  (:terminals (|.| - digit))
  (:precedence nil)
  (S ((:option -) (:plus D) (:option |.| (:plus D))) ())
  (D digit))

;;; The toplevel loop

(defun ebnf-example ()
  (format t "A EBNF example:
             S := '-'? D+ ('.' D+)?
             D := '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'~%")
  (loop
     (with-simple-restart (abort "Return to ebnf-example toplevel.")
       (format t "? ")
       (let ((e (parse-with-lexer #'lexer *expression-ebnf-parser*)))
         (when (null e)
           (return-from ebnf-example))
         (format t " => ~A~%" (print e))))))
