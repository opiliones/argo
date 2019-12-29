;;; main.lisp
(in-package :cl-user)
(defpackage :lisnail
  (:use cl yacc smug)
  (:export :repl))
(in-package :lisnail)

(defun repl ()
  (loop do (princ "@ ")
           (finish-output)
           (let ((cmd (sh-read)))
             (prin1 cmd)
             (princ #\newline)
             (finish-output)
             (prin1 (eval cmd))
             (princ #\newline)
             (finish-output))))

(defun sh-read ()
  (parse-with-lexer (sh-lexer (read-line)) *expression-parser*))

(defun sh-lexer (code)
  (lambda ()
    (let* ((x (car (run (sh-parser) code)))
           (y (car x)))
      (setf code (cdr x))
      (values (car y) (cdr y)))))
 
(defun sh-parser ()
  (.or (.and-brank #'.operator)
       (.and-brank #'.float)
       (.and-brank #'.str)
       (.and-brank #'.sym)))

(defvar *whitespace* '(#\space #\newline #\tab))

(defun .whitespace (&optional result-type)
  (.first (.map result-type (.is 'member *whitespace*))))

(defun .and-brank (parser)
  (.let* ((x (funcall parser))
          (_ (.optional (.whitespace))))
    (.identity x)))

(defun .float ()
  (.let* ((f (.or (.hexadecimal) (.signed #'.scientific) (.signed #'.decimal))))
    (.identity (cons 'float (read-from-string f)))))

(defun .signed (parser)
  (.let* ((x (.optional (.or (.string= "+") (.string= "-"))))
          (n (funcall parser)))
    (.identity (concatenate 'string x n))))

(defun .hexadecimal ()
  (.let* ((_ (.string= "0x"))
          (n (.map 'string (.or (.is 'digit-char-p) (.is 'member '(#\A #\B #\C #\D #\E #\F))))))
    (.identity (concatenate 'string "#x" n))))

(defun .scientific ()
  (.let* ((n (.decimal))
          (_ (.string= "E"))
          (m (.signed #'.digit)))
    (.identity (concatenate 'string n "E" m))))

(defun .decimal()
  (.let* ((n (.digit))
          (m (.optional (.and (.string= ".") (.digit)))))
    (.identity (concatenate 'string n "." m))))

(defun .digit ()
  (.let* ((n (.map 'string (.is 'digit-char-p))))
    (.identity n)))

(defun .str ()
  (.let* ((x (.or (.quote-with #\") (.quote-with #\') (.quote-with #\`))))
    (.identity (cons 'str x)))) 

(defun .quote-with (q)
  (.let* ((_ (.char= q))
          (x (.zero-or-more
               (.or (.is-not #'char= q)
                    (.and (.char= q) (.char= q)))))
          (_ (.char= q)))
    (.identity (coerce x 'string))))

(defun .sym ()
  (.let* ((x (.map 'string (.or (.is-not 'member '(#\  #\\))
                                (.and (.char= #\\)
                                      (.item))))))
    (.identity (cons 'sym x))))

(defun .zero-or-more (parser)
  (.plus (.let* ((x parser)
                 (xs (.zero-or-more parser)))
           (.identity (cons x xs)))
         (.identity ())))

(defvar *operators*
  '((+  . +-)
    (-  . +-) 
    (%  . l7) 
    (/  . l7) 
    (*  . l7)
    (^   . r8) 
    (<=  . l4) 
    (<   . l4) 
    (>   . l4) 
    (>=  . l4) 
    (==  . l4) 
    (=   . l4) 
    (~   . l4) 
    (&&  . r3) 
    (||  . r2) 
    (!!  . r2) 
    (!   . pre) 
    (?   . post) 
    (\|! . r2))) 

(defun .operator ()
  (.let* ((x (.or (.string= "(") (.string= ")")
                  (.map 'string (.is 'member '(#\+ #\- #\^ #\% #\< #\> #\= #\~ #\! #\| #\& #\? #\/ #\*))))))
    (let* ((op (intern x))
           (y (cdr (assoc op *operators*))))
      (.identity (cons (if y y op)
                   (intern (concatenate 'string "sn-" x)))))))
(defun |sn-+| ()
(defun |sn--|
(defun |sn-%|
(defun |sn-/|
(defun |sn-*|



(defun |sn-^| ()
(defun |sn-<=|
(defun |sn-<|  
(defun |sn->|  
(defun |sn->=| 
(defun |sn-==| 
(defun |sn-=|  
(defun |sn-~|  
(defun |sn-&&| 
(defun sn-|| 
(defun |sn-!!| 
(defun |sn-!|  
(defun |sn-?|  
(defun sn-\|!


(defun |sn-!| (v s) (values v (not s)))
(defun |sn-?| (v s) (values s s))

(defun get-float (val)
  (if (numberp val) val
    (if (= (mismatch val "#") 1) val
      (read-from-string val))))

(eval-when (compile load eval)
  (defun i2p (a b c)
    `(values (,b (get-float ,a) (get-float ,c)) t))

  (defun post2pre (a b)
    `(multiple-value-bind (v s) ,a
       (,b v s))) 

  (defun val (v)
    `(values ,v t))

  (defun sign (a b)
    `(values (,a ,b) t))

  (defun pre (a b)
    `(multiple-value-bind (v s) ,b
      (,a v s)))

  (defun k-2-3 (a b c)
    (declare (ignore a c))
    b))

(define-parser *expression-parser*
  (:start-symbol expression)
  (:terminals (float sym str id l0 l1 l2 +- l3 l4 l5 l6 l7 l8 l9 r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 pre post |(| |)|))
  (:precedence ((:left l9) (:right r9) (:left l8) (:right r8) (:left l7)
                (:right r7) (:left l6) (:right r6) (:left l5) (:right r5) 
                (:left l4) (:right r4) (:left +- l3) (:right r3) (:left l2)
                (:right r2) (:left l1) (:right r1) (:left l0) (:right r0) (:right pre) (:left post)))

  (expression
   (expression l0 expression #'i2p)
   (expression l2 expression #'i2p)
   (expression +- expression #'i2p)
   (expression l3 expression #'i2p)
   (expression l4 expression #'i2p)
   (expression l5 expression #'i2p)
   (expression l6 expression #'i2p)
   (expression l7 expression #'i2p)
   (expression l8 expression #'i2p)
   (expression l9 expression #'i2p)
   (expression r0 expression #'i2p)
   (expression r1 expression #'i2p)
   (expression r2 expression #'i2p)
   (expression r3 expression #'i2p)
   (expression r4 expression #'i2p)
   (expression r5 expression #'i2p)
   (expression r6 expression #'i2p)
   (expression r7 expression #'i2p)
   (expression r8 expression #'i2p)
   (expression r9 expression #'i2p)
   (expression post #'post2pre)
   (pre expression #'pre)
   term)

  (term
   id
   l0 l1 l2 l3 l4 l5 l6 l7 l8 l9 
   r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 
   (sym #'val)
   (str #'val)
   (float #'val)
   (+- term #'sign)
   (|(| expression |)| #'k-2-3)))
