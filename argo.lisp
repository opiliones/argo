#!/usr/bin/sbcl --script

(in-package :cl-user)
(load (merge-pathnames
        (make-pathname :name ".sbclrc")
        (user-homedir-pathname)))

(ql:quickload '(:yacc :cl-lex :cl-ppcre :sb-cltl2 :bordeaux-threads :kmrcl :sb-posix :getopt :alexandria) :silent t)

(defpackage :cl-nsh
  (:use :cl :yacc :cl-lex :cl-ppcre :sb-ext :sb-cltl2 :alexandria))

(in-package :cl-nsh)

(define-string-lexer nsh-lexer
  ("{[ 	]*" (return (values '{ '{)))
  ("\\^\\([ 	]*" (return (values '|^(| '|^(|)))
  ("[ 	]*}" (return (values '} '})))
  ("[ 	]*#[^\\n]*" (return (values '|#| '|#|)))
  ("\\([ 	]*" (return (values '|(| '|(|)))
  ("[ 	]*\\)" (return (values '|)| '|)|)))
  ("[ 	]*([;\\n]+[ 	]*)+" (return (values '|;| '|;|)))
  ("[ 	]*(\\|\\||&&|->)[ 	]*" (return (values '|&&| (intern (string-trim " 	" $@)))))
  ("[ 	]*`[^`]+`[ 	]*" (return (values '|&&| (intern (string-trim " 	`" $@)))))
  ("[ 	]*(\\||&)[ 	]*" (return (values '|&| (intern (string-trim " 	" $@)))))
  ("[ 	]*(>>|>|<)[ 	]*" (return (values '|>| (intern (string-trim " 	" $@)))))
  ("\\$\\$[0-9]+" (return (values '$$* (read-from-string (subseq $@ 2)))))
  ("\\$[0-9]+" (return (values '$* (read-from-string (subseq $@ 1)))))
  ("\\$\\$([^^{}()    \\n#\\\\\"'!$&|<>;]|\\.)+" (return (values '$$ (intern (subseq $@ 2)))))
  ("\\$@([^^{}()    \\n#\\\\\"'!$&|<>;]|\\.)+" (return (values '$@ (intern (subseq $@ 2)))))
  ("\\$([^^{}()    \\n#\\\\\"'!$&|<>;]|\\.)+" (return (values '$ (intern (subseq $@ 1)))))
  ("\\^" (return (values '^ '^)))
  ("[ 	]*@" (return (values '@ '@)))
  ("[ 	]+" (return (values 'brank  'brank)))
  ("\"([^\\\\\"]|\\\\.)*\"" (return (values 'string (read-from-string $@))))
  ("'([^']|'')*'" (return (values 'string (regex-replace-all "''" (subseq $@ 1 (- (length $@) 1)) "'"))))
  ("-?0|[1-9][0-9]*(\\.[0-9]*)?([e|E][+-]?[0-9]+)?" (return (values 'number (read-from-string $@))))
  ("([^^{}() 	\\n#\\\\\"`'!$&|<>;]|\\.)+" (return (values 'symbol (intern $@))))
)

(defun comma (x) (cadr ``,,x))
(defun at-comma (x) (car (cadadr `'`(,@,x))))
(defun asta-comma (n) (cadr ``,(nth ,n |*|)))
(defvar *bq* (car '`,()))
(defvar *exit* #'(lambda () ()))
(defvar *symbol-string-func* '(+ - / * = /=))
(defconstant @ '@)

(defun infix (l m r)
  (list (trans-if-need m) l r))

(defun trans-if-need (x)
  (case x ('|>| 'redirect-write)
          ('|<| 'redirect-read)
          (t x)))

(define-parser nsh-parser
  (:start-symbol nsh)
  (:terminals (symbol brank string number |$| |$$| |$@| |$*| |$$*| |(| |)| |@| |^| |>| |&| |&&| |{| |^(| |}| |;| |#|))
  (:precedence ((:right |#|) (:right |^|) (:right |@|) (:left |>|)
                (:right |&|) (:right |&&|) (:right |;|)))

  (nsh
    nil
    command
  )

  (word
    string
    number
    |$|
    (symbol #'(lambda (s) `',s))
    (|$$| #'comma)
    (|$@| #'at-comma)
    (|$*| #'(lambda (n) `(nth ,(- n 1) *)))
    (|$$*| #'(lambda (n) (asta-comma (- n 1))))
    (|(| nsh |)| #'(lambda (l nsh r) nsh))
    (|{| nsh |}| #'(lambda (l nsh r) `(,*bq* ,nsh)))
    (|^(| nsh |)| #'(lambda (l nsh r) `#'(lambda (&rest |*|) (block nil ,nsh))))
  )

  (words
    (word word #'(lambda (l r) `(|^| ,l ,r)))
    (word |^| word #'infix)
  )

  (term
    word
    words
  )

  (command
    (terms #'(lambda (x) `(| | ,@x)))
    (command |#| #'(lambda (x _) x))
    (|#| command #'(lambda (_ x) x))
    (command |&| command #'infix)
    (command |&&| command #'infix)
    (command |;| command #'infix)
    (command |>| command #'infix)
  )

  (terms
    (term brank terms #'(lambda (l _ r) (cons l r)))
    (|@| terms #'cons)
    (term |@| #'list)
    (term |@| terms #'(lambda (l _ r) `(,l |@| ,@r)))
    (term #'list)
  )
)

(defun |^| (x y)
  (let ((xlistp (listp x)) (ylistp (listp y)))
    (if (or xlistp ylistp)
      (let ((x (if xlistp x (list x)))
            (y (if ylistp y (list y))))
        (reduce #'append
          (loop for i in x
            collect (loop for j in y
                      collect (|^| i j)))))
      (format nil "~A~A" x y))))

(defmacro | | (f &rest xs)
  (cond ((find '|@| (cons f xs)) `(splice ,f ,@xs))
        ((and (listp f) (eq 'quote (car f)))
           (let ((y (eval f)))
             (cond ((fboundp y) `(,y ,@xs))
                   (t `(if (fboundp ,f)
                         (,y ,@xs)
                         (|cmd| ,f ,@xs))))))
        ((and (listp f) (eq 'function (car f))) `(funcall ,f ,@xs))
        ((and (listp f) (null xs)) f)
        ((listp f) `(cmdcall (list ,f ,@xs)))
        ((symbolp f) `(cmdcall (list ,f ,@xs)))
        (t `(funcall ,f ,@xs))))

(defun |cmd| (cmd &rest args)
  (let ((x (sb-ext:process-exit-code (sb-ext:run-program
             (princ-to-string cmd) (mapcar #'princ-to-string args)
             :output *standard-output* :input *standard-input* :search t))))
    (values (eq x 0) x)))

(defun |init| (xs)
  (if (null (cdr xs)) nil (cons (car xs) (|init| (cdr xs)))))

(defmacro |;| (x y)
  (let ((f (cadr x)))
    (cond
      ((equal ''|let| f) `(| | ,f ,(|init| (cddr x)) ,(car (last x)) ,y))
      (t `(progn ,x ,y)))))

(defmacro |&| (x y)
  (let ((thd (gensym)) (ret (gensym))) 
    `(let ((,thd (bordeaux-threads:make-thread #'(lambda () ,x)))
           (,ret ,y))
        (cons (bordeaux-threads:join-thread ,thd) ,ret))))

(defmacro |\|| (x y)
  (let ((thd (gensym)) (ret1 (gensym)) (ret2 (gensym)) (r (gensym)) (w (gensym))) 
    `(multiple-value-bind (,r ,w) (sb-unix:unix-pipe)
        (let* ((,thd (bordeaux-threads:make-thread
                       #'(lambda ()
                           (let ((*standard-output*
                                   (sb-sys:make-fd-stream ,w :output t)))
                             (unwind-protect (progn ,x))
                               (close *standard-output*)
                               (sb-unix:unix-close ,w)))))
               (*standard-input*
                 (sb-sys:make-fd-stream ,r :input t))
               (,ret1 (unwind-protect ,y
                       (close *standard-input*)
                       (sb-unix:unix-close ,r)))
               (,ret2 (bordeaux-threads:join-thread ,thd)))
          (cons ,ret1 ,ret2)))))

(defmacro redirect-write (x y)
  (let ((ys (cdr y)))
    (if (cdr ys) `(redirect :output :supersede #'(lambda () ,x) ,@ys) 
                 `(redirect :output :supersede #'(lambda () ,x) 1 ,(car ys)))))

(defmacro |>>| (x y)
  (let ((ys (cdr y)))
    (if (cdr ys) `(redirect :output :append #'(lambda () ,x) ,@ys) 
                 `(redirect :output :append #'(lambda () ,x) 1 ,(car ys)))))

(defmacro redirect-read (x y)
  `(redirect :input :error  #'(lambda () ,x) 0 ,(nth 1 y)))

(defun splice (&rest xs)
  (cmdcall (splice-merge xs)))

(defun cmdcall (xs)
  (let ((f (car xs)) (args (cdr xs)))
    (cond ((functionp f) (apply f args))
          ((and (symbolp f) (fboundp f)) (apply f args))
          ((stringp f) (cmdcall (cons (intern f) args)))
          (t (apply #'|cmd| xs)))))

(defun splice-merge (ys)
  (let ((x (car ys))
        (xs (cdr ys)))
    (case x ('nil nil)
            (@ (if (listp (car xs)) (append (car xs) (splice-merge (cdr xs)))
                                    (splice-merge xs)))
            (t (cons x (splice-merge xs))))))

(defun redirect (m1 m2 f s d)
  (case d (0 (redirect-fd f s *standard-input*))
          (1 (redirect-fd f s *standard-output*))
          (2 (redirect-fd f s *error-output*))
          (t (with-open-file (h (if (stringp d) d (princ-to-string d))
               :direction m1 :if-exists m2)
               (redirect-fd f s h)))))

(defun redirect-fd (f s d)
  (case s (0 (let ((*standard-input* d)) (funcall f)))
          (1 (let ((*standard-output* d)) (funcall f)))
          (2 (let ((*error-output* d)) (funcall f)))))

(defmacro |import| (x y)
  (eval `(import-func ,(eval x) ,(eval y))) y)

(defmacro import-func (x y)
  `(defmacro ,y (&rest args) `(,',x ,@,'args)))

(defmacro |let| (vars x &optional body)
  (when body `(multiple-value-bind ,(mapcar #'eval vars) ,x  ,body)))

(defun |trap| (f &rest xs)
  (let ((xs (if xs xs '(0 1 2 5 15))))
    (loop for i in xs
      do (if (zerop i) (setf *exit* f)
           (kmrcl:set-signal-handler i f)))))

(defun |fork| (&rest xs)
  (if (zerop (sb-posix:fork)) (cmdcall xs) t))

(defmacro |\|\|| (x y) `(or ,x ,y))

(defmacro |&&| (x y) `(and ,x ,y))

(defmacro |->| (x y)
  (let ((ret (gensym)))
    `(let ((,ret ,x))
      (,@y ,ret))))

(defmacro |def| (f body)
  (eval `(defun ,(eval f) (&rest |*|) (block nil ,body))) f)

(defmacro |mac| (f body)
  (eval `(defmacro ,(eval f) (&rest |*|) (block nil ,body))) f)

(defmacro import-func1 (x)
  `(import-func ,x ,(intern (string-downcase (princ-to-string x)))))

(defmacro import-funcs (xs)
  `(progn ,@(mapcar #'(lambda (x) (list 'import-func1 x)) xs)))

(defmacro |simple-import| (&rest xs)
  (eval `(import-funcs ,(mapcar #'eval xs)))
  `(list ,@xs))

(defmacro |cond| (&rest xs)
  `(cond ,@(mapcar #'cdr xs)))

(defmacro |case| (x &rest xs)
  `(case ,x ,@(mapcar #'cdr xs)))

(defmacro |block| (x &rest xs)
  `(block ,(eval x) ,@xs))

(defmacro |return-from| (x &optional xs)
  `(return-from ,(eval x) ,xs))

(defmacro |exit| (&optional (n 0))
  `(exit :code (|num| ,n)))

(defmacro |ecode| (&rest xs)
  (let ((x (gensym)) (v (gensym)))
    `(multiple-value-bind (,x ,v) (|cmd| ,@xs) ,v)))

(defmacro |list-val| (&rest xs)
  `(multiple-value-list (| | ,@xs)))

(import-funcs (
  return values
  read-line read-char
  eval
  list car cdr cons listp first rest second third fourth nth last length
  mapcar remove-if remove-if-not reduce remove-duplicates reverse append
  eq eql equal and or when unless if
  numberp max min
  sin cos tan acos asin atan
  exp expt sqrt log
  sqrt isqrt
  abs
  conjugate phase signum cis
  gcd lcm
  floor ceiling round ffloor fceiling fround
  truncate ftruncate mod rem
  float
  rational rationalize
  numerator denominator
  complex realpart imagpart
  zerop plusp minusp evenp oddp integerp floatp rationalp realp complexp
))

(defun |true| (&optional &rest x) `(values t ,@x))
(defun |:| (&optional &rest x) `(values t ,@x))

(defun |false| (&optional &rest x) `(values nil ,@x))

(defmacro |load| (f) `(print-eval (parse-file (if (symbolp ,f) (princ-to-string ,f) ,f))))

; ~ loop tmpf tmpd chk dict udict ins del sep usep sub getenv setenv glob shift %

(import-func <  |lt|)
(import-func <= |le|)
(import-func >  |gt|)
(import-func >= |ge|)
(import-func read-line |read|)

(defun |echo| (&rest xs) (format t "~{~A~^ ~}~%" xs) xs)

(defun |num| (x)
  (if (numberp x) x
      (let ((n (read-from-string (if (symbolp x) (princ-to-string x) x))))
        (if (numberp n) n x))))

(defun repl ()
  (loop (print-eval (|parse| (read-line)))))

(defun build (file code) ())

(defun parse-file (file)
  (parse (read-file-into-string file)))

(defun |parse| (code)
  (parse-with-lexer (nsh-lexer (string-trim " 	
;" code)) nsh-parser))

(defun main ()
  (multiple-value-bind (out-args out-opts errors)
    (getopt:getopt (cdr sb-ext:*posix-argv*) '(("c" :REQUIRED) ("b" :REQUIRED)))
    (cond
      (errors (format t "Usage: ~a [-b FILE] {-c CODE|FILE}~%" (car sb-ext:*posix-argv*)))
      ((and (null out-opts) (null out-args)) (repl))
      (t (let* ((code (cdr (assoc "c" out-opts :test #'string=)))
                (ast (if code
                       (|parse| code)
                       (parse-file (car out-args))))
                (|*| (if code out-args (cdr out-args)))
                (build (cdr (assoc "b" out-opts :test #'string=))))
           (if build (build build ast)
                     (print-eval ast)))))))

(defun print-eval (p)
  (print p)
  (princ #\newline)
  (print (sb-cltl2:macroexpand-all p))
  (princ #\newline)
  (unwind-protect
    (eval p)
    (funcall *exit*)))

(main)
