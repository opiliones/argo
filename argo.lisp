#!/usr/bin/sbcl --script

(in-package :cl-user)
(load (merge-pathnames
        (make-pathname :name ".sbclrc")
        (user-homedir-pathname)))

(ql:quickload '(:yacc :cl-lex :cl-ppcre :sb-cltl2 :bordeaux-threads :kmrcl :sb-posix :getopt :alexandria :cl-fad) :silent t)

(defpackage :cl-nsh
  (:use :cl :yacc :cl-lex :cl-ppcre :sb-ext :sb-cltl2 :cl-fad) (:export))

(in-package :cl-nsh)

(defun and-brank (x) (concatenate 'string x "(\\\\\\n|[ 	])*"))
(defun brank-and (x) (concatenate 'string "(\\\\\\n|[ 	])*" x))
(defun in-brank (x) (and-brank (brank-and x)))
(defun and-nl (x) (concatenate 'string x "(\\\\\\n|[; \\n	])*"))
(defun nl-and (x) (concatenate 'string "(\\\\\\n|[; \\n	])*" x))
(defun in-brank-nl (x) (and-nl (brank-and x)))
(defun many1 (x) (concatenate 'string "(" x ")+"))

(defmacro my-define-string-lexer (x &rest xs)
  `(define-string-lexer ,x
     ,@(mapcar #'(lambda (y) (cons (eval (car y)) (cdr y))) xs)))

(defun trim-brank (s) (string-trim " 	\\
" s))

(my-define-string-lexer nsh-lexer
  ("^#![^\\n]*" (return (values '|#| '|#|)))
  ((nl-and "##[^\\n]*") (return (values '|#| '|#|)))
  ((and-nl "{") (return (values '{ '{)))
  ((and-nl "\\^\\{") (return (values '|^{| '|^{|)))
  ((and-nl "`\\{") (return (values '|`{| '|`{|)))
  ((and-nl ":\\{") (return (values '|:{| '|:{|)))
  ((nl-and "}") (return (values '} '})))
  ((and-nl "\\(") (return (values '|(| '|(|)))
  ((nl-and "\\)") (return (values '|)| '|)|)))
  ((many1 (in-brank "[;\\n]")) (return (values '|;| '|;|)))
  ((in-brank-nl "(\\|\\||&&)") (return (values '|&&| (intern (trim-brank $@)))))
  ((in-brank-nl "(/>|=>)") (return (values '|=>| (intern (trim-brank $@)))))
  ((in-brank-nl "(->|-->)") (return (values '|->| (intern (trim-brank $@)))))
  ((in-brank-nl "(\\||&)") (return (values '|&| (intern (trim-brank $@)))))
  ((in-brank-nl "(>>|>|<)") (return (values '|>| (intern (trim-brank $@)))))
  ("\\$\\$[0-9]+" (return (values '$$* (read-from-string (subseq $@ 2)))))
  ("\\$[0-9]+" (return (values '$* (read-from-string (subseq $@ 1)))))
  ("\\$\\$([^^{}() 	\\n\\\\\"'`$&|<>;])+" (return (values '$$ (intern (subseq $@ 2)))))
  ("\\$@[0-9]+" (return (values '$@* (read-from-string (subseq $@ 2)))))
  ("\\$@([^^{}() 	\\n\\\\\"'`$&|<>;])+" (return (values '$@ (intern (subseq $@ 2)))))
  ("\\$([^^{}() 	\\n\\\\\"'`$&|<>;])+" (return (values '$ (intern (subseq $@ 1)))))
  (":([^:^{}() 	\\n\\\\\"'`$&|<>;])+" (return (values '$ (read-from-string $@))))
  ("#([^:^{}() 	\\n\\\\\"'`$&|<>;])+" (return (values '$ (read-from-string $@))))
  ("\\^" (return (values '^ '^)))
  ((brank-and "@") (return (values '@ '@)))
  ((in-brank-nl "`[^ 	\\n`]+`") (return (values '|``| (intern (subseq (trim-brank $@) 1 (- (length $@) 1))))))
  ((in-brank-nl "`[^ 	\\n{`]+") (return (values '|`| (intern (subseq (trim-brank $@) 1)))))
  ("(\\\\\\n|[ 	])+" (return (values 'brank  'brank)))
  ("\"([^\\\\\"]|\\\\.)*\"" (return (values 'string (read-from-string $@))))
  ("'([^']|'')*'" (return (values 'string (regex-replace-all "''" (subseq $@ 1 (- (length $@) 1)) "'"))))
  ("-?(0|[1-9][0-9]*)(\\.[0-9]*)?([e|E][+-]?[0-9]+)?" (return (values 'number (read-from-string $@))))
  ("([^^{}() 	\\n\\\\\"`'$&|<>;]|\\\\.)+" (return (values 'symbol (intern (parse-escape $@)))))
)

(defun parse-escape (s)
  (if (find #\\ s :test #'equal)
    (with-input-from-string  (in s)
      (with-output-to-string (*standard-output*)
        (loop
          (let ((c (read-char in nil nil)))
            (case c
              ('nil (return))
              (#\\  (princ (read-char in nil nil)))
              (t    (princ c)))))))
    s))

(defun comma (x) (cadr ``,,x))
(defun at-comma (x) (car (cadadr `'`(,@,x))))
(defun at-comma-n (n) (car (cadadr `'`(,@(nth ,n |*|)))))
(defun asta-comma (n) (cadr ``,(nth ,n |*|)))
(defvar *bq* (car '`,()))
(defvar *exit* #'(lambda () ()))
(defvar *pipe-policy* '|last|)
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
  (:terminals (symbol brank string number |$| |$$| |$@| |$@*| |$*| |$$*| |(| |)| |@| |^| |>| |&| |&&| |`| |``| |=>| |->| |{| |:{| |`{| |^{| |}| |;| |#|))
  (:precedence ((:right |^|) (:left |``|) (:right |@|) (:left |>|) (right |#|)
                (:left |`|) (:right |&|) (:left |&&|) (:left |->|) (:left |=>|) (:right |;|)))

  (nsh
    nil
    command
    (|#| nsh #'(lambda (l r) r))
    (|#| |;| nsh #'(lambda (l _ r) r))
  )

  (word
    string
    number
    |$|
    (symbol #'(lambda (s) `',s))
    (|$$| #'comma)
    (|$@| #'at-comma)
    (|:{| terms |}| #'(lambda (l x r) `(list ,@x)))
    (|$@*| #'(lambda (n) (at-comma-n (- n 1))))
    (|$*| #'(lambda (n) `(nth ,(- n 1) *)))
    (|$$*| #'(lambda (n) (asta-comma (- n 1))))
    (|{| nsh |}| #'(lambda (l nsh r) nsh))
    (|(| lisp-cmd |)| #'(lambda (l nsh r) nsh))
    (|`{| nsh |}| #'(lambda (l nsh r) `(,*bq* ,nsh)))
    (|^{| nsh |}| #'(lambda (l nsh r) `#'(lambda (&rest |*|) (block nil ,nsh))))
  )

  (words
    (word word #'(lambda (l r) `(|^| ,l ,r)))
    (word |^| word #'infix)
  )

  (term
    word
    words
  )

  (lisp-cmd
    (lisp-terms #'(lambda (xs)
                  (cons (let ((x (car xs)))
                          (if (and (listp x) (eq 'quote (car x))) (eval x) x))
                        (cdr xs))))
  )

  (lisp-terms
    terms
    (terms |;| lisp-terms #'(lambda (l _ r) (append l r)))
  )

  (command
    (terms #'(lambda (x) `(| | ,@x)))
    (command |&| command #'infix)
    (command |`| command #'infix)
    (command |&&| command #'infix)
    (command |->| command #'infix)
    (command |=>| command #'infix)
    (command |;| command #'infix)
    (command |>| command #'infix)
    (command |#| #'(lambda (l _) l))
  )

  (terms
    (term brank terms #'(lambda (l _ r) (cons l r)))
    (|@| terms #'cons)
    (term |@| #'list)
    (term |@| terms #'(lambda (l _ r) `(,l |@| ,@r)))
    (term |``| term #'(lambda (l f r) `(',f ,l ,r)))
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
        ((numberp f) f)
        ((listp f) `(cmdcall (list ,f ,@xs)))
        ((symbolp f) `(cmdcall (list ,f ,@xs)))
        (t `(funcall ,f ,@xs))))

(defun |cmd| (&rest args)
  (handler-case (apply #'cmd args)
    (simple-error (c) (format *error-output* "~A~%" c))))

(defvar *standard-input-overloaded* nil)
(defun standard-input ()
  (if *standard-input-overloaded* *standard-input* t)) 

(defun cmd (cmd &rest args)
  (let ((x (sb-ext:process-exit-code (sb-ext:run-program
             (princ-to-string cmd) (mapcar #'princ-to-string args)
             :output *standard-output* :input (standard-input) :search t))))
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
    `(let ((bordeaux-threads:*default-special-bindings*
             `((*standard-input* . ,*standard-input*)
               (*standard-input-overloaded* . ,*standard-input-overloaded*)
               (*invoke-debugger-hook* . ,*invoke-debugger-hook*)))
           (,thd (bordeaux-threads:make-thread #'(lambda () ,x)))
           (,ret (multiple-value-list ,y)))
        (cons (multiple-value-list (bordeaux-threads:join-thread ,thd)) ,ret))))

(defmacro |\|| (x y)
  (let ((thd (gensym)) (ret1 (gensym)) (ret2 (gensym)) (r (gensym)) (w (gensym))) 
    `(multiple-value-bind (,r ,w) (sb-unix:unix-pipe)
        (let* ((bordeaux-threads:*default-special-bindings*
                 `((*standard-input* . ,*standard-input*)
                   (*standard-input-overloaded* . ,*standard-input-overloaded*)
                   (*invoke-debugger-hook* . ,*invoke-debugger-hook*)))
               (,thd (bordeaux-threads:make-thread
                       #'(lambda () (multiple-value-list
                           (let ((*standard-output*
                                   (sb-sys:make-fd-stream ,w :output t)))
                             (unwind-protect ,x
                               (close *standard-output*)
                               (sb-unix:unix-close ,w)))))))
               (*standard-input-overloaded* t)
               (*standard-input*
                 (sb-sys:make-fd-stream ,r :input t))
               (,ret1 (multiple-value-list
                        (unwind-protect ,y
                          (close *standard-input*)
                          (sb-unix:unix-close ,r))))
               (,ret2 (bordeaux-threads:join-thread ,thd)))
          (apply #'values
            (case *pipe-policy*
              ('|last|  ,ret1)
              ('|right| (if (and (car ,ret1) (not (car ,ret2))) ,ret2 ,ret1 ))
              ('|left|  (if (car ,ret2) ,ret1 ,ret2))
              ('|cons|  (cons ,ret2 ,ret1))))))))

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
  (let* ((xs (macroexpand xs))
         (f (car xs)) (args (cdr xs)))
    (cond ((functionp f) (apply f args))
          ((and (symbolp f) (fboundp f)) (apply f args))
          ((numberp f) f)
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
          (t (if (streamp d)
                (prog1 (redirect-fd f s d) (file-position d 0))
                (with-open-file (h (if (stringp d) d (princ-to-string d))
                  :direction m1 :if-exists m2)
                  (redirect-fd f s h))))))

(defun redirect-fd (f s d)
  (case s (0 (let ((*standard-input* d) (*standard-input-overloaded* t))
               (funcall f)))
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

(defmacro |\|\|| (x y)
  (let ((f (cadr x)))
    (cond
      ((equal ''|let| f)
        (let ((var (|init| (cddr x))))
          `(| | ,f ,var ,(car (last x)) (or ,(eval (car var)) ,y))))
      (t `(or ,x ,y)))))

(defmacro |&&| (x y)
  (let ((f (cadr x)))
    (cond
      ((equal ''|let| f)
        (let ((var (|init| (cddr x))))
          `(| | ,f ,var ,(car (last x)) (and ,(eval (car var)) ,y))))
      (t `(and ,x ,y)))))

(defmacro |=>| (x y)
  `(|&&| ,x (return ,y)))

(defmacro |/>| (x y)
  `(|\|\|| ,x (return ,y)))

(defmacro |!| (&rest xs) `(not (| | ,@xs)))

(defmacro |->| (x y)
  (let ((ret (gensym)))
    `(let ((,ret ,x))
      (,@y ,ret))))

(defmacro |-->| (x y)
  (let ((ret (gensym)))
    `(let ((,ret (multiple-value-list ,x)))
      (cmdcall (append (list ,@(cdr y)) ,ret)))))

(defmacro |fn| (f body)
  (let* ((tmp (intern (concatenate 'string "#" (princ-to-string (eval f)))))
         (c `(defun ,tmp (&rest |*|) (block nil ,body))))
    (eval `(defun ,(eval f) (&rest |*|) (block nil ,body)))
    `(progn (eval ',c) (setf (fdefinition ,f) (fdefinition ',tmp)))))

(defmacro |const| (n v)
  (eval `(defconstant ,(eval n) ,v)))

(defmacro |mac| (f body)
  (eval `(defmacro ,(eval f) (&rest |*|) (block nil ,body))) f)

(defmacro import-func1 (x)
  `(import-func ,x ,(intern (string-downcase (princ-to-string x)))))

(defun |to-lisp-symbol| (x)
  (let ((y (if (symbolp x) x (eval x))))
    (intern (string-upcase (princ-to-string y)))))

(defmacro import-funcs (xs)
  `(progn ,@(mapcar #'(lambda (x) (list 'import-func1 x)) xs)))

(defmacro |simple-import| (&rest xs)
  (eval `(import-funcs ,(mapcar #'eval xs)))
  `(list ,@xs))

(defmacro |block| (x &rest xs)
  `(block ,(eval x) ,@xs))

(defmacro |return-from| (x &optional xs)
  `(return-from ,(eval x) ,xs))

(defmacro |exit| (&optional (n 0))
  `(exit :code (|num| ,n)))

(defmacro |ecode| (&rest xs)
  (let ((x (gensym)) (v (gensym)))
    `(multiple-value-bind (,x ,v) (|cmd| ,@xs) ,v)))

(defun |path-exists| (p) (probe-file (princ-to-string p)))
(defun |dir-exists| (p) (directory-exists-p (princ-to-string p)))

(defun |in-list| (&rest xs) (multiple-value-list (cmdcall xs)))

(import-funcs (
  return values
  eval
  throw catch
  format unwind-protect
  list car cdr cons listp first rest second third fourth nth last length
  mapcar remove-if remove-if-not reduce remove-duplicates reverse append
  eq eql equal and or when unless if cond case
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
  subseq replace concatenate map
  string-upcase string-downcase string-capitalize string-trim
  string intern find-if position-if count-if position count search
  parse-integer read-from-string
  string= string-equal string< string< string-lessp
  scan scan-to-strings all-matches-as-strings regex-replace regex-replace-all split
  make-hash-table gethash remhash
))

(import-func string< |string-lt|)
(import-func string> |string-gt|)
(import-func length |len|)

(defmacro |var| (x y) `(defvar ,(eval x) ,y))
(defmacro |~| (x y) `(all-matches-as-strings (princ-to-string ,x) (princ-to-string ,y))) 
(defmacro |sub| (x y z) `(regex-replace-all (princ-to-string ,x) (princ-to-string ,z) (princ-to-string ,y)))
(defun |ulist| (&rest xs) (apply #'values (reduce #'append xs)))
(defun |sep| (x &optional y)
  (if y (split (princ-to-string x) y)
        (split "[ 	\n]+" (string-trim " 	\n" x))))

(defun |usep| (x &optional y)
  (if y (let ((x (princ-to-string x))) (reduce #'(lambda (a b) (concatenate 'string a x b)) y))
        (format nil "~{~A~^ ~}" x)))

(defmacro |true| (&optional &rest x) `(values t ,@x))
(defmacro |:| (&optional &rest x) `(values ,@(if x x '(0))))

(defmacro |false| (&optional &rest x) `(values nil ,@x))

(defmacro |load| (f) `(print-eval (parse-file (if (symbolp ,f) (princ-to-string ,f) ,f))))

(defmacro |form| (&rest xs) `(format nil (princ-to-string ,(car xs)) ,@(cdr xs)))

(defun |shift| (&optional (n 1)) (let ((x (car |*|))) (setf |*| (cdr |*|)) x)) 

(defun |glob| (g) (directory (princ-to-string g))) 

(defmacro |tmpf| (&rest cmd)
  (let ((f (gensym)))
    `(with-open-temporary-file (,f :direction :io)
      (| | ,@cmd ,f))))

(defmacro |read| () `(read-line *standard-input* nil nil))
(defmacro |read-all| () `(alexandria:read-stream-content-into-string *standard-input*))
(defmacro |read-char| () `(read-char *standard-input* nil nil))

(defun |cd| (p) (sb-posix:chdir (princ-to-string p)))
(defun |getenv| (v) (posix-getenv (princ-to-string v)))

(defmacro |loop| (&rest xs)
  `(loop ,@(trans-loop-symbols xs)))

(defun trans-loop-symbols (xs)
  (let ((x (car xs)) (xs (cdr xs)))
    (cond ((eq x nil) nil)
          ((member x '('|for| '|as| '|with|) :test #'equal)
            `(,(|to-lisp-symbol| x) ,(eval (car xs)) ,@(trans-loop-symbols (cdr xs))))
          ((equal x ''|named|)
            `(named ,(eval (car xs)) ,@(trans-loop-symbols (cdr xs))))
          ((equal x ''|using|)
            (let ((y (car xs)))
              `(using (,(|to-lisp-symbol| (second y)) ,(eval (third y))) ,@(trans-loop-symbols (cdr xs)))))
          ((and (listp x) (eq 'quote (car x))) (cons (|to-lisp-symbol| x) (trans-loop-symbols xs)))
          (t (cons x (trans-loop-symbols xs))))))

(defun |idx| (&rest xs)
  (let ((idxs (|init| xs)) (x (car (last xs))))
    (apply #'values (if (hash-table-p x)
      (mapcar #'(lambda (i) (gethash i x)) idxs)
      (mapcar #'(lambda (i) (elt x i)) idxs)))))

(defun |dict| (&rest xs)
  (let ((m (make-hash-table)))
    (apply #'|modf| `(,m ,@xs))))

(defun |modf| (x &rest xs) 
  (let ((n (car xs)) (v (cadr xs)))
    (unless v (return-from |modf| x))
    (if (hash-table-p x) (setf (gethash n x) v)
                         (setf (elt x n) v))
    (apply #'|modf| `(,x ,@(cddr xs)))
    x))

(defun |udict| (x)
  (loop for k being each hash-key of x
      using (hash-value v)
      collect k
      collect v))

(defun |rem| (x y)
  (remhash x y)
  y)

(import-func <  |lt|)
(import-func <= |le|)
(import-func >  |gt|)
(import-func >= |ge|)

(import-func concatenate |concat|)

(defun |echo| (&rest xs)
  (let ((s (format nil "~{~A~^ ~}" xs)))
    (format t (if (equal (elt s (- (length s) 1)) #\Newline) "~A" "~A~%") s)))

(defun |num| (x)
  (if (numberp x) x
      (let ((n (read-from-string (if (symbolp x) (princ-to-string x) x))))
        (if (numberp n) n x))))

(defun repl ()
  (kmrcl:set-signal-handler 2 (lambda (&rest _) ()))
  (tagbody retry
    (handler-bind
      ((end-of-file (lambda (c) (sb-ext:quit :recklessly-p t))) 
       (error (lambda (c) (format *error-output* "~A~%" c)
                          (go retry))))
      (loop (format t "@ ")
            (force-output)
            (print-eval (|parse| (read-line)))))))

(defun build (file code)
  (eval `(defun argo-main ()
           (let ((|*| (cdr sb-ext:*posix-argv*)))
             (unwind-protect ,code (funcall *exit*)))))
  (save-lisp-and-die file :toplevel #'argo-main :executable t :purify t))

(defun parse-file (file)
  (|parse| (alexandria:read-file-into-string file)))

(defun |parse| (code)
  (parse-with-lexer (nsh-lexer (string-trim " 	
;" code)) nsh-parser))

(defun main ()
  (in-package :cl-nsh)
  (setf sb-ext:*invoke-debugger-hook*  
        (lambda (condition hook)
          (declare (ignore hook))
          (format *error-output* "~A~%" condition)
          (sb-ext:quit :recklessly-p t)
  ))
  (multiple-value-bind (out-args out-opts errors)
    (getopt:getopt (cdr sb-ext:*posix-argv*) '(("c" :REQUIRED) ("b" :REQUIRED) ("x" :NONE)))
    (cond
      (errors (format t "Usage: ~a [-b FILE] {-c CODE|FILE}~%" (car sb-ext:*posix-argv*)))
      ((assoc "x" out-opts :test #'string=)
        (save-lisp-and-die "argo" :toplevel #'main :executable t :purify t))
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
  (handler-bind ((warning (lambda (x) (muffle-warning x))))
    (when t;nil
      (print p)
      (princ #\newline)
      (print (sb-cltl2:macroexpand-all p))
      (princ #\newline))
    (unwind-protect
        (eval p)
      (funcall *exit*))))

(main)
