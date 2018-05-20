(defvar token-list () "Holds the token list.")
(defvar rest-stack () "Holds the rest of the clauses to match.")

;;OR branches off:
;;the first true value branches off and shadows the testing of the next possible values

(defmacro protect (test)
  "Keep a HOLD of the original TOKEN-LIST. If TEST is false, then set it back."
  (let ((hold (gensym))
        (res (gensym)))
    `(let ((,hold token-list)
           (,res ,test))
       (unless ,res
         (setf token-list ,hold))
       ,res)))

(defmacro por (&rest rest)
  `(protect (or ,@rest)))

(defmacro pand (&rest rest)
  `(protect (and ,@rest)))

(defmacro cor (&rest rest)
  `(protect))

(defmacro cand (&rest rest)
  "Clausal and. Push the cdr of REST to REST-STACK.
  If the car of REST is true, then backtrack to see if it really works.
  If it does, keep the configuration. Otherwise test the next one." 
  (when rest
    `(progn (push (lambda () (cand ,(cdr rest))) rest-stack)
            (let ((hold token-list)
                  (hold2 rest-stack)
                  (res ,(car rest)))
              (cond
                (res
                 (funcall (pop rest-stack))
                 t)
                (t (setf token-list hold
                         rest-stack hold2)
                   (funcall (pop rest-stack)))))
            (if (protect ,(car rest))
                (progn (pop rest-stack)
                       t)
                (funcall (pop rest-stack))))))

(defun test-match ()
  (funcall (pop rest-stack)))

;;classic
((s (or (and a s b)
        empty)))

(defun parse-ab (tokens)
  (let ((token-list tokens)
        (rest-stack ()))
    (and (parse-s)
         (null token-list))))

(defun parse-s ()
  (cor (cand (parse-symbol 'a)
             (parse-s)
             (parse-symbol 'b))
       t))

;;backtrack
((s (and (or e a) b)))


;;backtrack2
((s (and a x b))
 (a (or (and c d)
        c)))

