(defvar token-list () "Holds the token list.")
(defvar rest-stack () "Holds the rest of the clauses to match.")
(defvar branches () "Holds the possible branches. List of lists of token-list and rest-stack.")

;;branch off on OR; push TOKEN-LIST and REST-STACK as a list to branches
;;only need OR to keep track of token-lists and rest-stacks
;;if AND returns false, just return that

(defun test-branches ()
  (cond
    ((null branches) nil)
    ((let* ((branch (pop branches))
            (token-list (car branch))
            (rest-stack (cadr branch)))
       (and (funcall (pop rest-stack))
            (null token-list)
            (null rest-stack))))
    (t (test-branches))))

(defmacro pand (&rest rest)
  (if (null rest)
      t
      `(progn
         (push (lambda () (pand ,@(cdr rest))) rest-stack)
         (if ,(car rest)
             (funcall (pop rest-stack))
             (progn (pop rest-stack)
                    nil)))))

;;holds token-list and rest-stack when trying each form
;;branches off on the first t
(defmacro por (&rest rest)
  (if (null rest)
      t
      `(progn
         (push (lambda () (por ,@(cdr rest))) rest-stack)
         (if (let ((hold token-list)
                   (hold2 rest-stack))
               (if ,(car rest)
                   (push (list hold hold2) branches)
                   (progn (setf token-list hold
                                rest-stack hold2)
                          nil)))
             (pop rest-stack)
             (funcall (pop rest-stack))))))

(defun parse-symbol (symbol)
  (eq symbol (pop token-list)))

;;classic
#|
((s (or (and a s b)
        empty)))
|#

(defun parse-ab (tokens)
  (let ((branches (list (list tokens (list (lambda () (parse-s)))))))
    (test-branches)))

(defun parse-s ()
  (por (pand (parse-symbol 'a)
             (parse-s)
             (parse-symbol 'b))
       t))

;;backtrack
#|
((s (and (or e a) b)))
|#

(defun parse-backtrack (tokens)
  (let ((branches (list (list tokens (list (lambda () (parse-s-back)))))))
    (test-branches)))

(defun parse-s-back ()
  (pand (por t (parse-symbol 'a))
        (parse-symbol 'b)))
