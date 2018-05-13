(defun parse (token-list grammar)
  "Parses TOKEN-LIST with GRAMMAR. Returns a syntax tree."
  )

(defvar token-list)

(defmacro protect (test)
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


((goal assignment)
 (assignment (and lhs "=" rhs ";"))
 (lhs identifier)
 (rhs (or (identifier)
          (number))))

(defun parse-assign (tokens)
  (setf token-list tokens)
  (parse-goal))

(defun parse-goal ()
  (protect
   (parse-assignment)))

(defun parse-assignment ()
  (pand
   (parse-lhs)
   (parse-string "=")
   (parse-rhs)
   (parse-string ";")))

(defun parse-lhs ()
  (parse-identifier))

(defun parse-rhs ()
  (por (parse-identifier)
       (parse-number)))

(defun parse-identifier ()
  (protect
   (alpha-char-p (char (pop token-list) 0))))

(defun parse-number ()
  (protect
   (digit-char-p (char (pop token-list) 0))))

(defun parse-string (string)
  (protect
   (string= string (pop token-list))))


((s (or (and a s b)
        empty)))

(defun parse-ab (tokens)
  (setf token-list tokens)
  (and (parse-s)
       (null token-list)))

(defun parse-s ()
  (por (pand
        (parse-symbol 'a)
        (parse-s)
        (parse-symbol 'b))
       t))

(defun parse-symbol (symbol)
  (protect
   (eq symbol (pop token-list))))
