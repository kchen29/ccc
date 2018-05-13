;;implementation of mdl language

(defun classifier (str)
  (cond
    ((member str '("rotate" "move") :test #'string=) (make-symbol (string-upcase str)))
    ((alpha-char-p (char str 0)) '#:symbol)
    ((digit-char-p (char str 0)) '#:number)
    (t (error "~a does not indicate a token" str))))

;;mdl grammar
((input (or nil
            (command input)))
 (command (or (rotate symbol number)
              (move number number number))))

((input (* (command)))
 (command (or (rotate symbol number)
              (move number number number))))

(defun parse-mdl (token-list)
  "Hand-written parser for mdl. Returns a list where each car is a command."
  (labels ((parse-input ()
             (loop while token-list
                   collect (parse-command)))
           (parse-command ()
             (loop for test in '((rotate symbol number)
                                 (move number number number))
                   until (match test token-list :test #'name=)
                   finally (return (let ((hold token-list)
                                         (end (nthcdr (1- (length test)) token-list)))
                                     (setf token-list (cdr end)
                                           (cdr end) nil)
                                     hold)))))
    (parse-input)))

(defun match (seq1 seq2 &key (test #'eql))
  "Returns t if the seq1 is a subsequence of seq2 starting at index 0."
  (loop for obj1 in seq1
        for i = 0 then (1+ i)
        for obj2 in seq2
        for j = 0 then (1+ j)
        always (funcall test obj1 obj2)
        finally (return (= i j))))

(defun name= (sym1 sym2)
  "Returns if SYM1 and SYM2 have string= symbol names."
  (string= (symbol-name sym1) (symbol-name sym2)))
