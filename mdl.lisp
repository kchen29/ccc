;;implementation of mdl language

(defun classifier (str)
  (cond
    ((member str '("rotate" "move") :test #'string=) (intern (string-upcase str)))
    ((alpha-char-p (char str 0)) 'symbol)
    ((digit-char-p (char str 0)) 'number)
    (t 'unknown)))
