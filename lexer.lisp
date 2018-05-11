;;;;lexer

(defun lexify (file)
  "Given SPECS, classify each token in FILE, producing a token list."
  (with-open-file (stream file)
    (do ((sub (read-token stream)
              (read-token stream)))
        ((string= "" sub))
      (print sub))))

(defun read-token (stream)
  "Reads a token from STREAM."
  (with-output-to-string (s-stream)
    ;;remove all whitespace
    (do-stream (char stream) ((not (whitespace-p char))
                              (when (characterp char)
                                (unread-char char stream))))
    ;;then read token
    (do-stream (char stream) ((whitespace-p char))
      (princ char s-stream))))

(defmacro do-stream ((var stream) (test &optional (result nil result-supplied-p)) &body body)
  "Concise way to iterate over STREAM. Iterates over each character; stops when eof is found or
   TEST is true, and returns RESULT. Performs BODY after each time VAR is set."
  `(do ((,var (read-char ,stream nil nil)
              (read-char ,stream nil nil)))
       ,(if result-supplied-p
            `((or (null ,var) ,test) ,result)
            `((or (null ,var) ,test)))
     ,@body))

(defun whitespace-p (char)
  "Returns true if CHAR is a whitespace character."
  (or (not (graphic-char-p char)) (char= char #\Space)))