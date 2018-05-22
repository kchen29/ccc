(defvar token-list)
(defvar branches)
(defvar branch)
(defvar grammar)

(defun parse-symbol (sym)
  (eq sym (pop token-list)))

(defun parse-branch ()
  "Evaluates BRANCH with GRAMMAR.
   BRANCH represents the things to be checked against the token-list.
   GRAMMAR is an alist of the definitions of the nonterminals.
   PARSE-BRANCH parses first form. Evaluates and removes it."
  (cond
    ((assoc (car branch) grammar)
     (setf (car branch) (copy-tree (cadr (assoc (car branch) grammar))))
     (parse-branch))
    ((atom (car branch))
     (if (eq 'e (car branch))
         (pop branch)
         (eq (pop branch) (pop token-list))))
    ((eq 'and (caar branch))
     (if (null (cdar branch))
         (pop branch)
         (progn     
           (let ((temp (cadar branch)))
             (setf (cdar branch) (cddar branch)
                   branch (cons temp branch)))
           (if (parse-branch)
               (parse-branch)
               (progn (pop branch)
                      nil)))))
    ((eq 'or (caar branch))
     (if (null (cdar branch))
         (pop branch)
         (if (let ((hold token-list))
               (let ((temp (cadar branch)))
                 (setf (cdar branch) (cddar branch)
                       branch (cons temp branch)))
               (if (parse-branch)
                   t
                   (progn (setf token-list hold)
                          nil)))
             (progn (push (copy-tree branch) branches)
                    (pop branches))
             (parse-branch))))
    (t (error "Unknown branch: ~a" branch))))


(defun test-branches ()
  (do (done)
      ((or (null branches) done) done)
    (format t "~a~%" branches)
    (let ((branch (pop branches)))
      (setf done (and (parse-branch)
                      (null token-list))))))

;;classic

((s (or (and a s b)
        empty)))

(defun parse-ab (tokens)
  (let ((token-list tokens)
        (grammar '((s (or (and a s b)
                       e))))
        (branches '((s))))
    (test-branches)))
