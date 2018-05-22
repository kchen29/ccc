(defmacro parser-interpret (token-list grammar)
  "Grammar is defined as a list of clause definitions.
   Each clause form is the nonterminal followed by its definition."
  )

(defvar token-list () "Holds the token list.")
(defvar tree () "Holds the current tree.")

;;classic
#|
((s (or (and a s b)
        empty)))
|#
(defun parse-ab (tokens)
  #|(parser-interpret token-list ((s (or (and a s b)
  e))))|#
  (let ((grammar '((s (or (and a s b)
                       t))))
        (token-list tokens)
        (tree (list 's)))
    (let ((branches (list (list tree token-list))))
      (labels (
               (eval-branch (branch)
                 (cond
                   ((null branch))
                   ((eq 'and (car branch))
                    (cond
                      ((null (cdr branch)))
                      ((eval-branch (listify (cadr branch)))
                       (setf (cdr branch) (cddr branch))
                       (eval-branch branch))))
                   ((eq 'or (car branch))
                    (cond
                      ((null (cdr branch)))
                      ((let ((hold token-list))
                         (if (eval-branch (listify (cadr branch)))
                             (progn (setf (cdr branch) (cddr branch))
                                    (push (list (copy-tree tree) hold) branches))
                             (progn (setf (cdr branch) (cddr branch)
                                          token-list hold)
                                    nil))))
                      (t (eval-branch branch))))
                   ((assoc (car branch) grammar)
                    (let ((expansion (cadr (assoc (car branch) grammar))))
                      (setf (car branch) (car expansion)
                            (cdr branch) (copy-tree (cdr expansion))))
                    (eval-branch branch))
                   ((eq 'e (car branch)))
                   ((atom (car branch))
                    (eq (car branch) (pop token-list)))
                   (t (error "Unknown branch ~a" branch))))
               (test-branch ()
                 (cond
                   ((null branches) nil)
                   ((let* ((branch (pop branches))
                           (tree (car branch))
                           (token-list (cadr branch)))
                      (and (eval-branch tree)
                           (null token-list))))
                   (t (test-branch)))))
        (test-branch)))))

;;backtrack
#|
((s (and (or e a) b)))
|#
