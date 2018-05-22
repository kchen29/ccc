(defvar token-list)
(defvar branches)
(defvar branch)

(defun parse-symbol (sym)
  (eq sym (pop token-list)))

(defun parse-branch (grammar)
  "Evaluates BRANCH with GRAMMAR.
   BRANCH represents the things to be checked against the token-list.
   GRAMMAR is an alist of the definitions of the nonterminals.
   PARSE-BRANCH parses first form. Evaluates and removes it."
  (cond
    ((assoc (car branch) grammar)
     (setf (car branch) (copy-tree (cadr (assoc (car branch) grammar))))
     (parse-branch grammar))
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
           (if (parse-branch grammar)
               (parse-branch grammar)
               (progn (pop branch)
                      nil)))))
    ((eq 'or (caar branch))
     )
    (t (error "Unknown branch: ~a" branch))))


(defun test-branch (tokens test-branch test-grammar)
  (let ((token-list tokens)
        (branch test-branch)
        (grammar test-grammar))
    (and (parse-branch grammar)
         (null token-list))))
