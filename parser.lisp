(defvar token-list '() "Represents the sentence to be checked.")
(defvar branches '() "A list of lists of token-lists and branches.")
(defvar whole-branch '() "The entire current branch.")
(defvar grammar '() "An alist of the definitions of the nonterminals.")

(defun parse-symbol (sym)
  (eq sym (pop token-list)))

;;all ORs branch apart
(defun parse-branch (branch)
  "Completely parses BRANCH with GRAMMAR.
   BRANCH represents the things to be checked against TOKEN-LIST."
  (format t "tl: ~a~%branch: ~a~%" token-list whole-branch)
  (cond 
    ((assoc (car branch) grammar)
     (let ((expand (copy-tree (cadr (assoc (car branch) grammar)))))
       (setf (car branch) (car expand)
             (cdr branch) (cdr expand)))
     (parse-branch branch))
    ((eq 'e (car branch)))
    ((eq 'and (car branch))
     (cond
       ((null (cdr branch)))
       ((parse-branch (cadr branch))
        (if (cdddr branch)
            (setf (cdr branch) (cddr branch))
            (setf (car branch) (caaddr branch)
                  (cdr branch) (cdaddr branch)))
        (parse-branch branch))))
    ((eq 'or (car branch))
     (let ((test (pop (cdr branch))))
       (when (cdr branch)
         (unless (cddr branch)
           (setf (car branch) (caadr branch)
                 (cdr branch) (cdadr branch)))
         (push (list token-list (copy-tree whole-branch)) branches))
       (setf (car branch) (car test)
             (cdr branch) (cdr test)))
     (parse-branch branch))
    (t (parse-symbol (car branch)))))

(defun parse-branches ()
  "Given an initial BRANCHES, parses through all the branches
   and returns t if TOKEN-LIST is a correct sentence following GRAMMAR."
  (do ()
      ((null branches))
    (format t "branches: ~a~%" branches)
    (let* ((part (pop branches))
           (token-list (car part))
           (whole-branch (cadr part)))
      (when (and (parse-branch whole-branch)
                 (null token-list))
        (return t)))))

;;preprocess grammar: terminals and nonterminals should have parens around them
#|
'(a b)
(s)
(or (and a s b)
    e)
(and a s b)
(and s b)
(and (or (and a s b)
         e)
     b)
(and (and a s b)
     b)
nil

(and (or e)
     b)
(and e b)
(and b)
t
|#
(defun parse-ab (tokens)
  (let ((grammar '((s (or (and (a) (s) (b))
                       (e)))))
        (branches (list (list tokens (list 's)))))
    (parse-branches)))

(defun parse-backtrack (tokens)
  (let ((grammar '((s (and (or (e) (a)) (b)))))
        (branches (list (list tokens (list 's)))))
    (parse-branches)))
