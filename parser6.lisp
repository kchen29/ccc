(defvar token-list)
;;branches is a list of lists of token-lists and branches
(defvar branches)
(defvar whole-branch)
(defvar grammar)

(defun parse-symbol (sym)
  (eq sym (pop token-list)))

;;all ORs branch apart
(defun parse-branch (branch)
  "Completely parses the given BRANCH."
  (format t "tl: ~a~%branch: ~a~%" token-list whole-branch)
  (cond 
    ((assoc (car branch) grammar)
     (let ((expand (copy-tree (cadr (assoc (car branch) grammar)))))
       (setf (car branch) (car expand)
             (cdr branch) (cdr expand)))
     (parse-branch branch))
    ((eq 'e (car branch)))
    ((eq 'and (car branch))
     (if (null (cdr branch))
         t
         (when (parse-branch (cadr branch))
           (setf (cdr branch) (cddr branch))
           (parse-branch branch))))
    ((eq 'or (car branch))
     (let ((test (pop (cdr branch))))
       (when (cdr branch)
         (push (list token-list (copy-tree whole-branch)) branches))
       (setf (car branch) (car test)
             (cdr branch) (cdr test)))
     (parse-branch branch))
    (t (parse-symbol (car branch)))))

(defun test-branches ()
  (do ()
      ((null branches))
    (format t "branches: ~a~%" branches)
    (let* ((part (pop branches))
           (token-list (car part))
           (whole-branch (cadr part)))
      (when (and (parse-branch whole-branch)
                 (null token-list))
        (return t)))))

;;preprocess grammar: terminals and nonterminals have parens around them

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

(defun parse-ab (tokens)
  (let ((grammar '((s (or (and (a) (s) (b))
                       (e)))))
        (branches (list (list tokens (list 's)))))
    (test-branches)))

(defun parse-backtrack (tokens)
  (let ((grammar '((s (and (or (e) (a)) (b)))))
        (branches (list (list tokens (list 's)))))
    (test-branches)))
