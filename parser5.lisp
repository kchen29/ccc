(defvar token-list)
;;branches is a list of lists of token-lists and branches
(defvar branches)
;;(defvar branch)
(defvar whole-branch)
(defvar grammar)

(defun parse-symbol (sym)
  (eq sym (pop token-list)))

(defun parse-branch ()
  "Evaluates BRANCH with GRAMMAR.
   BRANCH represents the things to be checked against the token-list.
   GRAMMAR is an alist of the definitions of the nonterminals.
   PARSE-BRANCH parses first form. Evaluates and removes it."
  (format t "tl: ~a~%branch: ~a~%" token-list branch)
  (cond
    ((assoc (car branch) grammar)
     (setf (car branch) (copy-tree (cadr (assoc (car branch) grammar))))
     (parse-branch))
    ((atom (car branch))
     )
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
     (cond
       ((null (cdar branch)) (pop branch))
       ((let ((hold token-list))
          (let ((temp (cadar branch)))
            (setf (cdar branch) (cddar branch)
                  branch (cons temp branch)))
          (if (parse-branch)
              (progn (when (cdar branch)
                       (push (list hold (copy-tree branch)) branches))
                     (pop branch))
              (progn (setf token-list hold)
                     nil))))
       (t (parse-branch))))
    (t (error "Unknown branch: ~a" branch))))

(defun parse-branch-2 (branch)
  (format t "tl: ~a~%branch: ~a~%" token-list whole-branch)
  (cond
    ((atom branch)
     (if (eq 'e branch)
         t
         (parse-symbol branch)))
    ((assoc (car branch) grammar)
     (let ((expand (copy-tree (cadr (assoc (car branch) grammar)))))
       (setf (car branch) (car expand)
             (cdr branch) (cdr expand)))
     (parse-branch-2 branch))
    ((eq 'and (car branch))
     (if (null (cdr branch))
         t
         (when (parse-branch-2 (cadr branch))
           (setf (cdr branch) (cddr branch))
           (parse-branch-2 branch))))
    ((eq 'or (car branch))
     (cond
       ((null (cdr branch)))
       ((let ((hold token-list))
          (if (prog1 (parse-branch-2 (cadr branch))
                (setf (cdr branch) (cddr branch)))
              (push (list hold (copy-tree whole-branch)) branches)
              (progn (setf token-list hold)
                     nil))))
       (t (parse-branch-2 branch))))
    (t (error "Unknown branch: ~a" branch))))

'(a b)
(s)
(or (and a s b)
    e)
(or (and s b)
    e)
(or (and (or (and a s b)
             e)
         b)
    e)
(or (and (or e)
         b)
    e)
(or (and b)
    e)
t

(s)
(or (and a (s) b)
    e)

'()
(s)
(and (or e a) b)
(and b)
;;nil
;;backtrack
(and (or a) b)
;;nil
;;backtrack
;;no more - nil

(defun test-branches ()
  (do ()
      ((null branches))
    (format t "branches: ~a~%" branches)
    (do* ((part (pop branches))
          )
         ((null branch) ())
      
      (when (and (parse-branch)
                 (null token-list))
        (return-from test-branches t)))))

(defun test-branches-2 ()
  (do ()
      ((null branches))
    (format t "branches: ~a~%" branches)
    (let* ((part (pop branches))
           (token-list (car part))
           (whole-branch (cadr part)))
      (when (and (parse-branch-2 whole-branch)
                 (null token-list))
        (return-from test-branches-2 t)))))
;;classic
#|
((s (or (and a s b)
        empty)))
|#

(defun parse-ab (tokens)
  (let ((grammar '((s (or (and a (s) b)
                       e))))
        (branches (list (list tokens (list 's)))))
    (test-branches-2)))

;;backtrack
#|
((s (and (or e a) b)))
|#

(defun parse-backtrack (tokens)
  (let ((grammar '((s (and (or e a) b))))
        (branches (list (list tokens (list 's)))))
    (test-branches-2)))
