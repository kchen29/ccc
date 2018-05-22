(defvar token-list)
;;branches is a list of lists of token-lists and branches
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
  (format t "tl: ~a~%branch: ~a~%" token-list branch)
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


(defun test-branches ()
  (do ()
      ((null branches))
    (format t "branches: ~a~%" branches)
    (let* ((part (pop branches))
           (token-list (car part))
           (branch (cadr part)))
      (when (and (parse-branch)
                 (null token-list))
        (return-from test-branches t)))))

;;classic
#|
((s (or (and a s b)
        empty)))
|#

(defun parse-ab (tokens)
  (let ((grammar '((s (or (and a s b)
                       e))))
        (branches (list (list tokens (list 's)))))
    (test-branches)))

;;backtrack
#|
((s (and (or e a) b)))
|#

(defun parse-backtrack (tokens)
  (let ((grammar '((s (and (or e a) b))))
        (branches (list (list tokens (list 's)))))
    (test-branches)))
