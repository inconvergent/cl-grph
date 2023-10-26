(in-package :grph)

(defun list->fset (ll &optional (fx #'identity))
  (declare (list ll) (function fx))
  "make an fset with every (fx o) for every o in ll. see fset->list."
  (loop with res = (fset:empty-set)
        for l in ll do (setf res (fset:with res (funcall fx l)))
        finally (return res)))

(defun fset->list (ss &optional (fx #'identity) &aux (res (list)))
  (declare (fset:set ss) (function fx))
  "inverse of list->fset."
  (do-set (o ss) (push (funcall fx o) res)) res)

(defun edge-set->ht (es &optional (ht (make-hash-table :test #'equal)))
  (declare (list es) (hash-table ht))
  "convert edge set to hash table."
  (loop for (a b) in es
        do (setf (gethash (if (< a b) `(,a ,b) `(,b ,a)) ht) t))
  ht)
(defun ht->edge-set (ht)
  (declare (hash-table ht))
  "inverse of edge-set->ht."
  (loop for e being the hash-keys of ht collect e))

(defun path->edge-set (path &key closed)
  (declare (list path) (boolean closed))
  "return edge set from cycle.
ex: (1 2 3 4 5) -> ((1 2) (2 3) (3 4) (4 5))
if closed is t, (1 5) will be included in the above output."
  (loop for a in path
        and b in (if closed (cons (first (last path)) path) (cdr path))
        collect (list a b)))


(defun -edge-map (es)
  (declare (list es))
  (let ((edge-map (make-hash-table :test #'equal)))
    (labels ((-insert (a b)
               (multiple-value-bind (_ exists) (gethash a edge-map)
                 (declare (ignore _))
                 (if exists (push b (gethash a edge-map))
                            (setf (gethash a edge-map) (list b))))))
      (loop for (a b) in es do (-insert a b)
                               (-insert b a)))
    edge-map))

; NOTE: this assumes the edge set is bidirectional
; TODO: this can cause heap overflow on manifold paths
(defun edge-set->path (es)
  (declare (list es))
  "convert edge set: ((3 4) (4 5) (5 6) (1 2) (6 1) (2 3))
into a path: (4 5 6 1 2 3)
second result is a boolean for whether it is a cycle."

  (when (< (length es) 2)
        (return-from edge-set->path (values (car es) nil)))

  (let ((edge-map (-edge-map (cdr es))))
    (labels
      ((-next-vert-from (a &key but-not)
         (car (remove-if (lambda (v) (= v but-not))
                         (gethash a edge-map))))
       (-until-dead-end (a but-not)
         (loop with prv = a
               with res = (list prv)
               with nxt = (-next-vert-from a :but-not but-not)
               until (equal nxt nil)
               do (push nxt res)
                  (let ((nxt* (-next-vert-from nxt :but-not prv)))
                    (setf prv nxt nxt nxt*))
               finally (return res))))

      (destructuring-bind (a b) (car es)
        (let ((left (-until-dead-end a b)))
          (when (and (= (car left) b) (= (car (last left)) a))
                ; this is a cycle
                (return-from edge-set->path (values left t)))
          ; not a cycle
          (let* ((right (-until-dead-end b a))
                 (res (concatenate 'list left (reverse right))))
            ; this isnt an exhaustive manifold test?
            ; and it should be configurable whether it fails?
            (unless (= (1- (length res)) (length es))
                    (error "path is manifold or incomplete:~%~a~% eslen: ~a. pathlen ~a"
                           res (length es) (length res)))
            (values res nil)))))))

