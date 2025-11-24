(in-package :grph)

; EDGE-SET ITERATORS
; TODO: all s/itr could use improvements
(defmacro es/itr-sprop ((g sp edges)
                        (es &optional (sp-default :/g/_default/) (sp-cat :id))
                        &body body)
  (declare
    (symbol g sp edges) (keyword sp-cat) ((or keyword null) sp-default))
  "iterate sprop category [eg :id] and a ht of corresponding edges.
if an edge has multiple sprops from the same category [:/g/id/id-1 :/g/id/id-2]
the edge will be included for each sprop."
  (awg (sp->edges es-in)
   `(let ((,es-in ,es)
          (,sp->edges (make-hash-table :test #'eql)))
      (labels ((fnd-sp (c) (and (grph:sprop? c)
                                (eql (grph:unpack-sprop c) ,sp-cat)))
               (add-sp->edge (sp e)
                 (mvb (edges exists) (gethash sp ,sp->edges)
                   (unless exists (let ((new (make-hash-table :test #'equal)))
                                    (setf (gethash sp ,sp->edges) new
                                          edges new)))
                   (setf (gethash e edges) sp))))
        (loop for (a b) in ,es-in
              for e = (srt a b)
              for hit = nil
              do (fset:do-set (c (fset:union
                                   (or (grph:@prop ,g e) (fset:set))
                                   (or (grph:@prop ,g (reverse e)) (fset:set))))
                   (when (fnd-sp c) (add-sp->edge c e)
                                    (setf hit t)))
                 ,@(if sp-default `((unless hit (add-sp->edge ,sp-default e))))))

      (loop for ,sp being the hash-keys of ,sp->edges
            using (hash-value ,edges)
            do (let ((,sp ,sp) (,edges ,edges))
                 (declare (ignorable ,sp ,edges))
                 ,@body)))))

; TODO: there are side-effects in edges. relevant for: es/itr-walk-segments,
; es/itr-sprop
(defun es/itr-walk (p c edges body)
  (declare #.*opt* (symbol p c))
  "INTERNAL. execute body with paths/segments from walks over this edge set in g.
  p is the path, c is t if p is a closed loop.
every edge in es is included in one path only, and only once.
NOTE: edges is destroyed in the process. ignores edge dir."
  (awg (start a b)
   `(labels ((-get-start-edge ()
             (loop for e being the hash-keys of ,edges
                   do (return-from -get-start-edge e)))
            (-next-vert-from (a &key but-not)
              (car (remove-if
                     (lambda (v) (or (= v but-not) (not (gethash (srt a v) ,edges))))
                     (-@either a)))) ; defined in walk
            (-closed? (p) (if (equal (first p) (last* p))
                              (values (cdr p) t)
                              (values p nil)))
            (-until-dead-end (a but-not)
              (loop with prv = a with res = (list prv)
                    with nxt = (-next-vert-from a :but-not but-not)
                    until (equal nxt nil)
                    do (push nxt res)
                       (remhash (srt prv nxt) ,edges)
                       (let ((nxt* (-next-vert-from nxt :but-not prv)))
                         (setf prv nxt nxt nxt*))
                    finally (return res))))
      (loop while (> (hash-table-count ,edges) 0)
            for ,start = (-get-start-edge) for (,a ,b) = ,start
            do (mvb (,p ,c) (-closed?
                              (progn (remhash ,start ,edges)
                                     `(,@(-until-dead-end ,a ,b)
                                          ,@(reverse (-until-dead-end ,b ,a)))))
                    (declare (ignorable ,p ,c) (list ,p) (boolean ,c))
                    ,@body)))))

(defun es/itr-walk-segments (p c edges body)
  (declare #.*opt* (symbol p c))
  "INTERNAL. same as es/itr-walk, but splits p into segments. ignores edge dir."
  (awg (edg p* c*)
   ; NOTE / TODO: this is kinda bad, but remember that edges is destroyed in
   ; es/itr-walk, so we need a copy. fset would be more elegant, but less
   ; efficient probably? might be better to rewrite the walker logic
  `(let ((,edg (copy-hash-table ,edges)))
    (labels ((-with-2cnt (p) (loop for v in p collect (list v (= 2 (-cnt v)))))
             (-split-paths (p c) (rec (-with-2cnt (if c (close-path p) p))))
             (-cnt (v) (loop for w in (-@either v) ; defined in walk
                             if (gethash (srt w v) ,edges) summing 1))
             (-closed? (p) (if (= (first p) (last* p))
                               (values (butlast p) t)
                               (values p nil)))
             (rec (pp) (unless (> (length pp) 1) (return-from rec))
                       (loop for i from 1 for (vi b) in (cdr pp)
                             if (not b) do (return-from rec
                                             (cons (veq:lpos (subseq pp 0 (1+ i)))
                                                   (rec (subseq pp i)))))
                       (list (veq:lpos pp))))
      ,(es/itr-walk p* c* edg
        `((loop for ,p* in (-split-paths ,p* ,c*)
              do (mvb (,p ,c) (-closed? ,p*)
                      (declare (ignorable ,p ,c) (list ,p) (boolean ,c))
                      ,@body))))))))

(defun es/itr (p c edges body)
  (declare #.*opt* (symbol p c))
  "INTERNAL. iterate edges on the same pattern as segments, paths"
  (awg (e) `(loop for ,e being the hash-keys of ,edges
                  do (let ((,p ,e) (,c nil))
                       (declare (ignorable ,c ,p) (list ,p))
                       ,@body))))



; NOTE / TODO: it would be better to make versions of es/itr-walk etc that
;              preserve edge direction when walking.
(defmacro walk ((g &optional (p (gensym "PATH")) (c (gensym "CLOSED?"))
                             (sid (gensym "SID")))
                (modes &key (prop :_ prop?) (es nil es?) (sp-cat :id) (sp-default :/g/_default/)
                       &aux (modes (valid-modes :walk modes
                                    '(:progn :collect :dir :any
                                      :paths :segments :edges :compound :keep :drop))))
                &body body)
  (declare (symbol g p c) (keyword prop sp-cat sp-default))
  "walk edges in graph as tuples of (p c). where p is a path and c is t if p is closed.

ex:

  (grph:walk (g) (collect prop path))

  (grph:walk (g p c)
             ((dir segments) :prop :path)
    (print (list (reverse p) c)))

modes:

* :progn    : don't collect result.                                           [default]
  :collect  : collect results as a list.

* :paths    : greedily walk to make as long paths as possible. verts can      [default]
              be visited multiple times. handles [pure] loops.
  :segments : split paths into segments. handles [pure] loops
  :edges    : just return edges

* :simple   : dont group by special prop                                      [default]
  :compound : group by special prop [cat :id / :/g/id/]
* :keep     : keep non-compond paths in :/g/_default/                         [default]
  :drop     : drop non-compound paths

* :any      : use paths as they come out of the walker                        [default]
  :dir      : greedily attempt to align path direction with edges in the
              graph by comparing the first edge in p with the graph, and
              aligning the path to the edge direction.
              useful for loops, and with the :segments mode.
  " (awg (res edges es* p* c*)
  (unless body (setf body `((list ,p ,c))))
  ; this might be fine. but i have not checked. depends on some of the labels below
  (when (and prop? es?) (warn "WALK: can not use :prop and :es simulataneously
                                ~a ~a" prop? es?))
  (labels ((do-es/itr (body*)
             (let ((proc   (ecase (select-mode modes '(:any :dir))
                                  (:dir  `(coerce-dir ,p* ,c* ,sid))
                                  (:any  `(values ,p* ,c* ,sid))))
                   (es/itr (ecase (select-mode modes '(:paths :segments :edges))
                                  (:segments #'es/itr-walk-segments)
                                  (:paths    #'es/itr-walk)
                                  (:edges    #'es/itr))))
               (funcall es/itr p* c* edges
                 `((mvb (,p ,c) ,proc
                        (declare (ignorable ,p ,c) (list ,p) (boolean ,c))
                        ,body*))))))

    `(let* ((,res (list))
            (,sid ,sp-default)
            (,es* (cond (,es? (es/normalize ,es))
                        (,(any? prop) (edge-set ,g))
                        (t (edge-set ,g ,prop))))
            (,edges (edge-set->ht ,es*)))
       (declare (ignorable ,edges ,sid)
                (symbol ,sid) (keyword ,sid) (list ,es*) (hash-table ,edges))
       ; this is messy ...
       (labels ((ctx/prop (e) ; if prop is :_ we can check for
                  ,(if (any? prop) `(apply #'@mem ,g e)
                                   `(@prop ,g e ,prop)))
                (ctx/mem (e sid)
                   (if (eql sid ,sp-default) (ctx/prop e)
                                             (and (@prop ,g e sid) (ctx/prop e))))
                (coerce-dir (pth c sid)
                   (if (ctx/mem (subseq pth 0 2) sid) (values pth c sid)
                                                      (values (reverse pth) c sid)))
                (-@either (v)
                  ,(if (any? prop)
                       `(@either ,g v)
                       `(loop for w in (@either ,g v)
                              if (@prop ,g (list w v) ,prop) collect w
                              else if (@prop ,g (list v w) ,prop) collect w))))
         ,(ecase (select-mode modes '(:simple :compound))
                 (:simple #1=(do-es/itr (ecase (select-mode modes '(:progn :collect))
                                               (:collect `(push (progn ,@body) ,res))
                                               (:progn   `(progn ,@body)))))
                 (:compound `(es/itr-sprop (,g ,sid ,edges)
                                (,es* ,(and (eq :keep (select-mode modes '(:keep :drop)))
                                            sp-default)
                                      ,sp-cat)
                                ,#1#))))
       ,res))))

