(in-package :grph)

(declaim (fset:map nilmap) (fset:set nilset))
(defconstant nilmap (empty-map))
(defconstant nilset (empty-set))
; (defconstant anymap '(fset:map (:_ t)))

(declaim (inline -del -add grph prop grph))


(defun prt (o &optional s)
  (declare (notinline grph-num-edges))
  (format s "<@grph: (verts: ~a, edges: ~a)>" (@vnum o) (@enum o)))

; TODO: improve this
; this is silly, we should ensure that props accepts at least an fset:map
(defun props-as-list (m &aux (res (list)))
  (unless m (return-from props-as-list nil))
  (fset:do-map (k v m) (push (list k v) res))
  res)

(defstruct (grph
  (:constructor grph (&optional (adj nilmap) (num-edges 0)
                                (props nilmap) (mid nilmap)))
  (:print-object prt))
  "create a directed graph instance with no spatial awareness.

assuming the following graph, where all edges are undirected:

  x-y-u
  |   |
a-b-c-d-o
  |
  y

this terminology is used :
  - ab, by and do are (simple) filaments.
  - bcd and bxyud are segments.
  - (simple) filaments are segments.
  - bcduyx(b) is a cycle.
  - b and d are multi intersection points/vertices
  - a, y, o are dead-ends.
  - a, b, c, y are incident of b
"
  (adj nilmap :type fset:map :read-only t)
  (props nilmap :type fset:map :read-only t)
  (mid nilmap :type fset:map :read-only t)
  (num-edges 0 :type pn :read-only t))

; GET / LOOKUP

(defun @mem (g a b &aux (eset (@ (adj g) a)))
  (declare (grph g) (pn a b))
  "t if edge (a b) exists."
  (and eset (fset:@ eset b)))

(defun @prop (g k &optional p)
  (declare (grph g))
  "get val of prop, p, for key, k. should be an edge k=(a b) or a vert, k=a."
  (if p (get-multi-rel (props g) k :prop p)
        (get-multi-rel (props g) k)))

(defun @mid (g k &optional p)
  (declare (grph g))
  "get val of prop, p, for key, k. should be an edge k=(a b)"
  (if p (get-multi-rel (mid g) k :prop p)
        (get-multi-rel (mid g) k)))

(defun @enum (g)
  (declare (grph g))
  "total number of edges in graph."
  (grph-num-edges g))

(defun @vnum (g &aux (res 0))
  (declare (grph g) (pn res))
  "count all connected verts."
  (itr-verts (g a) (incf res)) res)

(defun @vmax (g &aux (res 0))
  (declare (grph g) (pn res))
  "get highest vertex index."
  (itr-verts (g a) (setf res (max a res))) res)

(defun @edges (g &aux (res (list)))
  (declare (grph g) (list res))
  "list of lists of all edges."
  (itr-edges (g e) (push e res)) res)

(defun @verts (g &aux (res (list)))
  (declare (grph g) (list res))
  "list of all connected verts."
  (itr-verts (g a) (push a res)) res)

(defun @out (g a &aux (res (list)))
  (declare (grph g) (pn a) (list res))
  "list all outboud verts of a."
  (itr-adj (g a b ->) (push b res)) res)
(defun @in (g a &aux (res (list)))
  (declare (grph g) (pn a) (list res))
  "list all outboud verts of a."
  (itr-adj (g a b <-) (push b res)) res)
(defun @both (g a &aux (res (list)))
  (declare (grph g) (pn a) (list res))
  "list all verts of a that are bi-directional."
  (itr-adj (g a b <>) (push b res)) res)
(defun @either (g a &aux (res (list)))
  (declare (grph g) (pn a) (list res))
  "list both inbound and outbond verts of a."
  (itr-adj (g a b ><) (push b res)) res)

; MUTATE

; TODO: clear edge props fx?

; TODO: assert edge exists?
(defun prop (g k props)
  (declare #.*opt* (grph g) (list k props))
  "set prop, p, of edge or vert, k."
  (labels ((with-prop (p val)
             (declare (keyword p))
             (if (not (eq p :_))
                 (grph (adj g) (grph-num-edges g)
                     (set-multi-rel (props g) k p val)
                     (set-multi-rel (mid g) p k))
                 g)))
    (loop for p* in props
          do (setf g (etypecase p*
                       (cons (dsb (p val) p* (with-prop p val)))
                       (keyword (with-prop p* t)))))
    g))

(defun -add (g a b)
  (declare #.*opt* (grph g) (pn a b))
  (grph (set-multi-rel (if (@mem g b a) (adj g)
                           (set-multi-rel (adj g) b a nil))
                       a b t)
        (1+ (grph-num-edges g))
        (props g) (mid g)))

; alter all refs to add
(defun add (g a b &optional props) ; option to force set prop?
  (declare #.*opt* (grph g) (pn a b) (list props))
  "new edge (a b). optionally set prop, p, (with val).
returns: (values g created?)"
  (when (= a b) ;(warn "ADD: ignoring incorrect edge: (~a ~a)." a b)
                (return-from add (values g nil)))
  (if (@mem g a b) (values (prop g (list a b) props) nil)
                   (values (prop (-add g a b) (list a b) props) t)))

(defun -del (g a b)
  (declare #.*opt* (grph g) (pn a b))
  (labels
    ((prune-props (&aux (mid (mid g)) (ab `(,a ,b)))
       (do-map (p _ (or (@prop g ab) nilmap)) ; ignore _=v
         (declare (ignorable _))
         (setf mid (del-multi-rel mid p ab)))
       (let ((adj (if (@mem g b a) (adj g)
                    (del-multi-rel (adj g) b a))))
        (grph (del-multi-rel adj a b)
              (1- (grph-num-edges g))
              (del-multi-rel (props g) ab)
              mid))))
    (if (@mem g a b) (values (prune-props) t)
                     (values g nil))))

(defun del (g a b) ; option to dont delete props?, option to force del prop?
  (declare (grph g) (pn a b))
  "delete edge (a b). deletes associated props.
returns: (values g deleted?)"
  (when (= a b) (warn "DEL: incorrect edge: (~a ~a)." a b))
  (-del g a b))

; TODO: what happens with dangling mid/props values?
(defun -del-prop (g ab prop)
  (declare (grph g) (list ab) (symbol prop))
  (if (@prop g ab prop)
      (values (grph (adj g)
                    (grph-num-edges g)
                    (del-multi-rel (props g) ab prop)
                    (del-multi-rel (mid g) prop ab))
              t)
      (values g nil)))

(defun del-props (g ab props)
  (declare (grph g) (list ab props))
  (loop with deleted? = nil
        for p in props
        do (mvb (g* del?) (-del-prop g ab p)
             (setf g g* deleted? (or del? deleted?)))
        finally (return-from del-props (veq:vpr (values g deleted?)))))

; VARIOUS ---

(defun grp (val &optional (s :g))
  (declare (symbol val s))
  "for val = :black and s = :color, creates list of two props
((:color :black) :black). this is useful for making both :colour and :black
filterable via @prop or in queries.

eg: (add! g a b (grp :black :color))"
  `((,s ,val) ,val))

(defun ingest-edges (g f)
  (declare (list f))
  "ingest a list of edges with props. eg: ((0 :a 3) (8 :_ 9))."
  (loop for (l p r) in f
        do (cond ((not (@mem g l r))
                    (add! g l r (if (not (eq (kv p) :_)) (list p))))
                 ((not (@prop g (list l r) p))
                    (add! g l r (list p)))))
  g)

