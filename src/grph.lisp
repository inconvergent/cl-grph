(in-package :grph)

(declaim (fset:map nilmap) (fset:set nilset))
(defconstant nilmap (empty-map))
(defconstant nilset (empty-set))
; (defconstant anymap '(fset:map (:_ t)))

(declaim (inline -del -add grph prop grph))

(defun prt (o &optional s)
  (format s "<@grph: (v: ~a/~a, e: ~a, p: ~a)>"
            (@vcnt o) (@vmax o) (@enum o) (@pnum o)))

(defstruct (grph
  (:constructor grph (&optional (adj nilmap) (num-edges 0) (props nilmap) (mid nilmap)))
  (:constructor make (&optional (adj nilmap) (num-edges 0) (props nilmap) (mid nilmap)))
  (:print-object prt))
  "create a graph instance.

assuming the following graph, where all edges are bi directional:

  x-y-u
  |   |
a-b-c-d-o-l
  |
  y

the following terminology is used:
  - ab, by and dol are (simple) filaments
  - bcd and bxyud are segments.
  - (simple) filaments are segments.
  - bcduyx(b) is a cycle.
  - b and d are multi intersection points/vertices
  - a, y, l are dead-ends.
  - a, b, c, y are incident of b"
  (adj nilmap :type fset:map :read-only t)
  (props nilmap :type fset:map :read-only t)
  (mid nilmap :type fset:map :read-only t)
  (num-edges 0 :type pn :read-only t))

(define-struct-load-form grph)
#+SBCL(declaim (sb-ext:freeze-type grph))

; GET / LOOKUP ----------------

(defun @mem (g a b &aux (eset (@ (adj g) a)))
  (declare #.*opt* (grph g) (in a b)) "t if edge (a b) exists."
  (and eset (values (fset:@ eset b))))

; TODO: filter by edge/vert prop?
; TODO: docs
(defun @prop (g k &optional p)
  (declare #.*opt* (grph g) ((or list in) k))
  "get props of k (edge ab or vert); or check if p is a prop of k."
  (if p (get-multi-rel (props g) k :prop p) ; t / nil if prop exists
        (get-multi-rel (props g) k)))
(defun @mid (g k &optional p)
  (declare #.*opt* (grph g))
  "get props of k (edge ab or vert); or check of p is a prop of k."
  (if p (get-multi-rel (mid g) k :prop p) ; t / nil if prop+edge/vert exists
        (get-multi-rel (mid g) k)))

(defun adjcnt (adj &aux (n 0))
  (declare #.*opt* (fset:map adj) (veq:pn n)) "count total number of edges in grph-adj."
  (do-map (a edges adj) (declare (ignorable a))
    (do-map (b dir edges) (declare (ignorable b))
      (when dir (incf n))))
  n)
(defun ecnt (g)
  (declare #.*opt* (grph g)) "count total number of edges in grph."
  (adjcnt (grph-adj g)))
(defun @enum (g)
  (declare #.*opt* (grph g)) "total number of edges in graph."
  (grph-num-edges g))
(defun @pnum (g)
  (declare #.*opt* (grph g)) "total number of props in graph."
  (fset:size (grph-mid g)))
(defun @vcnt (g &aux (res 0))
  (declare #.*opt* (grph g) (pn res)) "count all connected verts."
  (itr-verts (g a) (incf res)) res)

(defun @edges (g &aux (res (list)))
  (declare #.*opt* (grph g) (list res)) "list of lists of all edges."
  (itr-edges (g e) (push e res)) res)
(defun @out (g a &aux (res (list)))
  (declare #.*opt* (grph g) (in a) (list res)) "list all outboud verts of a."
  (itr-adj (g a b ->) (push b res)) res)
(defun @in (g a &aux (res (list)))
  (declare #.*opt* (grph g) (in a) (list res)) "list all inboud verts of a."
  (itr-adj (g a b <-) (push b res)) res)
(defun @both (g a &aux (res (list)))
  (declare #.*opt* (grph g) (in a) (list res)) "list all verts of a that are bi-directional."
  (itr-adj (g a b <>) (push b res)) res)
(defun @either (g a &aux (res (list)))
  (declare #.*opt* (grph g) (in a) (list res)) "list both inbound and outbond verts of a."
  (itr-adj (g a b ><) (push b res)) res)

(defun @vmax (g &aux (res 0))
  (declare #.*opt* (grph g) (pn res)) "get highest vertex index."
  (itr-verts (g a) (when (> a res) (setf res a)))
  res)
(defun @verts (g &aux (res (list)))
  (declare #.*opt* (grph g) (list res)) "list of all connected verts."
  (itr-verts (g a) (push a res)) res)

; TODO: clear edge props fx?
(defun prop (g k props) ; TODO: assert edge exists?
  (declare #.*opt* (grph g) ((or list in) k) ((or keyword list fset:set) props))
  "set prop, p, of edge or vert, k."
  (labels ((with-prop (p)
             (declare (keyword p))
             (if (not (eq p :_)) (grph (adj g) (grph-num-edges g)
                                       (set-multi-rel (props g) k p)
                                       (set-multi-rel (mid g) p k))
                                 g)))
    (etypecase props
      (keyword (setf g (with-prop props)))
      (list (loop for p in props do (setf g (etypecase p (keyword (with-prop p))))))
      (fset:set (do-set (p props) (setf g (etypecase p (keyword (with-prop p)))))))
    g))

; TODO: *prefix* for :/g/ prefix
(defun sprop (&rest rest) (declare #'*opt*)
  "make a special prop. special props have a distinct prefix: :/g/.
and a category. eg: :/g/id/. where :id is the category.
special props behave like all other props, but they can have special
behaviour in some limited cases.

see compound paths in grph:walk macro.

possible future special props are types. eg
  :/g/bzspl/ for bezier curves.
or even :/g/circ/, where one edge denotes a center and radius. which would work
well w/ svg export."
  (apply #'psymb :keyword :/g/ rest))

(defun sprop-id (&optional (sp-cat "^SID^"))
  "generate a unique special prop :/g/id/[gensym]."
  (sprop :id/ (gensym sp-cat)))

(defun unpack-sprop (s)
  (declare #.*opt* (keyword s))
  "unpack eg. :/g/id/abc into values :id and :abc"
  (unless (sprop? s) (error "not a special prop: ~a" s))
  (dsb (ty val) (split-string #\/ (subseq (symbol-name s) 3))
    (declare (string ty val))
    (values (psymb :keyword ty) (psymb :keyword val))))

(defun sprop? (s) (declare #'*opt*)
  "is this a special prop? returns s or nil."
  (typecase s (keyword (let ((str (symbol-name s)))
                         (and (> (length str) 5)
                              (string= str "/G/" :end1 3) s)))))

(defun -add (g a b)
  (declare #.*opt* (grph g) (in a b))
  (grph (set-multi-rel (if (@mem g b a) (adj g)
                           (set-multi-rel (adj g) b a nil))
                       a b t)
        (1+ (grph-num-edges g)) (props g) (mid g)))
(defun add (g a b &optional props) ; option to force set prop?
  (declare #.*opt* (grph g) (in a b) ((or keyword list fset:set) props))
  "new edge (a b). optionally set prop, p.
returns: (values g created?)"
  (when (= a b) (return-from add (values g nil)))
  (if (@mem g a b) (values (prop g (list a b) props) nil)
                   (values (prop (-add g a b) (list a b) props) t)))

(defun -del-adj-both (adj ea eb a b)
  (declare #.*opt* (fset:map ea eb) (in a b))
  (let* ((ea (fset:less ea b)) (eb (fset:less eb a))
         (nila (fset:empty? ea)) (nilb (fset:empty? eb)))
    (declare (fset:map ea eb) (boolean nila nilb))
    (cond ((and nila nilb) (fset:less (fset:less adj b) a))
          (nilb (fset:with (fset:less adj b) a ea))
          (nila (fset:with (fset:less adj a) b eb))
          (t (fset:map (fset:$ adj) (a ea) (b eb))))))
(defun -del-adj (adj a b)
  (declare #.*opt* (fset:map adj) (in a b))
  ; this EXPECTS a->b to exits. but handles the possibility that b<-a exists too
  (let ((ea (@ adj a)) (eb (@ adj b)))
    (declare (fset:map ea eb))
    (if (@ eb a) (fset:with adj a (fset:with ea b nil)) ; a <> b. now set a->b to nil, keep ba
                 (-del-adj-both adj ea eb a b))))
(defun -del (g a b)
  (declare #.*opt* (grph g) (in a b))
  (labels ((prune-props (&aux (mid (mid g)) (ab `(,a ,b)))
             (do-set (p (or (@prop g ab) nilset)) ; ignore _=v
               (setf mid (del-multi-rel mid p ab)))
             (grph (-del-adj (adj g) a b)
               (1- (grph-num-edges g))
               (del-multi-rel (props g) ab)
               mid)))
     (if (@mem g a b) (values (prune-props) t) (values g nil))))

(defun del (g a b) ; option to dont delete props?, option to force del prop?
  (declare #.*opt* (grph g) (in a b))
  "delete edge (a b). deletes associated props.
returns: (values g deleted?)"
  (when (= a b) (warn "DEL: incorrect edge: (~a ~a)." a b))
  (-del g a b))

; TODO: only expose del-props?
; TODO: what happens with dangling mid/props values?
(defun del-prop (g k prop)
  (declare #.*opt* (grph g) ((or list fixnum) k) (symbol prop))
  "delete prop from k."
  (if (@prop g k prop)
      (values (grph (adj g) (grph-num-edges g)
                    (del-multi-rel (props g) k prop)
                    (del-multi-rel (mid g) prop k))
              t)
      (values g nil)))

; TODO: what does deleted? really mean here?
(defun del-props (g k props) ; TODO: optional props to delete all
  (declare #.*opt* (grph g) ((or list fixnum) k) ((or list fset:set) props))
  "delete props from k"
  (let (deleted?)
    (loop for p in (etypecase props (list props) (fset:set (set->lst props)))
          do (mvb (g* del?) (del-prop g k p)
                  (setf g g* deleted? (or del? deleted?))))
    (values g deleted?)))

