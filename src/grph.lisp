(in-package :grph)

(declaim (fset:map nilmap) (fset:set nilset))
(defconstant nilmap (empty-map))
(defconstant nilset (empty-set))

(declaim (inline -del -add grph prop))


(defun -print-grph (o s)
  (declare (notinline grph-num-edges))
  (format s "<@grph: (verts: ~a, edges: ~a)>" (@vnum o) (@enum o)))

; TODO: ignore incorrect edge?
(defstruct (grph
  (:constructor grph (&optional (adj nilmap) (num-edges 0) (props nilmap)))
  (:print-object -print-grph))
  "create undirected graph instance with no spatial awareness.

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
  (num-edges 0 :type pn :read-only t))

(defun @mem (g a b &aux (eset (@ (adj g) a)))
  (declare (grph g) (pn a b))
  "t if edge (a b) exists."
  (and eset (contains? eset b)))

(defun @prop (g k p)
  (declare (grph g) (symbol p))
  ; optional second arg
  "get prop, p, of key, k. k should be an edge k=(a b) or a vert, k=a."
  (get-multi-rel (props g) k :prop p))

(defun @enum (g)
  (declare (grph g))
  "total number of edges in graph."
  (grph-num-edges g))

(defun @vnum (g &aux (res 0))
  (declare (grph g) (pn res))
  "list all connected verts."
  (itr-verts (g a) (incf res)) res)

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
  (itr-out (g a b) (push b res)) res)

(defun prop (g k p &optional (val t))
  (declare (grph g) (list k) (symbol p))
  "set prop, p, of edge or vert, k."
  (grph (adj g) (grph-num-edges g)
        (set-multi-rel (props g) k p val)))

(defun -add (g a b)
  (declare (grph g) (pn a b))
  (grph (set-multi-rel (adj g) a b)
        (1+ (grph-num-edges g)) (props g)))

(defun add (g a b &optional p (val t))
  (declare (grph g) (pn a b))
  "new edge (a b). optionally set prop, p, (with val)."
  (unless (/= a b) (warn "-ADD incorrect edge: (~a ~a)." a b))
  (if (@mem g a b)
      (values g nil) ; do nothing
      (values (typecase p ; add edge (and prop)
                (symbol (prop #1=(-add g a b) (list a b) p val))
                (t #1#))
              t)))

(defun -del (g a b)
  (declare (grph g) (pn a b))
  (let* ((adj (adj g))
         (eset (@ adj a)))
    (if (and eset (contains? eset b))
        (grph (fset:with adj a (less eset b))
              (1- (grph-num-edges g))
              (props g))
        g)))

; TODO: nullify props of ab?
(defun del (g a b)
  (declare (grph g) (pn a b))
  "delete edge (a b)."
  (-del g a b))

