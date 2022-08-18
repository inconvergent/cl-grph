(in-package :grph)

(declaim (fset:map nilmap) (fset:set nilset))
(defconstant nilmap (empty-map))
(defconstant nilset (empty-set))

(declaim (inline -del -add grph prop grph))


(defun -print-grph (o s)
  (declare (notinline grph-num-edges))
  (format s "<@grph: (verts: ~a, edges: ~a)>" (@vnum o) (@enum o)))

; TODO: ignore incorrect edge?
(defstruct (grph
  (:constructor grph (&optional (adj nilmap) (num-edges 0)
                                (props nilmap) (inv nilmap)))
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
  (inv nilmap :type fset:map :read-only t)
  (num-edges 0 :type pn :read-only t))

; GET / LOOKUP

(defun @mem (g a b &aux (eset (@ (adj g) a)))
  (declare (grph g) (pn a b))
  "t if edge (a b) exists."
  (and eset (contains? eset b)))

(defun @prop (g k &optional p)
  (declare (grph g))
  ; optional second arg
  "get val of prop, p, for key, k. should be an edge k=(a b) or a vert, k=a."
  (if p (get-multi-rel (props g) k :prop p)
        (get-multi-rel (props g) k)))

(defun @inv (g k &optional p)
  (declare (grph g))
  ; optional second arg
  "get val of prop, p, for key, k. should be an edge k=(a b) or a vert, k=a."
  (if p (get-multi-rel (inv g) k :prop p)
        (get-multi-rel (inv g) k)))

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

; MUTATE

(defun prop (g k p &optional (val t))
  (declare #.*opt* (grph g) (list k) (symbol p))
  "set prop, p, of edge or vert, k."
  (grph (adj g) (grph-num-edges g)
        (set-multi-rel (props g) k p val)
        (set-multi-rel (inv g) p k)))

(defun -add (g a b)
  (declare #.*opt* (grph g) (pn a b))
  (grph (set-multi-rel (adj g) a b)
        (1+ (grph-num-edges g))
        (props g) (inv g)))

(defun add (g a b &optional p (val t)) ; option to force set prop?
  (declare #.*opt* (grph g) (pn a b))
  "new edge (a b). optionally set prop, p, (with val)."
  (unless (/= a b) (warn "-ADD incorrect edge: (~a ~a)." a b))
  (if (@mem g a b)
      (values g nil) ; do nothing
      (values (typecase p ; add edge (and prop)
                (null #1=(-add g a b))
                (symbol (prop #1# (list a b) p val)))
              t)))

(defun -del (g a b)
  (declare #.*opt* (grph g) (pn a b))
  (labels
    ((prune-props (&aux (inv (inv g)) (ab `(,a ,b)))
       (do-map (p _ (or (@prop g ab) nilmap)) ; ignore _=v
               (setf inv (del-multi-rel inv p ab)))
       (grph (del-multi-rel (adj g) a b)
             (1- (grph-num-edges g))
             (del-multi-rel (props g) ab)
             inv)))
    (if (@mem g a b) (values (prune-props) t)
                     (values g nil))))

(defun del (g a b) ; option to dont delete props?, option to force del prop?
  (declare (grph g) (pn a b))
  "delete edge (a b). deletes associated props."
  (-del g a b))

