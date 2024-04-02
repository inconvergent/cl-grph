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

; (define-struct-load-form grph)
#+SBCL(declaim (sb-ext:freeze-type grph))

; GET / LOOKUP ----------------

(defun @mem (g a b &aux (eset (@ (adj g) a)))
  (declare (grph g) (in a b)) "t if edge (a b) exists."
  (and eset (values (fset:@ eset b))))

; TODO: filter by edge/vert prop?
(defun @prop (g k &optional p)
  (declare (grph g) ((or list in) k)) "get val of prop, p, for key, k should be edge (a b); or vert."
  (if p (get-multi-rel (props g) k :prop p) ; t / nil if prop exists
        (get-multi-rel (props g) k)))
(defun @mid (g k &optional p)
  (declare (grph g)) "get val of prop, p, for key, k. should be a prop (keyword)."
  (if p (get-multi-rel (mid g) k :prop p) ; t / nil if prop+edge/vert exists
        (get-multi-rel (mid g) k)))

(defun adjcnt (adj &aux (n 0))
  (declare (fset:map adj) (veq:pn n)) "count total number of edges in grph-adj."
  (do-map (a edges adj) (do-map (b dir edges) (when dir (incf n))))
  n)
(defun ecnt (g) (declare (grph g)) "count total number of edges in grph." (adjcnt (grph-adj g)))
(defun @enum (g) (declare (grph g)) "total number of edges in graph." (grph-num-edges g))
(defun @pnum (g) (declare (grph g)) "total number of props in graph." (fset:size (grph-mid g)))
(defun @vcnt (g &aux (res 0)) (declare (grph g) (pn res)) "count all connected verts."
  (itr-verts (g a) (incf res)) res)

(defun @edges (g &aux (res (list))) (declare (grph g) (list res)) "list of lists of all edges."
  (itr-edges (g e) (push e res)) res)
(defun @out (g a &aux (res (list))) (declare (grph g) (in a) (list res)) "list all outboud verts of a."
  (itr-adj (g a b ->) (push b res)) res)
(defun @in (g a &aux (res (list))) (declare (grph g) (in a) (list res)) "list all outboud verts of a."
  (itr-adj (g a b <-) (push b res)) res)
(defun @both (g a &aux (res (list))) (declare (grph g) (in a) (list res)) "list all verts of a that are bi-directional."
  (itr-adj (g a b <>) (push b res)) res)
(defun @either (g a &aux (res (list))) (declare (grph g) (in a) (list res)) "list both inbound and outbond verts of a."
  (itr-adj (g a b ><) (push b res)) res)

(defun @vmax (g &aux (res 0)) (declare (grph g) (pn res)) "get highest vertex index."
  (itr-verts (g a) (setf res (max a res))) res)
(defun @verts (g &aux (res (list))) (declare (grph g) (list res)) "list of all connected verts."
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
(defun -del-prop (g ab prop) ; TODO: what happens with dangling mid/props values?
  (declare #.*opt* (grph g) (list ab) (symbol prop))
  (if (@prop g ab prop)
      (values (grph (adj g) (grph-num-edges g)
                    (del-multi-rel (props g) ab prop)
                    (del-multi-rel (mid g) prop ab))
              t)
      (values g nil)))
(defun del-props (g ab props)
  (declare #.*opt* (grph g) (list ab) ((or list fset:set) props))
  (loop with deleted? = nil
        for p in (etypecase props (list props) (fset:set (set->lst props)))
        do (mvb (g* del?) (-del-prop g ab p)
             (setf g g* deleted? (or del? deleted?)))
        finally (return-from del-props (values g deleted?))))

