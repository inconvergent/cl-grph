(in-package :grph)

"
this is much nicer code than what's in qry-runtime.lisp, but it is a lot slower.
we are using it to test the fast code in tests
"

(declaim (inline map/keys->set map/get-keys map/split-by-keys
                 set/outer-join set/isect-merge-1 set/isect-merge-2))


(defun map/keys->set (&rest mm &aux (s (fset:set)))
  (declare (optimize speed (safety 1)) (fset:set s))
  "set of all keys in one or more maps"
  (loop for m in mm do (do-map (k v m)
                         (declare (ignore v))
                         (setf s (fset:with s k))))
  s)

(defun map/get-keys (m kk)
  (declare (optimize speed (safety 1)) (fset:map m) (fset:set kk))
  "get new map from m using keys in kk"
  (fset:filter (lambda (k v) (when (member? k kk) (values k v))) m))

(defun map/split-by-keys (m kk &aux (in (empty-map)) (out (empty-map)))
  (declare (optimize speed (safety 1)) (fset:map m) (fset:set kk))
  "split the map into two maps. the keys in kk are in in, the rest in out"
  (do-map (k v m)
    (if (member? k kk) (setf in (fset:with in k v))
                            (setf out (fset:with out k v))))
  (values in out))

(defun set/isect-merge-1 (common lft rht)
  (declare (optimize speed (safety 1)) (fset:set common lft rht))
  "assumes common == lft-keys, and |rht-keys| > |common|"
  (fset:filter (lambda (m) (member? (map/get-keys m common) lft)) rht))


(defun logic-set/collapse-by-keys (ss kk &aux (res (logic-set)))
  "collapse mm by keys in kk"
  (declare (optimize speed (safety 1)) (fset:set ss kk))
  (do-set (m ss)
    (veq:mvb (in out) (map/split-by-keys m kk)
      (declare (fset:map in out))
      (setf res (fset:with res in (fset:with (fset:@ res in) out)))))
  (fset:with-default res nil))

(defun set/outer-join (a b &aux (res (empty-set)))
  (declare (optimize speed (safety 1)) (fset:set a b))
  "assumes a and b to be disjoint (in keys)"
  (do-set (ma a)
    (declare (fset:map ma))
    (do-set (mb b)
      (declare (fset:map mb))
      (setf res (fset:with res (fset:map-union ma mb)))))
  res)

(defun set/isect-merge-2 (common lft rht &aux (res (empty-set)))
  (declare (optimize speed (safety 1))
           (fset:set common lft rht))
  "assumes |common| < |lft-keys|, and |common| < |rht-keys|"
  (let ((lft* (logic-set/collapse-by-keys lft common))
        (rht* (logic-set/collapse-by-keys rht common)))
    (when (> (fsize lft*) (fsize rht*)) (rotatef lft* rht*))
    (do-map (root-key ll lft*)
      (let ((rr (fset:@ rht* root-key)))
        (when rr (do-set (s (set/outer-join ll rr))
                    (setf res (fset:with res (fset:map-union root-key s)))))))
    res))

(defun set/qry-and* (aa bb)
  (declare (optimize speed (safety 1)) (fset:set aa bb))
  (when (< (fsize bb) (fsize aa)) (rotatef aa bb)) ; ensure |a| <= |b|
  (when (fset:empty? aa) (return-from set/qry-and* (empty-set))) ; exit if empty

  (let* ((ak (map/keys->set (fset:arb aa)))
         (bk (map/keys->set (fset:arb bb)))
         (common (fset:intersection ak bk))
         (all (fset:union ak bk)))
    (when (fset:empty? common) ; no common
          (return-from set/qry-and* (empty-set)))
    (when (= (fsize common) (fsize all)) ; identical variables/keys
          (return-from set/qry-and* (fset:intersection aa bb)))
    (when (= (fsize common) (fsize ak)) ; slice bb with aa
          (return-from set/qry-and* (set/isect-merge-1 common aa bb)))
    (when (= (fsize common) (fsize bk)) ; slice aa with bb
          (return-from set/qry-and* (set/isect-merge-1 common bb aa)))
    (set/isect-merge-2 common aa bb))) ; complex intersection-merge

