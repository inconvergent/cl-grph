(in-package :grph)

(defmacro adj (g) `(grph-adj ,g)) (defmacro mid (g) `(grph-mid ,g)) (defmacro props (g) `(grph-props ,g))
(defmacro srt (a b) (declare (symbol a b)) `(if (< ,a ,b) (list a b) (list b a)))

(defmacro get-multi-rel (props k &key prop (default nil))
  (awg (pmap) `(let ((,pmap (@ ,props ,k)))
                 ,(if prop `(and ,pmap (values (@ ,pmap ,prop)))
                    `(or ,pmap ,default)))))

(defmacro set-multi-rel (props k p &optional (val nil map?))
  (declare (symbol k p val))
  (awg (pk props*)
    `(let* ((,props* ,props) (,pk (@ ,props* ,k)))
      (fset:with ,props* ,k ; props is a map
        ; pk is a map if val is provided, otherwise set
        ,(if map? `(if ,pk (fset:with ,pk ,p ,val) (fset:map (,p ,val)))
                  `(if ,pk (fset:with ,pk ,p) (fset:set ,p)))))))

(defun -del-multi-prune (props k pk p)
  (let ((new-pk (when pk (fset:less pk p))))
    (if (fset:empty? new-pk) (fset:less props k)
                             (fset:with props k new-pk))))
(defmacro del-multi-rel (props k &optional p)
  (if p `(-del-multi-prune ,props ,k (@ ,props ,k) ,p)
        `(fset:less ,props ,k)))

(defmacro itr-edges ((g a &optional b) &body body) ; TODO: itr prop edges, ignore half?
  (declare (symbol g a))
  "iterate all edges, as either a=(v1 v2) or a=v1, b=v2."
  (unless (or (not b) (symbolp b)) (error "bad arg to itr-edges"))
  (awg (a* b* eset has)
    `(do-map (,a* ,eset (adj ,g))
       (do-map (,b* ,has ,eset)
         (when ,has
            ,(if b `(let ((,a ,a*) (,b ,b*)) (declare (ignorable ,a ,b)) ,@body)
                `(let ((,a (list ,a* ,b*))) ,@body)))))))
(defmacro itr-adj ((g a b &optional (modes :->)) &body body)
  (declare (symbol g b) (symbol modes))
  "iterate all adjacent verts, b, of a. modes: (-> <- >< <>)."
  (awg (b* eset has)
    (let ((modes (valid-modes :itr-adj modes *dir-modes*))
          (let-b `(let ((,b ,b*)) (declare (in ,b*)) ,@body)))
      `(let* ((,eset (@ (adj ,g) (the in ,a))))
         (when ,eset (do-map (,b* ,has ,eset)
                       (declare (in ,b*) (ignorable ,has))
                       ,(ecase (select-mode modes *dir-modes*)
                         (:-> `(when (@mem ,g ,a ,b*) ,let-b)) ; a -> b
                         (:<- `(when (@mem ,g ,b* ,a) ,let-b)) ; a <- b
                         (:>< let-b) ; either
                         (:<> `(when (and (@mem ,g ,b* ,a) (@mem ,g ,a ,b*)) ; both
                                       ,let-b)))))))))
(defmacro itr-verts ((g a) &body body)
  (declare (symbol g a))
  "iterate all connected verts, as a."
  (awg (seen b)
    `(let ((,seen (make-hash-table :test #'eql)))
      (labels ((,seen (,b) (if (gethash ,b ,seen) t
                               (progn (setf (gethash ,b ,seen) t)
                                      nil))))
        (itr-edges (,g ,a ,b)
          (when (not (,seen ,a)) (let ((,a ,a)) (declare (ignorable ,a)) ,@body))
          (when (not (,seen ,b)) (let ((,a ,b)) (declare (ignorable ,a)) ,@body)))))))

(defmacro add! (g a b &optional props) ; TODO: deprecate this in favor of add*!?
  (declare (symbol g))
  "add edge edge and re-bind. returns: (a b) or nil."
  (awg (a* b* g* created?)
    `(let ((,a* ,a) (,b* ,b))
       (mvb (,g* ,created?) (add ,g ,a* ,b* ,props)
         (setf ,g ,g*)
         (if ,created? (list ,a* ,b*) nil)))))
; TODO: ladd*!, clear! (props) ; ldel!, lclear! ?
(defmacro add*! (g a b &optional (modes :->) props)
  (declare (symbol g))
  "add edge edge and re-bind. returns: (a b) or nil. modes: (-> <- <>)"
  (awg (props* a* b*)
    (let ((modes (valid-modes :add*! modes *dir-modes*))
          (-> `(add! ,g ,a* ,b* ,props*))
          (<- `(add! ,g ,b* ,a* ,props*)))
       `(let ((,props* ,props) (,a* ,a) (,b* ,b))
          (declare (in ,a* ,b*))
         ,(ecase (select-mode modes *dir-modes*)
              (:-> ->) (:<- <-) (:<> `(progn ,<- ,->)))))))

(defmacro prop! (g k props)
  (declare (symbol g))
  "add edge/vert prop for key, k."
  (awg (k*)
    `(let ((,k* ,k))
       (setf ,g (prop ,g ,k* ,props)))))

(defmacro path! (g path &optional (modes '(:open :->)) props)
  (declare (symbol g))
  "add path (a b c ...). modes (-> <- <> open closed)"
  (awg (i j path* props*)
    (let* ((modes (valid-modes :path! modes `(,@*dir-modes* :closed :open)))
           (-> `(add! ,g ,i ,j ,props*))
           (<- `(add! ,g ,j ,i ,props*))
           (closed (eq :closed (select-mode modes '(:open :closed)))))
      `(let* ((,props* ,props)
              (,path* ,(if closed `(close-path ,path) path)))
        (declare (list ,path*))
        (mapcar (lambda (,i ,j)
                  ,(ecase (select-mode modes *dir-modes*)
                          (:-> ->) (:<- <-) (:<> `(progn ,<- ,->))))
                ,path* (cdr ,path*))
        ,path*)))) ; should we return edges instead?

(defmacro split! (g a b c &optional (modes '(:->)))
  (declare (symbol g))
  "delete edge and insert c according to mode c is only evaluated once, and
only if the mode is triggered. returns nil if nothing happens otherwise
returns what c evaluates to.

modes:
  ->, <- : delete ab or ba if it exists and create either acb or bca
  <>     : deletes both edges if both exist and creates acb and bca
  ><     : does -> or <- or both"
  (awg (a* b* c* props* ab ba)
  (let ((modes (valid-modes :split! modes *dir-modes*))
        (cc `(setf ,c* (the in ,c)))
        (-> `(let ((,props* (@prop ,g (list ,a* ,b*))))
               (del! ,g ,a* ,b*)
               (add! ,g ,a* ,c* ,props*)
               (add! ,g ,c* ,b* ,props*)))
        (<- `(let ((,props* (@prop ,g (list ,b* ,a*))))
               (del! ,g ,b* ,a*)
               (add! ,g ,b* ,c* ,props*)
               (add! ,g ,c* ,a* ,props*))))
    `(let* ((,a* ,a) (,b* ,b) ,c*
            (,ab (@mem ,g ,a* ,b*))
            (,ba (@mem ,g ,b* ,a*)))
      (declare (in ,a* ,b*))
     ,(ecase (select-mode modes *dir-modes*)
        (:-> `(when ,ab ,cc ,->))
        (:<- `(when ,ba ,cc ,<-))
        (:<> `(when (and ,ba ,ab) ,cc ,<- ,->))
        (:>< `(when (or ,ba ,ab) ,cc (when ,ba ,<-) (when ,ab ,->))))
     ,c*))))

; TODO: del?, path?
; TODO: ignore :_ when creating edges/props
(defmacro modify! ((g* sym &key (out g*)) &body body)
  (declare (symbol g* out sym))
  "batch modify g in a transaction. more efficient for loading a large number
of edges and/or props. faster for larger batches. g will be available
unchanged inside the context. and the changes are applied at the end. use
:out to bind the result to a different variable.

ex: (modify! (g mygrp)
      (loop for a = (rnd:rndi n) for b = (rnd:rndi n) repeat 10
            do ; NOTE:
               ; (mygrp-cancel) aborts the transaction,
               ; (mygrp-stop) stops the transaction, but keeps the changes,
               (rnd:either (mygrp-> a b '(:x :c))
                           (mygrp<> a b '(:y :d)))))"
  (awg (g ht mget madd sadd do-merge merge listify
        do-> ne hm-adj hm-mid hm-props stop bdy)
    `(locally (declare (optimize speed (safety 1)))
     (let ((,g ,g*))
       (declare (grph ,g))
       (labels
       ((,ht (&optional (fx #'eql)) (declare (function fx))
          (make-hash-table :test fx))
        (,mget (hm a b) (@ (gethash a hm (fset:empty-map)) b))
        (,madd (fx hm a b &optional (v t)) (declare (function fx) (hash-table hm))
          (setf (gethash a hm)
                (fset:with (gethash a hm (or (@ (funcall fx ,g) a)
                                             (fset:empty-map)))
                           b v)))
        (,sadd (fx hm a b) (declare (function fx) (hash-table hm))
          (setf (gethash a hm) (fset:with (gethash a hm (or (@ (funcall fx ,g) a)
                                                            (fset:empty-set)))
                                          b)))
        (,do-merge (adj hm)
          (loop for a being the hash-keys of hm using (hash-value emap)
                do (setf adj (fset:with adj a emap)))
           adj)
        (,listify (o) (etypecase o (fset:set (set->lst o)) (list o) (keyword (list o)))))
       (let ((,hm-adj (,ht)) (,hm-mid (,ht #'eq))
             (,hm-props (,ht #'equal)) (,ne 0))
         (declare (pn ,ne) (hash-table ,hm-adj ,hm-mid ,hm-props))
         (labels
           ((,do-> (a b) (declare (in a b))
              (,madd #'grph-adj ,hm-adj a b)
              (,madd #'grph-adj ,hm-adj b a (or (,mget ,hm-adj b a) (@mem ,g b a)))
              (incf ,ne)
              (list a b))
            (,(symb sym :->) (a b &optional props) (declare (in a b))
              (when (= a b) (return-from ,(symb sym :->) nil))
              (loop with edg of-type list = (list a b)
                    for p* in (,listify props)
                    do (etypecase p*
                         (keyword (,sadd #'grph-props ,hm-props edg p*)
                                  (,sadd #'grph-mid ,hm-mid p* edg))))
              (unless (or (,mget ,hm-adj a b) (@mem ,g a b))
                          (,do-> a b)))
            (,(symb sym :<-) (a b &optional props) (declare (in a b))
              (,(symb sym :->) b a props))
            (,(symb sym :<>) (a b &optional props) (declare (in a b))
              (list (,(symb sym :->) a b props) (,(symb sym :<-) a b props)))
            (,merge ()
              (grph (,do-merge (adj ,g) ,hm-adj) (+ ,ne (grph-num-edges ,g))
                    (,do-merge (props ,g) ,hm-props) (,do-merge (mid ,g) ,hm-mid))))

           (macrolet ((,(symb sym :-cancel) (&body body)
                         `(return-from ,',stop (progn ,@body)))
                      (,(symb sym :-stop) (&body body)
                         `(return-from ,',stop (let ((,',bdy (progn ,@body)))
                                                 (setf ,',out (,',merge))
                                                 ,',bdy))))
             (block ,stop (let ((,bdy (progn ,@body)))
                            (setf ,out (,merge))
                            ,bdy))))))))))

(defmacro del! (g a b &optional p) ; TODO: del*! with modes
  (declare (symbol g))
  "del edge and re-bind. returns: deleted?"
  (awg (a* b* g* deleted?)
    `(let ((,a* ,a) (,b* ,b))
      (mvb (,g* ,deleted?) ,(if p `(del-props ,g (list ,a* ,b*) ,p)
                                  `(del ,g ,a* ,b*))
        (setf ,g ,g*)
        ,deleted?))))
(defmacro ldel! (g ab &rest rest)
  "del edge ab=(a b) and re-bind. returns: deleted?"
  `(dsb (a b) ,ab (del! ,g a b ,@rest)))

; (defmacro pdel! (g path &optional p) ; TODO:
;   (declare (symbol g))
;   "del edge and re-bind. returns: deleted?"
;   (awg (a* b* g* deleted?)
;     `(let ((,a* ,a) (,b* ,b))
;       (mvb (,g* ,deleted?) ,(if p `(del-props ,g (list ,a* ,b*) ,p)
;                                   `(del ,g ,a* ,b*))
;         (setf ,g ,g*)
;         ,deleted?))))

(defmacro using ((&rest using) &body body)
  (declare (notinline ^var? no-dupes?))
  "transaction context:

  (let ((a (list)))
    (grph:using (^a) ; ^[var] prefix is required, and [var] must be bound outside
      (push 1 ^a)    ; ^a is now '(1)
      (push 2 ^a)    ; ^a is now '(2 1)
      (print a))     ;  a is still nil
    (print a))       ; ^a is bound to a, so a is now (2 1)

(grph:cancel) and (grph:stop) can be used inside the macro to stop and discard
or keep changes respectively.

NOTE: this behaviour is only guaranteed for immutable data structures
(eg grph, fset); or for operations that do not mutate their operands, such as
push."
  (unless (and (every #'^var? using) (no-dupes? using))
          (warn "USING: got bad value for :using ~s. vars need ^ prefix." using))
  (awg (res stop*)
    (labels
      ((re-intern (g) (intern (subseq (mkstr g) 1) (symbol-package g)))
       (bind-partial () (loop for g in using collect `(,g ,(re-intern g))))
       (re-bind-result ()
         `(setf ,@(awf (loop for g in using collect `(,(re-intern g) ,g))))))
      `(macrolet
         ((cancel (&body cbody) `(return-from ,',stop* (progn ,@cbody)))
         ; stop, but keep the results
         (stop (&body sbody) `(progn ,',(re-bind-result)
                                    (return-from ,',stop* (progn ,@sbody)))))
         (let (,@(bind-partial))
           (block ,stop* (let ((,res (veq:lst (progn ,@body))))
                           ,(re-bind-result)
                           (values-list ,res))))))))

