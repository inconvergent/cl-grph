(in-package :grph)

(defmacro srt (a b) (declare (symbol a b)) `(if (< ,a ,b) (list a b) (list b a)))
(defmacro adj (g) (declare (symbol g)) `(grph-adj ,g))
(defmacro mid (g) (declare (symbol g)) `(grph-mid ,g))
(defmacro props (g) (declare (symbol g)) `(grph-props ,g))


(defmacro get-multi-rel (props k &key prop (default nil))
  (awg (pmap)
    `(let ((,pmap (@ ,props ,k)))
      ,(if prop `(and ,pmap (values (@ ,pmap ,prop)))
                `(or ,pmap ,default)))))

(defmacro set-multi-rel (props k p &optional (val nil val?)
                                   &aux (default (if val?  'nilmap 'nilset)))
  (awg (pk props*)
    `(let* ((,props* ,props)
            (,pk (@ ,props* ,k)))
      (declare (ignorable ,pk))
      (fset:with ,props* ,k ; props is a map
        ; pk is a map if val is provided, otherwise set
        (fset:with (or ,pk ,default) ,p
                   ,@(if val? `(,val)))))))


(defun -del-multi-prune (props k pk p)
  (let ((new-pk (when pk (fset:less pk p))))
    (if (fset:empty? new-pk) (fset:less props k)
                             (fset:with props k new-pk))))
(defmacro del-multi-rel (props k &optional p)
  (if p `(-del-multi-prune ,props ,k (@ ,props ,k) ,p)
        `(fset:less ,props ,k)))

; TODO: itr prop edges, itr prop verts?
; TODO: ignore half?
(defmacro itr-edges ((g a &optional b) &body body)
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
  "iterate all adjacent verts, b, of a. use modes (-> <- >< <>)."
  (awg (b* eset has)
    (let ((modes (valid-modes :itr-adj modes `(,@*dir-mode* :><)))
          (bind-let `(let ((,b ,b*)) (declare (pn ,b*)) ,@body)))
      `(let* ((,eset (@ (adj ,g) (the pn ,a))))
         (when ,eset (do-map (,b* ,has ,eset)
                       (declare (pn ,b*) (ignorable ,has))
                       ,(ecase (select-mode modes `(,@*dir-mode* :><))
                         (:-> `(when (@mem ,g ,a ,b*) ,bind-let)) ; a -> b
                         (:<- `(when (@mem ,g ,b* ,a) ,bind-let)) ; a <- b
                         (:>< bind-let) ; either
                         (:<> `(when (and (@mem ,g ,b* ,a) (@mem ,g ,a ,b*)) ; both
                                       ,bind-let)))))))))

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

(defmacro add! (g a b &optional props)
  (declare (symbol g))
  "add edge edge and re-bind. returns: (a b) or nil."
  (awg (a* b* g* created?)
    `(let ((,a* ,a) (,b* ,b))
       (mvb (,g* ,created?) (add ,g ,a* ,b* ,props)
         (setf ,g ,g*)
         (if ,created? (list ,a* ,b*) nil)))))

; TODO: ladd*!, clear! (props)
; ldel!, lclear! ?
(defmacro add*! (g a b &optional (modes :->) props)
  (declare (symbol g))
  "add edge edge and re-bind. returns: (a b) or nil."
  (awg (props* a* b*)
    (let ((modes (valid-modes :add*! modes *dir-mode*))
          (-> `(add! ,g ,a* ,b* ,props*))
          (<- `(add! ,g ,b* ,a* ,props*)))
       `(let ((,props* ,props)
              (,a* ,a) (,b* ,b))
          (declare (list ,props*) (pn ,a* ,b*))
         ,(ecase (select-mode modes *dir-mode*)
              (:-> ->) (:<- <-) (:<> `(progn ,<- ,->)))))))

(defmacro path! (g path &optional (modes '(:open :->)) props)
  (declare (symbol g))
  (awg (i j path* path** props*)
    (let* ((modes (valid-modes :path! modes `(,@*dir-mode* :closed :open)))
           (-> `(add! ,g ,i ,j ,props*))
           (<- `(add! ,g ,j ,i ,props*))
           (closed (eq :closed (select-mode modes '(:open :closed)))))
      `(let* ((,props* ,props)
              (,path* ,path)
              ,@(if closed `((,path** (cons (last* ,path*) ,path*)))))
        (declare (list ,props* ,path*))
        (mapcar (lambda (,i ,j)
                  ,(ecase (select-mode modes *dir-mode*)
                          (:-> ->) (:<- <-) (:<> `(progn ,<- ,->))))
                ,@(if closed `(,path** (cdr ,path**))
                             `(,path* (cdr ,path*))))
        ,path*)))) ; should we return edges instead?


(defmacro del! (g a b &optional p)
  (declare (symbol g))
  "del edge and re-bind. returns: deleted?"
  (awg (a* b* g* deleted?)
    `(let ((,a* ,a) (,b* ,b))
      (mvb (,g* ,deleted?) ,(if p `(del-props ,g (list ,a* ,b*) ,p)
                                  `(del ,g ,a* ,b*))
        (setf ,g ,g*)
        ,deleted?))))
(defmacro ldel! (g ab &rest rest) `(dsb (a b) ,ab (del! ,g a b ,@rest)))

(defmacro using ((&rest using) &body body)
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
           (block ,stop* (let ((,res (progn ,@body)))
                           ,(re-bind-result)
                           ,res)))))))

