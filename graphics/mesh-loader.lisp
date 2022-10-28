;;;; MESH LOADER
;; This file contains the function load-obj, which will load .obj files into a plist
;; format, with vertex, normal & texture coordinates and indexes into each of them
;; it is in *no way* a good or correct implementation, I just needed to hack something
;; together that would allow me to use blender to make 3D models
;; because of this, it isn't well documented, but that may change if I decide to put
;; more effort into this part of the project
(in-package :glint.graphics)

;; list = (a b c) -> (a b c) (triangles become triangles)
;; list = (a b c d) -> (a b c c d a) (for square faces)
;; TODO: fix for n-vertex faces currently only works for 3 & 4
(defun to-triples (data)
  "function which takes a face of n vertices & decomposes it into triangles"
  (if (= (length data) 3) data
      (list (elt data 0)
            (elt data 1)
            (elt data 2)
            (elt data 2)
            (elt data 3)
            (elt data 0))))

(defun gen-key-list (predicates indices)
  (when predicates
    (if (car predicates)
        (cons (car indices) (gen-key-list (cdr predicates) (cdr indices)))
        (gen-key-list (cdr predicates) (cdr indices)))))
  

(defun load-obj (filename &key (texture-p nil) (normal-p nil))
  "Function for loading in Waveform (.obj) 3D meshes"

  (let (;; Vertex data: one vector per datatype
        (vertices   (make-array 1 :fill-pointer 0))
        (tex-coords (make-array 1 :fill-pointer 0))
        (normals    (make-array 1 :fill-pointer 0))

        ;; index data: indices into the different vertex types
        (indices        (make-array 1 :fill-pointer 0))
        (tex-indices    (make-array 1 :fill-pointer 0))
        (normal-indices (make-array 1 :fill-pointer 0))

        (return-mesh (list
                      :vertices (make-array 1 :fill-pointer 0)
                      :indices (make-array 1 :fill-pointer 0)
                      :texture-p texture-p
                      :normal-p normal-p)))

    (with-open-file (obj-file filename
                              :direction :input)
      (loop for line = (read-line obj-file nil)
            while line do
              (let ((data (split-sequence:split-sequence #\Space line)))
                (cond
                  ((string= (car data) "v")
                   (dolist (vertex (cdr data))
                     (vector-push-extend (read-from-string vertex) vertices 1)))
                  ((string= (car data) "vn")
                   (dolist (normal (cdr data))
                     (vector-push-extend (read-from-string normal) normals 1)))
                  ((string= (car data) "vt")
                   (dolist (normal (cdr data))
                     (vector-push-extend (read-from-string normal) tex-coords 1)))
                  ((string= (car data) "f")
                   (dolist (index (to-triples (cdr data)))
                     (let ((indexes (split-sequence:split-sequence #\/ index)))
                       (vector-push-extend
                        ;; subtract 1 because vertex references start at 1
                        (- (read-from-string (car indexes)) 1)
                        indices 1)

                       (vector-push-extend
                        (- (read-from-string (cadr indexes)) 1)
                        tex-indices 1)

                       (vector-push-extend
                        (- (read-from-string (caddr indexes)) 1)
                        normal-indices 1))))))))


    ;; for each combination of indices, we create a /new/ index, and a new vertex
    ;; if that  combination is already in the vertex list, then use it's index
    ;; we only consider normals/texture indices if the corresponding key is not nil
    (loop for vindex across indices
          for tindex across tex-indices
          for nindex across normal-indices
          for key = (cons vindex (gen-key-list (list texture-p normal-p) (list tindex nindex)))

          with next-index = 0
          with map = (make-hash-table :test #'equal) do
            (if (gethash key map)
                ;; if the index is already in the hash-table, add to indices & continue
                (vector-push-extend (gethash key map) (getf return-mesh :indices) 1)

                ;; otherwise, make a new index, and a new vertex which concatenates (vertex, texture, normal) 
                ;; which ones we make will 
                (progn
                  (vector-push-extend next-index (getf return-mesh :indices) 1)
                  (setf (gethash key map) next-index)
                  (incf next-index)
                  (loop for i from 0 to 2 do (vector-push-extend
                                              (aref vertices (+ (* vindex 3) i))
                                              (getf return-mesh :vertices)
                                              1))
                  (when normal-p
                    (loop for i from 0 to 2 do (vector-push-extend
                                                (aref normals (+ (* nindex 3) i))
                                                (getf return-mesh :vertices)
                                                1)))
                  (when texture-p
                    (loop for i from 0 to 1 do (vector-push-extend
                                                (aref tex-coords (+ (* tindex 2) i))
                                                (getf return-mesh :vertices)
                                                1))))))
    return-mesh))

