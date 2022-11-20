(in-package :glint.graphics)


(defgeneric update-model-matrix (model)
  (:documentation "Generates a new model matrix (via calc-model-matrix) and "))


(defvar *shader-program* nil)


(defclass model (transform)
  ((vao
    :accessor model-vao 
    :documentation "Handle to OpenGL VAO")
   (size
    :accessor model-size
    :documentation "The number of vertices to draw: |indices|")
   (texture
    :initform nil
    :documentation "A handle to the texture that the framebuffer will render to")
   (colour
    :accessor model-colour
    :initarg :colour
    :initform (vector 0.8 0.8 0.8)
    :documentation "An rgb vector denoting the colour of the model")
   (shader
    :accessor model-shader
    :initform *shader-program*
    :documentation "A Handle to the shader of the entity")
   (model-matrix
    :type matrix:matrix
    :accessor model-matrix
    :initform (matrix:identity 4)
    :documentation "A matrix describing the rotation, tranlsation and scale
(cached, should be calculated from the transform rotation, translation & scale)"))
  
  (:documentation "Contains information needed by OpenGL to render an Object"))

(defgeneric sync-model-matrix (entity))

(defmethod sync-model-matrix ((model model))
  (setf (model-matrix model) (calc-model-matrix model)))

(defmethod rotate ((model model) amt)
  (call-next-method)
  (sync-model-matrix model))


(defmethod translate ((model model) amt)
  (call-next-method)
  (sync-model-matrix model))


(defmethod scale ((model model) amt)
  (call-next-method)
  (sync-model-matrix model))


;; a vao: currently just coordiantes & indices
(defun make-vao (mesh data)
  ;; generate & bind the VBO (data storage)
  (let* ((buffers (gl:gen-buffers 2))
         (vert-buf (elt buffers 0))
         (ind-buf (elt buffers 1)))

    ;; populate a c-style array with data
    (let* ((verts (getf data :vertices))
           (arr (gl:alloc-gl-array :float (length verts))))
      (dotimes (i (length verts))
        (setf (gl:glaref arr i) (aref verts i)))

      ;; bind the corret VBO
      (gl:bind-buffer :array-buffer vert-buf)

      ;; copy the data into the vbo
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr)

      ;; unbind
      (gl:bind-buffer :array-buffer 0))

    (let* ((indices (getf data :indices))
           (arr (gl:alloc-gl-array :unsigned-short (length indices))))
      ;; unique: set size to length indices
      (setf (model-size mesh) (length indices))
      
      (dotimes (i (length indices))
        (setf (gl:glaref arr i) (aref indices i)))

      ;; bind the index VBO
      (gl:bind-buffer :element-array-buffer ind-buf)

      ;; copy the data into the vbo
      (gl:buffer-data :element-array-buffer :static-draw arr)
      (gl:free-gl-array arr)

      ;; unbind
      (gl:bind-buffer :element-array-buffer 0))


      ;; generate a vao to reference this data
    (let ((vao (gl:gen-vertex-array))
          (stride (* (cffi:foreign-type-size :float)
                     (+ 3
                        (if (getf data :texture-p) 2 0)
                        (if (getf data :normal-p) 3 0)))))
      (gl:bind-vertex-array vao)
      (gl:bind-buffer :array-buffer vert-buf)

      ;; how the data is layed out
      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 3 :float nil stride 0)

      (when (getf data :texture-p)
        (gl:enable-vertex-attrib-array 1)
        (gl:vertex-attrib-pointer 1 2 :float nil stride (* 3 (cffi:foreign-type-size :float))))

      (when (getf data :normal-p)
        (gl:enable-vertex-attrib-array 1)
        (gl:vertex-attrib-pointer 1 3 :float nil stride (* 3 (cffi:foreign-type-size :float))))

      ;; bind EAO
      (gl:bind-buffer :element-array-buffer ind-buf)

      ;; unbind the vao
      (gl:bind-vertex-array 0)
      ;; return
      (setf (model-vao mesh) vao))))

;; see https://github.com/3b/cl-opengl/blob/master/examples/misc/opengl-array.lisp

(defgeneric draw (entity camera world)
  (:documentation "a generic draw method"))


(defmethod draw ((m entity) (c camera) (w engine)))


(defmethod draw ((m model) (camera camera) (w engine))
  (gl:viewport 0 0 (camera-width camera) (camera-height camera))
  (with-slots (shader texture colour) m
    (gl:use-program shader)

    (gl:uniformfv (get-uniform shader "light_pos") (vector 0.0 5.0 0.0))
    (gl:uniformfv (get-uniform shader "object_colour") colour)

    ;; model matrix
    (set-uniform (get-uniform shader "model") (model-matrix m))

    ;; view matrix
    (set-uniform (get-uniform shader "view") (gen-view-matrix camera))

    ;; perspective matrix
    (set-uniform (get-uniform shader "projection") (camera-projection camera))

    ;; now actually draw the shape
    (when texture (gl:bind-texture :texture-2d texture))
    (gl:bind-vertex-array (model-vao m))
    (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count (model-size m))))


(defun set-shader (shader) (setf *shader-program* shader))


(defun render-callback (camera world)
  (poll-events)
  (mapcar (lambda (x) (draw camera x world)) (get-entities world))
  (display))


