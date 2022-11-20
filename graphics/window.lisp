;;;; WINDOW
;; Window functionality for Glint. Windows are implemented as observers, which
;; attach themselves to a camera entity, and render a frame every time they are notified
(in-package :glint.graphics)



;; A file which mostly provides thin wrappers over glfw/opengl functions
(defclass window ()
  ((window
    :type t ;; todo what type is a window?
    :initarg :window
    :accessor window-glfw-window
    :documentation "a glfw window. ")
   (camera
    :type camera
    :initarg :camera
    :accessor window-camera))
  (:documentation "A window, with attached camera"))

(defvar *window*)



(glfw:def-error-callback err (message)
  (format t "GLFW error: ~a" message))

(glfw:set-error-callback 'err)

(defun make-window (camera &key (clear-colour #(0.2 0.3 0.3 1.0)))
  (let ((width (camera-width camera))
        (height (camera-height camera)))
    (glfw:initialize)
    (glfw:create-window :width width :height height :title "Euclidea")
    (glfw:set-input-mode :cursor :disabled)

    (gl:viewport 0 0 width height)
    (gl:clear-color (elt clear-colour 0) (elt clear-colour 1) (elt clear-colour 2) (elt clear-colour 3))
    (gl:enable :depth-test)

    (setf *window* (glfw:get-current-context))
    (make-instance 'window
                   :window (glfw:get-current-context)
                   :camera camera)))


;; deletes the /currently active/ window
(defun release-window (window)
  (glfw:destroy-window (window-glfw-window window)))

;; To be called when rendering is done: will swap the render buffer and
;; window buffer, then clear the (new) render-buffer
(defun display ()
  (glfw:swap-buffers *window*)
  (gl:clear :color-buffer :depth-buffer))


;; functions which are essentially re-exporting symbols this will
;; be redone when I use a proper system definition facility/packages
(defun key-is-pressed-p (key)
  (eq (glfw:get-key key *window*) :press))

(defun window-should-close-p ()
  (glfw:window-should-close-p *window*))

(defun poll-events ()
  (glfw:poll-events))

(defun get-cursor-pos ()
  "Return cursor position as a 2-vector"
  (make-array 2 :initial-contents
              (glfw:get-cursor-position *window*)))


;;; SHADERS
;; these functiosn are simple interfaces to the OpenGL facilities which allow
;; interfacing with shaders

(defun new-shader-program (program_name)
  "Creates a new shader program given a string/path, automatically appends '.vert' and '.frag'"
  ;; initialize variables
  (let ((vertex-shader (gl:create-shader :vertex-shader))
        (fragment-shader (gl:create-shader :fragment-shader))
        (shader-program (gl:create-program))
        (vertex-shader-path (concatenate 'string program_name ".vert"))
        (fragment-shader-path (concatenate 'string program_name ".frag")))

    ;; compile the vertex & fragment shaders
    (with-open-file (vert-file vertex-shader-path
                               :direction :input)
      (gl:shader-source vertex-shader (uiop:read-file-string vert-file))
      (gl:compile-shader vertex-shader)
      ;; report any errors
      (format t "~a~%" (gl:get-shader-info-log vertex-shader)))

    (with-open-file (frag-file fragment-shader-path
                               :direction :input)
      (gl:shader-source fragment-shader (uiop:read-file-string frag-file))
      (gl:compile-shader fragment-shader)
      ;; report any errors
      (format t "~a~%" (gl:get-shader-info-log fragment-shader)))

    ;; link the shaders into a program
    (gl:attach-shader shader-program vertex-shader)
    (gl:attach-shader shader-program fragment-shader)
    (gl:link-program shader-program)
    ;; output the info log so errors are reported
    (format t "~a~%" (gl:get-program-info-log shader-program))

     ;; cleanup
    (gl:delete-shader vertex-shader)
    (gl:delete-shader fragment-shader)

    ;; return value
    shader-program))
    

(defun set-uniform (uniform value)
  "A generic set-uniform function which will detect the type of the value passed"
  ;; TODO: expand this to use more types, currently is basically a convenient wrapper
  ;;       to avoid having to place matrix::matrix-data everywhere...
  (cond ((typep value 'matrix:matrix)
         (gl:uniform-matrix-4fv uniform (matrix::matrix-data value)))
        (t (error (format nil "called set-uniform with unsupported value: ~a ~%" value)))))


(defun get-uniform (shader uniform-name)
  "Given handles to a shader, and a string uniform-name, returns a handle to a uniform"
  (gl:get-uniform-location shader uniform-name))


;; attach a glfw window to the engine instance. May update the (currently clunky)
;; way of handling windows: they are just a raw glfw window handle at the moment
;; (defmethod attach-window ((engine engine) window)
;;   (with-slots (width height projection) (get-resource 'camera engine)
;;     (setf projection (matrix:perspective (/ pi 2) 16/9 0.1 100))
;;     (setf width 1280)
;;     (setf height 720))

;;   ;; set the window size callback to adjust height/width of the viewport
;;   ;; and aspect ratio, accordingly
;;   (glfw:def-window-size-callback update-viewport (window w h)
;;     (declare (ignore window))
;;     (lambda ()
;;       (with-slots (width height projection) (get-resource 'camera engine)
;;         (setf projection (matrix:perspective (/ pi 2) (/ w h) 0.1 100))
;;         (setf width w)
;;         (setf height h)))))

;; render all entities
(defun window-notify-tick (window engine)
  (map 'vector (lambda (e) (draw e (window-camera window) engine))
       (engine-entities engine))
  (poll-events)
  (display)
  (when (or (window-should-close-p) (key-is-pressed-p :escape))
            (setf (engine-should-stop engine) t)))
