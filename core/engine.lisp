;;;; ENGINE.LISP
;; This file details an extremely simpe render engine (for the OpenGl code, see
;; engine/graphics/render-system.lisp) The engine has a set of functions that
;; it will call
;; + on initialization
;; + on update
;; + on cleanup/finish (using unwind-protect)
;; in addition, the engine contains several state variables:
;; + a list of entites
;; + a camera (see engine/graphics/camera.lisp)
;; + update & input values: delta-time, cursor position, ...
(in-package :glint.core)


;;; ENTITIES AND MESSAGING
(defclass entity ()
  ((inbox
    :initform (lparallel.queue:make-queue)
    :reader entity-inbox)
   (children
    :type (vector entity)
    :initform #()
    :reader entity-children
    :documentation "Sub-entities which are in some way attached to or tightly
associated with the parent"))
  (:documentation "Root of Object hierarchy for systems"))

(defgeneric send-message (entity message)
  (:documentation "Send a message to an entity"))

(defmethod send-message ((entity entity) message)
  (lparallel.queue:push-queue message (entity-inbox entity)))

(defun process-messages (entity)
  "Walk through a message queue, processing each entity in turn"
  (with-slots (inbox) entity
    (loop until (lparallel.queue:queue-empty-p inbox) do
      (funcall (lparallel.queue:pop-queue inbox) entity))))

;; (defgeneric receive-message ((entity entity) message)
;;   (:documentation "Method which can be overridden by inheritors to receive
;; custom messages"))

;; (defmethod receive-message ((entity entity) message)
;;   (:documentation ""))



;;; UPDATES

(defgeneric update (entity args state)
  (:documentation "A function that a user of the engine can choose to implement to define custom behaviours, but
which do not justify building an entirely new system"))

;; if the entity subclass does not implemnt update, do nothing
(defmethod update ((entity entity) args state)
  (map 'vector #'update (entity-children entity)))


;;; UTILITY: TODO - Move this elsewhere..

(defclass thunk ()
  ((val
    :initarg :val
    :accessor thunk-val)
   (called
    :initarg :fun
    :accessor thunk-called
    :initform nil)))

(defmacro lazy (form) `(make-instance 'thunk :val (lambda () ,form)))

(defun force (thunk)
  (unless (thunk-called thunk)
    (progn
      (setf (thunk-called thunk) t)
      (setf (thunk-val thunk) (funcall (thunk-val thunk))))))



(defclass engine ()
  ;; as mentioned at the beginning of the file, the engine contains three lists of functions
  ((entities
    :type vector
    :initform (vector)
    :initarg :entities
    :accessor engine-entities
    :documentation "The objectes which the simulation engine will operate on")
   (resources
    :type hash-table
    :initform (make-hash-table)
    :accessor engine-resources
    :documentation "A collection of globally-available resources such as
shaders & textures, meshes, controller input, etc. Structured like a directory,
so you can have the resource at path '(graphics shaders portal)
(i.e. graphics/shaders/portal)")
   (unloaded-resources
    :type hash-table
    :initform (make-hash-table)
    :accessor engine-unloaded-resources
    :documentation "A collection of globally-available resources such as
shaders & textures, meshes, controller input, etc. Structured like a directory,
so you can have the resource at path '(graphics shaders portal)
(i.e. graphics/shaders/portal)")

   ;; collections of functions
   (tickers 
    :type list
    :initform nil
    :initarg :tickers
    :accessor engine-tickers
    :documentation "Functions which are called once per tick: each operates on
only a single entity")

   (update-arg-tickers
    :type list
    :initform nil
    :initarg :update-arg-tickers
    :accessor engine-update-arg-tickers
    :documentation "A list of functions to call to modify update-args")

   (observer-tickers
    :type list
    :initform nil
    :initarg :observer-tickers
    :accessor engine-observer-tickers
    :documentation "A list of callback functions which are to be invoked at the  
completion of a tick")

   (should-stop
    :type boolean
    :initform nil
    :accessor engine-should-stop
    :documentation "If the engine is running, then setting this value to nil
will stop the engine")

   (inbox
    :initform (lparallel.queue:make-queue)
    :reader engine-inbox))


   (:documentation "Encapsulates the necessary state & methods to run Glint simulation/game"))

;; (defun simple-engine ()
;;   "Return an engine which is capable of running physics etc. Making use of the
;; glint physics engine"
;;   (make-instance 'engine
;;                  :update-arg-tickers (list (make-timer))
;;                  :tickers (list #'update)))

(defmethod print-object ((engine engine) stream)
  (with-slots (entities tickers) engine
    (format t "(engine ~%  :num-entities ~d ~%  :tickers ~{~a~})"
            (length entities)
            tickers)))

(defun register-resource (engine path init &key cleanup)
  "Register a resource. This resource may be provided with a function to update
it (e.g. cursor position)"
  (let ((resource (concatenate 'list
                    (list init)
                    (when cleanup (list :cleanup cleanup)))))
    (if (typep init 'thunk)
        (setf (gethash path (engine-unloaded-resources engine)) resource)
        (setf (gethash path (engine-resources engine)) resource))))


  

;; Run an instance of the simulation. Provide an initial state for both the
;; engine and update-args.
(defun run (engine update-args &key (parallel t) (frame-limit 60))
  (if parallel
      (run-parallel engine update-args frame-limit)
      (run-sequential engine update-args frame-limit)))

(defun run-parallel (engine update-args frame-limit)
  (setf lparallel:*kernel* (lparallel:make-kernel 8))
  (with-slots (entities tickers update-arg-tickers observer-tickers) engine
    (unwind-protect
         (let ((frame-start (get-internal-real-time)))
           (loop until (engine-should-stop engine) do
             ;; framerate limiter
             ;; (let* ((curr-time (get-internal-real-time))
             ;;        (sleep-time (- (/ 1 frame-limit)
             ;;                       (/ (- frame-start curr-time)
             ;;                          internal-time-units-per-second))))
             ;;   (setf frame-start curr-time)
             ;;   (when (> sleep-time 0) (sleep (/ sleep-time 100))))

             ;; First: call all update-arg functions
             (mapcar (lambda (f) (funcall f update-args)) update-arg-tickers)

             ;; Tick all entities
             (mapcar (lambda (f)
                       (lparallel:pmap
                        'vector
                        (lambda (entity)
                          (funcall f entity update-args (make-view engine)))
                        entities))
                     tickers)

             ;; Process messages: 
             ;; TODO: when processing a message, functions may send other
             ;; messages to entities. We want all of these messages to go in a
             ;; separate message pool, but for now probably safe to ignore. 
             (lparallel:pmap 'vector #'process-messages entities)

             ;; Finally, tick all observers
             (mapcar (lambda (f) (funcall f engine)) observer-tickers)))

      ;; TODO: call resource destructors!
      (format t "bye..."))))

(defun run-sequential (engine update-args frame-limit)
  (with-slots (entities tickers update-arg-tickers observer-tickers) engine
    (unwind-protect
         (let ((frame-start (get-internal-real-time)))
           (loop until (engine-should-stop engine) do
             ;; framerate limiter
             (let* ((curr-time (get-internal-real-time))
                    (sleep-time (- (/ 1 frame-limit)
                                   (/ (- frame-start curr-time)
                                      internal-time-units-per-second))))
               (setf frame-start curr-time)
               (when (> sleep-time 0) (sleep (/ sleep-time 100))))
             ;; First: call all update-arg functions
             (mapcar (lambda (f) (funcall f update-args)) update-arg-tickers)

             ;; Tick all entities
             (mapcar (lambda (f)
                       (map 'vector
                            (lambda (entity)
                              (funcall f entity update-args (make-view engine)))
                            entities))
                     tickers)

             ;; Process messages: 
             ;; TODO: when processing a message, functions may send other
             ;; messages to entities. We want all of these messages to go in a
             ;; separate message pool, but for now probably safe to ignore. 
             ;;(lparallel:pmap 'vector #'process-messages entities)

             ;; Finally, tick all observers
             (mapcar (lambda (f) (funcall f engine)) observer-tickers)))

      ;; TODO: call resource destructors!
      (format t "bye..."))))


(defclass view ()
  ((engine 
    :type engine
    :accessor view-engine
    :initarg :engine
    :documentation "The engine which this grants access to"))
  (:documentation "A thin wrapper around the engine providing access to a subset
of the engine's state"))

(defun make-view (engine)
  (make-instance 'view :engine engine))

;; 'private' view functions (not exported)
(defun view-resources (view)
  (with-slots (engine) view
    (with-slots (resources) engine 
       resources)))

(defun view-unloaded-resources (view)
  (with-slots (engine) view
    (with-slots (unloaded-resources) engine 
       unloaded-resources)))

(defgeneric get-resource (resource view)
  (:documentation "retrieve a particular resource from the resource registry"))

(defmethod get-resource (path (view view))
  (let* ((entry (gethash path (view-resources view))))
    (if entry
        (car entry)
        (let ((lzy_entry (gethash path (view-unloaded-resources view))))
          (if lzy_entry
              (progn
                (force (car lzy_entry))
                (setf (gethash path (view-resources view)) 
                      (cons (thunk-val (car lzy_entry)) (cdr lzy_entry)))
                (thunk-val (car lzy_entry)))
              (error 'unknown-resource :path 'hello))))))


(defmethod get-resource (path (engine engine))
  (let* ((entry (gethash path (engine-resources engine))))
    (if entry
        (car entry)
        (let ((lzy_entry (gethash path (engine-unloaded-resources engine))))
          (if lzy_entry
              (progn
                (force (car lzy_entry))
                (setf (gethash path (engine-resources engine)) 
                      (cons (thunk-val (car lzy_entry)) (cdr lzy_entry)))
                (thunk-val (car lzy_entry)))
              (error 'unknown-resource :path 'hello))))))


(defgeneric get-entities (view)
  (:documentation "Get the list of all entities associated with an engine/view"))


(defmethod get-entities ((engine engine))
  (engine-entities engine))


(defmethod get-entities ((view view))
  (engine-entities (view-engine view)))


(defun filter-entities (fun view)
  (remove-if-not fun (get-entities view)))


(defun register-ontick-observer (observer engine)
  (setf (engine-observer-tickers engine) (cons observer (engine-observer-tickers engine))))


(defun spawn-entity (entity view)
  (lparallel.queue:push-queue
   (lambda (engine) (vector-push entity (engine-entities engine)))
   (engine-inbox (view-engine view))))


(defun kill-engine (view)
  (setf (engine-should-stop (view-engine view)) t))
