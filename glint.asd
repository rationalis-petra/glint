(require 'asdf)

(defsystem "glint"
  :name "glint"
  :version "1.0.0"
  :maintainer "Connor Redfern"
  :author "Connor Redfern"
  :license "MIT"
  :description "A game engine for common lisp"
  :depends-on ("cl-glfw3"
               "cl-opengl"
               "array-operations"
               "split-sequence"
               "bordeaux-threads"
               "lparallel")
  :components ((:file "glint" :depends-on (core graphics physics))

               (:module graphics
                :pathname "graphics"
                :depends-on (math core)
                :components ((:file "window" :depends-on ("package" "render-system"))
                             (:file "render-system" :depends-on ("package" "camera"))
                             (:file "camera" :depends-on ("package"))
                             (:file "mesh-loader" :depends-on ("package"))
                             (:file "package")))

               (:module physics
                :pathname "physics"
                :depends-on (math core)
                :components ((:file "geometry" :depends-on ("package"))
                             (:file "package")))
               
               (:module core
                :pathname "core"
                :depends-on (math)
                :components ((:file "update-args" :depends-on ("package"))
                             (:file "transform" :depends-on ("package" "engine"))
                             (:file "engine" :depends-on ("package" "update-args"))
                             (:file "package")))

               (:module math
                :pathname "math"
                :components ((:file "matrix" :depends-on ("vector"))
                             (:file "vector")
                             (:file "package")))

               ))

