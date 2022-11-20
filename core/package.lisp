(defpackage :glint.core
  (:use :cl)
  (:export
   ;; utils (todo: move into utils)
   :lazy

   ;; engine stuff
   :engine :engine-entities :engine-resources :engine-unloaded-resources
   :engine-tickers :engine-update-arg-tickers :engine-observer-tickers
   :engine-should-stop
   :register-resource :register-ontick-observer
   :filter-entities :spawn-entity
   :run
   :kill-engine

   ;; entity styff
   :send-message

   ;; view stuff
   :view :get-resource :get-entities

   ;; entity stuff
   :entity :update

   ;; TODO: export transforms, meshes etc. into a 'math' or similar package 
   ;; transform.lisp
   :transform :transform-position :transform-rotation :transform-scale :calc-model-matrix
   :rotate :rotate-msg
   :translate :translate-msg
   :scale :scale-msg))

