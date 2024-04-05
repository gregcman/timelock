;;;; timelock.asd

(asdf:defsystem #:timelock
  :description "Describe timelock here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:ironclad #:utility #:alexandria)
  :components ((:file "package")
               (:file "timelock")))
