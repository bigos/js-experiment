(asdf:defsystem #:js-experiment
  :description "JS generation experiment"
  :author "Jacek Podkanski"
  :depends-on (#:cl-who)
  :serial T
  :components ((:file "package")
               (:file "js-experiment")))
