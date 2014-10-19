;;;; paip.asd

(asdf:defsystem #:paip
  :serial t
  :components ((:file "packages")
	       (:file "utils" :depends-on ("packages"))
	       (:file "gps-1" :depends-on ("utils"))
	       (:file "gps-2" :depends-on ("utils"))
	       (:file "eliza" :depends-on ("utils"))))

