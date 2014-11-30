;;;; paip.asd

(asdf:defsystem #:lp-2014-2
  :serial t
  :depends-on (:optima)
  :components ((:file "packages")
	       (:file "utils"    :depends-on ("packages"))
	       (:file "gps-1"    :depends-on ("utils"))
	       (:file "gps-2"    :depends-on ("utils"))
	       (:file "eliza"    :depends-on ("utils"))
	       (:file "katabank" :depends-on ("utils"))
	       (:file "tableaux" :depends-on ("utils"))
	       (:file "tableaux-opt" :depends-on ("tableaux"))
	       (:file "tableaux-ext" :depends-on ("tableaux"))
	       (:file "tableaux-safe" :depends-on ("tableaux"))
	       (:file "tableaux-vestidos" :depends-on ("tableaux"))))

