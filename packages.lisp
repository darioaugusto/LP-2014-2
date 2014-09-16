;; Author: Alexandre Rademaker

(defpackage #:utils
  (:use #:cl)
  (:export
   #:complemento
   #:mappend
   #:member-equal
   #:starts-with
   #:dbg
   #:dbg-on
   #:dbg-off
   #:dbg-indent
   #:find-all
   #:find-all-if))

(defpackage #:gps-1
  (:use #:cl #:utils))

(defpackage #:gps-2
  (:use #:cl #:utils))

