(defpackage pardom
  (:use cl)
  (:import-from util)
  (:export body
	   new-node node
	   tag
	   write-html))

(in-package pardom)

(defstruct node
  (attributes nil :type list)
  (script nil :type list))

(defstruct (custom-node (:include node))
  (tag (error "Missing tag") :type keyword)
  (body nil :type list))

(defun new-node (tag attrs &rest body)
  (make-custom-node :tag tag :attributes attrs :body body))

(defmethod tag ((nod custom-node))
  (custom-node-tag nod))

(defmethod body ((nod custom-node))
  (custom-node-body nod))

(defmethod (setf body) (val (nod custom-node))
  (setf (custom-node-body nod) val))

(defun to-s (val)
  (string-downcase (symbol-name val)))

(defmethod write-html ((val string) out)
  (write-string val out))

(defmethod write-html ((nod node) out)
  (write-char #\< out)
  (write-string (to-s (tag nod)) out)
  
  (dolist (a (node-attributes nod))
    (write-char #\space out)
    (write-string (to-s (first a)) out)
    (write-char #\= out)
    (write-char #\" out)
    (princ (rest a) out)
    (write-char #\" out))

  (let ((bns (custom-node-body nod)))
    (if bns
	(progn
	  (write-char #\> out)
	  
	  (dolist (bn bns)
	    (write-html bn out))

	  (write-char #\< out)
	  (write-char #\/ out)
	  (write-string (to-s (tag nod)) out)
	  (write-char #\> out))
	(progn
	  (write-char #\/ out)
	  (write-char #\> out)))))

(defun tests ()
  (let ((n (new-node :foo '((:bar . "baz")) "qux")))
    (assert (string= (with-output-to-string (out)
		       (write-html n out))
		     "<foo bar=\"baz\">qux</foo>"))))
