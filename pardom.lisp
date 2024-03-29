(defpackage pardom
  (:use cl)
  (:import-from util)
  (:export append-body
	   body
	   document
	   new-document new-node node
	   tag to-html
	   write-html))

(in-package pardom)

(defstruct node
  (attributes nil :type list)
  (script nil :type list))

(defmethod body ((nod node))
  (declare (ignore nod)))

(defstruct (custom-node (:include node))
  (tag (error "Missing tag") :type keyword)
  (body nil :type list))

(defun new-node (tag &optional attrs &rest body)
  (make-custom-node :tag tag :attributes attrs :body body))

(defmethod tag ((nod custom-node))
  (custom-node-tag nod))

(defmethod body ((nod custom-node))
  (reverse (custom-node-body nod)))

(defmethod (setf body) (val (nod custom-node))
  (setf (custom-node-body nod) val))

(defun append-body (target node)
  (push node (body target)))

(defmethod to-html ((val symbol) &key)
  (string-downcase (symbol-name val)))

(defmethod write-html ((val string) out &key pretty?)
  (declare (ignore pretty?))
  (write-string val out))

(defmethod write-html ((nod node) out &key pretty?)
  (write-char #\< out)
  (write-string (to-html (tag nod)) out)
  
  (dolist (a (node-attributes nod))
    (write-char #\space out)
    (write-string (to-html (first a)) out)
    (write-char #\= out)
    (write-char #\" out)
    (princ (rest a) out)
    (write-char #\" out))

  (let ((bns (body nod)))
    (if bns
	(progn
	  (write-char #\> out)

	  (when pretty?
	    (terpri out))
	  
	  (dolist (bn bns)
	    (write-html bn out :pretty? pretty?)

	    (when pretty?
	      (terpri out)))

	  (write-char #\< out)
	  (write-char #\/ out)
	  (write-string (to-html (tag nod)) out)
	  (write-char #\> out))
	(progn
	  (write-char #\/ out)
	  (write-char #\> out)))))

(defmethod to-html ((nod node) &key pretty?)
  (with-output-to-string (out)
    (write-html nod out :pretty? pretty?)))

(defstruct (document (:include node))
  (head (new-node :head) :type node)
  (title (new-node :title) :type node)
  (body (new-node :body) :type node))

(defun new-document (&key title)
  (let ((d (make-document)))
    (append-body (document-head d) (document-title d))

    (when title
      (append-body (document-title d) title))
    
    d))

(defmethod tag ((doc document))
  :html)

(defmethod body ((doc document))
  (list (document-head doc) (document-body doc)))

(defmethod (setf body) (val (doc document))
  (setf (body (document-body doc)) val))

(defmethod write-html ((doc document) out &key pretty?)
  (write-string "<!DOCTYPE html>" out)
  
  (when pretty?
    (terpri out))

  (call-next-method))

(defun node-tests ()
  (let ((n (new-node :foo '((:bar . "baz")) "qux")))
    (assert (string= (with-output-to-string (out)
		       (write-html n out))
		     "<foo bar=\"baz\">qux</foo>"))))

(defun document-tests ()
  (let ((d (new-document :title "foo")))
    (assert (string= (with-output-to-string (out)
		       (write-html d out :pretty? t))
		     "<!DOCTYPE html>
<html>
<head>
<title>
foo
</title>
</head>
<body/>
</html>"))))

(defun tests ()
  (node-tests)
  (document-tests))
