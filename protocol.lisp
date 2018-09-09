;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; Copyright (c) 2007 Ivan Shvedunov. All rights reserved.
;;; Copyright (c) 2007 David Lichteblau. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(in-package :xpath)


;; generic functions of the XPath protocol

(defparameter *navigator* :default-navigator)

(macrolet ((defprotocol (name &rest extra-args)
             (let ((navi-name
                    (find-symbol
                     (format nil "~A~A" name :-using-navigator)
                     (symbol-package name))))
               `(progn
                  (defgeneric ,navi-name (navigator node ,@extra-args))
                  (defun ,name (node ,@extra-args)
                    (,navi-name *navigator* node ,@extra-args))))))
  (defprotocol xpath-protocol:node-p)
  ;; NODE-EQUAL and NODE-IDENTITY need to be implemented so that the
  ;; following equavalence holds (but NODE-EQUAL might be implemented
  ;; directly instead):
  ;;   (node-equal a b) <=> (equal (hash-key a) (hash-key b))
  ;; (This is unlike Java's equals() and hashCode(), because HASH-KEY
  ;; doesn't return a hash code, it returns an object to be used in an
  ;; EQUAL hash table.)
  (defprotocol xpath-protocol:node-equal other)
  (defprotocol xpath-protocol:hash-key)
  (defprotocol xpath-protocol:child-pipe)
  (defprotocol xpath-protocol:attribute-pipe)
  (defprotocol xpath-protocol:namespace-pipe)
  (defprotocol xpath-protocol:parent-node)
  (defprotocol xpath-protocol:node-text)
  (defprotocol xpath-protocol:qualified-name)
  (defprotocol xpath-protocol:local-name)
  (defprotocol xpath-protocol:namespace-prefix)
  (defprotocol xpath-protocol:namespace-uri)
  (defprotocol xpath-protocol:processing-instruction-target)
  (defprotocol xpath-protocol:node-type-p type)
  (defprotocol xpath-protocol:base-uri)
  (defprotocol xpath-protocol:get-element-by-id id)
  (defprotocol xpath-protocol:unparsed-entity-uri name))

(defmacro define-default-method (name (&rest args) &body body)
  (let ((navi-name
         (find-symbol
          (format nil "~A~A" name :-using-navigator)
          (symbol-package name))))
    `(defmethod ,navi-name ((navigator (eql :default-navigator)) ,@args)
       ,@body)))

(define-default-method xpath-protocol:node-p ((node t))
  nil)

(define-default-method xpath-protocol:node-equal (a b)
  (eq a b))

(define-default-method xpath-protocol:hash-key (node)
  node)


;; helper functions

(defun parent-pipe (node)
  (let ((parent (xpath-protocol:parent-node node)))
    (if parent
        (list parent)
        empty-pipe)))

(defun vector->pipe (vector &optional (start 0))
  (if (>= start (length vector))
      empty-pipe
      (make-pipe (elt vector start)
                 (vector->pipe vector (1+ start)))))

;; DOM mapping: simple slots

(define-default-method xpath-protocol:node-p ((node fxml.dom:node)) t)

(define-default-method xpath-protocol:parent-node ((node fxml.dom:attr))
  (fxml.dom:owner-element node))

(define-default-method xpath-protocol:parent-node ((node fxml.dom:node))
  (fxml.dom:parent-node node))

(define-default-method xpath-protocol:local-name ((node fxml.dom:node))
  ;; fixme?
  (or (fxml.dom:local-name node) (fxml.dom:node-name node)))

(define-default-method xpath-protocol:namespace-prefix ((node fxml.dom:node))
  (fxml.dom:prefix node))

(define-default-method xpath-protocol:namespace-uri ((node fxml.dom:node))
  (or (fxml.dom:namespace-uri node) ""))

(define-default-method xpath-protocol:qualified-name ((node fxml.dom:node))
  (fxml.dom:node-name node))

(define-default-method xpath-protocol:processing-instruction-target
    ((node fxml.dom:node))
  (fxml.dom:node-value node))

(define-default-method xpath-protocol:base-uri ((node fxml.dom:node))
  ;; fixme
  "")


;; DOM mapping: pipes

(define-default-method xpath-protocol:parent-node ((node fxml.dom:node))
  (fxml.dom:parent-node node))

(define-default-method xpath-protocol:child-pipe ((node fxml.dom:node))
  empty-pipe)

(define-default-method xpath-protocol:child-pipe ((node fxml.dom:document))
  (list (fxml.dom:document-element node)))

(define-default-method xpath-protocol:child-pipe ((node fxml.dom:element))
  (vector->pipe (fxml.dom:child-nodes node)))

(define-default-method xpath-protocol:attribute-pipe ((node fxml.dom:node))
  empty-pipe)

(define-default-method xpath-protocol:attribute-pipe ((node fxml.dom:element))
  (filter-pipe #'(lambda (item)
                   (not (equal (fxml.dom:namespace-uri item)
                               "http://www.w3.org/2000/xmlns/")))
               (vector->pipe (fxml.dom:items (fxml.dom:attributes node)))))

(define-default-method xpath-protocol:namespace-pipe ((node fxml.dom:node))
  (when (fxml.dom:parent-node node)
    (xpath-protocol:namespace-pipe (fxml.dom:parent-node node))))

(defstruct (dom-namespace
             (:constructor make-dom-namespace (parent prefix uri)))
  parent
  prefix
  uri)

(define-default-method xpath-protocol:node-p ((node dom-namespace))
  t)

(define-default-method xpath-protocol:node-equal
    ((a dom-namespace) (b dom-namespace))
  (and (eq (dom-namespace-parent a) (dom-namespace-parent b))
       (equal (dom-namespace-prefix a) (dom-namespace-prefix b))))

(define-default-method xpath-protocol:hash-key
    ((node dom-namespace))
  (cons (dom-namespace-parent node) (dom-namespace-prefix node)))

(define-default-method xpath-protocol:child-pipe
    ((node dom-namespace))
  empty-pipe)
(define-default-method xpath-protocol:attribute-pipe
    ((node dom-namespace))
  empty-pipe)
(define-default-method xpath-protocol:namespace-pipe
    ((node dom-namespace))
  empty-pipe)

(define-default-method xpath-protocol:parent-node ((node dom-namespace))
  (dom-namespace-parent node))
(define-default-method xpath-protocol:local-name ((node dom-namespace))
  (dom-namespace-prefix node))
(define-default-method xpath-protocol:qualified-name ((node dom-namespace))
  (dom-namespace-prefix node))
(define-default-method xpath-protocol:namespace-prefix ((node dom-namespace))
  nil)
(define-default-method xpath-protocol:namespace-uri ((node dom-namespace))
  "")

(define-default-method xpath-protocol:namespace-pipe ((node fxml.dom:element))
  ;; FIXME: completely untested
  ;; FIXME: rewrite this lazily?
  (let ((table (make-hash-table :test 'equal))
        (result '()))
    (labels ((record* (parent prefix uri)
               (unless (or (equal prefix "xmlns")
                           (gethash prefix table))
                 (setf (gethash prefix table)
                       (make-dom-namespace parent prefix uri))))
             (record (parent node)
               (record* parent
                        (or (fxml.dom:prefix node) "")
                        (fxml.dom:namespace-uri node)))
             (recurse (node)
               (record node node)
               (dolist (attribute (fxml.dom:items (fxml.dom:attributes node)))
                 (cond
                   ((equal (fxml.dom:namespace-uri attribute)
                           "http://www.w3.org/2000/xmlns/")
                    ;; record explicitly declared namespaces, which might
                    ;; not be in use anywhere
                    (record* node
                             (fxml.dom:local-name attribute)
                             (fxml.dom:value attribute)))
                   ((plusp (length (fxml.dom:prefix attribute)))
                    ;; record namespaces from DOM 2 slots, which might not
                    ;; be declared in an attribute
                    (record node attribute))))
               (let ((parent (fxml.dom:parent-node node)))
                 (when parent
                   (recurse parent)))))
      (record* nil "xml" "http://www.w3.org/XML/1998/namespace")
      (recurse node))
    (maphash #'(lambda (prefix nsnode)
                 (declare (ignore prefix))
                 (push nsnode result))
             table)
    result))

(define-default-method xpath-protocol:node-text ((node fxml.dom:node))
  ;; FIXME: support document and document-fragment
  (with-output-to-string (s)
    (labels ((write-text (node)
               (let ((value (fxml.dom:node-value node)))
                 (when value (write-string value s))
                 (unless (fxml.dom:attribute-p node) ;; FIXME: verify CDATA sections
                   (fxml.dom:do-node-list (child (fxml.dom:child-nodes node))
                     (cond ((or (fxml.dom:element-p child)
                                (fxml.dom:entity-reference-p child))
                            (write-text child))
                           ((or (fxml.dom:text-node-p child)
                                (fxml.dom:attribute-p child)
                                (fxml.dom:cdata-section-p child))
                            (write-string (fxml.dom:node-value child) s))))))))
      (write-text node))))

(define-default-method xpath-protocol:node-text ((node dom-namespace))
  (dom-namespace-uri node))

;; currently computed from child-pipe
;;; (defmethod preceding-sibling-pipe ()
;;;   (let ((parent (fxml.dom:parent-node node)))
;;;     (if parent
;;;     (let* ((children (fxml.dom:child-nodes parent))
;;;            (pos (position node children)))
;;;       (loop
;;;          for i from (1- pos) downto 0
;;;          collect (elt children i)))
;;;     empty-pipe)))

(define-default-method xpath-protocol:node-type-p ((node fxml.dom:node) type)
  (declare (ignore type))
  nil)

(define-default-method xpath-protocol:node-type-p ((node dom-namespace) type)
  (declare (ignore type))
  nil)

(macrolet ((deftypemapping (class keyword)
             `(define-default-method xpath-protocol:node-type-p
                  ((node ,class) (type (eql ,keyword)))
                t)))
  (deftypemapping fxml.dom:comment :comment)
  (deftypemapping fxml.dom:processing-instruction :processing-instruction)
  (deftypemapping fxml.dom:text :text)
  (deftypemapping fxml.dom:attr :attribute)
  (deftypemapping fxml.dom:element :element)
  (deftypemapping dom-namespace :namespace)
  (deftypemapping fxml.dom:document :document))

(define-default-method xpath-protocol:get-element-by-id ((node fxml.dom:node) id)
  (fxml.dom:get-element-by-id
   (if (fxml.dom:document-p node) node (fxml.dom:owner-document node)) id))

(define-default-method xpath-protocol:unparsed-entity-uri
    ((node fxml.dom:node) name)
  (let ((dtd (fxml.rune-dom::dtd (if (fxml.dom:document-p node)
                                     node
                                     (fxml.dom:owner-document node)))))
    (when dtd
      (let ((entdef (cdr (gethash name (fxml::dtd-gentities dtd)))))
	(when (typep entdef 'fxml::external-entdef)
	  (let ((uri (fxml::extid-system (fxml::entdef-extid entdef))))
	    (when uri
              (quri:render-uri uri nil))))))))

;; Character data

(define-default-method xpath-protocol:local-name
    ((node fxml.dom:character-data))
  "")

(define-default-method xpath-protocol:namespace-prefix
    ((node fxml.dom:character-data))
  "")

(define-default-method xpath-protocol:namespace-uri
    ((node fxml.dom:character-data))
  "")

(define-default-method xpath-protocol:qualified-name
    ((node fxml.dom:character-data))
  "")
