;;; gvel.el --- S-expression grammar for generating Graphviz dot graphs -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Devin Homan <devinwh7 at gmail dot com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; Keywords: dot dot-language dotlanguage graphviz graphs

;;; Commentary:

;; Generate dot code using s-expressions with `gvel->dot' or `gvel-tree->dot':

;; (gvel->dot "G" t t
;; 	   '(node b
;; 		 (((fillcolor . orange:blue)
;; 		   (shape . record)
;; 		   ("label" . [("left" "f0") ("mid dle" "f1") ("right" "f2")]))))
;; 	   '(edge [b d e]))

;; (gvel-tree->dot '(G t t
;; 		    (node b
;; 			  (((fillcolor . orange:blue)
;; 			    (shape . record)
;; 			    ("label" . [("left" "f0") ("mid dle" "f1") ("right" "f2")]))))
;; 		    (edge [b d e])))

;; each produce the following dot code:

;; strict digraph "G" {
;; b  [fillcolor="orange:blue", shape="record", label="<f0> left|<f1> mid&#92; dle|<f2> right"] 
;; b -> d -> e
;; }

;; HTML labels are produced using `xmlgen':

;; (gvel-tree->dot
;;  '(structs nil nil
;; 	   (((shape . plaintext)) node)
;; 	   (node struct1
;; 		 (((label . (table :border "0" :cellborder "1" :cellspacing "0"
;; 				   (tr
;; 				    (td "left")
;; 				    (td :port "f1" "mid dle")
;; 				    (td :port "f2" "right")))))))))

;; produces the following (with the HTML wrapped):

;; graph "structs" {
;;  node [shape="plaintext"]
;;  struct1  [label=<<table border="0" cellborder="1" cellspacing="0">
;;  		     <tr>
;;  		      <td>left</td>
;;  		      <td port="f1">mid dle</td>
;;  		      <td port="f2">right</td>
;;  		     </tr>
;;  		    </table>>] 
;; }

(require 'xmlgen)
(require 'cl-lib)
(require 'graphviz-dot-mode)

(defun gvel--sym-strp (value)
  "Test whether VALUE is a Graphviz symbol."
  (and (not (null value))
       (or (symbolp value) (stringp value) (numberp value))))

(defun gvel--sym-str (value)
  "Convert VALUE to a string to be used as a Graphviz symbol."
  (pcase value
    ((pred symbolp)
     (if (null value)
	 ""
       (symbol-name value)))
    ((pred numberp) (number-to-string value))
    ((pred stringp) value)
    (_ (error "Unhandled value type for %s" value))))

(defun gvel--escape-string (str)
  "Escape characters that are special in Graphviz."
  (replace-regexp-in-string
   "\r" "&#92;r"
   (replace-regexp-in-string
    "\\\\" "&#92;&#92;"
    (replace-regexp-in-string
     "\n" "&#92;n"
     (replace-regexp-in-string "\\([ {}|<>]\\)" "&#92;\\1" str)))))

(defun gvel--create-record (record)
  "Given an s-expression, RECORD, returns a Graphviz record as a string."
  (unless (arrayp record) (error "Record is not an array"))
  (mapconcat #'(lambda (cell)
		 (cond
		  ((listp cell)
		   (concat "<" (gvel--sym-str (second cell)) "> "
			   (gvel--escape-string
			    (gvel--sym-str (first cell)))))
		  ((gvel--sym-strp cell) (gvel--escape-string (gvel--sym-str cell)))
		  ((arrayp cell) (concat "{" (gvel--create-record cell) "}"))
		  (t
		   (error "Unhandled type for %S." cell))))
	     record "|"))

(defun gvel-->dot (name directedp strictp &rest graph)
  "Parse the graph defined by NAME, DIRECTEDP, STRICTP, and GRAPH
and insert the dot tokens."
  (unless (listp graph) (error "GRAPH must be a list"))
  (cl-labels
      ((parse-graph (graph)
		    (let ((named (gvel--sym-strp (cadr graph))))
		      (insert "subgraph ")
		      (when named
			(insert (gvel--sym-str (cadr graph)))
			(insert " "))
		      (insert "{\n")
		      (dolist (subtree (if named (cddr graph) (cdr graph)))
			(pcase (car subtree)
			  ('node (parse-node subtree))
			  ('edge (parse-edge subtree))
			  ('graph (parse-graph subtree))
			  ((pred listp) (parse-attribute subtree t))
			  (_ (error "Unhandled type %S" (car subtree))))
			(insert "\n"))
		      (insert "}")))
       (parse-node (node)
		   (if (not (gvel--sym-strp (cadr node)))
		       (error "Un-named node.")
		     (insert (gvel--sym-str (cadr node)))
		     (insert " ")
		     (dolist (subtree (cddr node))
		       (pcase (car subtree)
			 ('graph (error "A node cannot have a subgraph."))
			 ('edge (error "A node cannot have an edge."))
			 ('node (error "A node cannot have a sub-node"))
			 ((pred listp) (parse-attribute subtree))
			 (_ (error "Unhandled type %S" (car subtree)))))
		     (insert " ")))
       (parse-edge (edge)
		   (unless (and (arrayp (cadr edge))
				(<= 2 (length (cadr edge))))
		     (error "Invalid edge specification."))
		   (let ((from-val (aref (cadr edge) 0))
			 (index 1)
			 (len (length (cadr edge))))
		     (cond
		      ((gvel--sym-strp from-val)
		       (insert (gvel--sym-str from-val)))
		      ((and (listp from-val) (eq 'graph (car from-val)))
		       (parse-graph from-val))
		      (t (error "Unhandled edge FROM value: %S" from-val)))
		     (while (< index len)
		       (insert " ")
		       (if directedp
			   (insert "->")
			 (insert "--"))
		       (insert " ")
		       (let ((to-val (aref (cadr edge) index)))
			 (cond
			  ((gvel--sym-strp to-val)
			   (insert (gvel--sym-str to-val)))
			  ((and (listp to-val) (eq 'graph (car to-val)))
			   (parse-graph to-val))
			  (t (error "Unhandled edge TO value: %S" to-val))))
		       (incf index)))
		   (dolist (subtree (cddr edge))
		     (pcase (car subtree)
		       ('graph (error "An edge cannot have a subgraph attribute."))
		       ('edge (error "An edge cannot have an edge attribute."))
		       ('node (error "An edge cannot have a sub-node attribute."))
		       ((pred listp) (parse-attribute subtree))
		       (_ (error "Unhandled type %S" (car subtree))))))
       (parse-attribute (attribute &optional stmtp)
			(when (and (null stmtp)
				   (not (null (second attribute))))
			  (error "Attribute cannot be a statement attribute."))
			(when (cddr attribute) (insert "{"))
			(when stmtp
			  (insert " ")
			  (insert (gvel--sym-str (or (second attribute) "graph"))))
			(insert " [")
			(insert
			 (mapconcat #'(lambda (pair)
					(concat
					 (gvel--sym-str (car pair))
					 "="
					 (gvel--sym-str (parse-value (cdr pair)))))
				    (car attribute) ", "))
			(insert "]")
			(when (cddr attribute)
			  (dolist (elt (cddr attribute))
			    (insert " ")
			    (pcase (car elt)
			      ('node (parse-node elt))
			      ('edge (parse-edge elt))
			      ('graph (parse-graph elt))
			      (_ (error "Unhandled type %S" (car elt)))))
			  (insert "}")))
       (parse-value (value)
		    (pcase value
		      ((pred booleanp) (concat "\""(if value "true" "false") "\""))
		      ((pred gvel--sym-strp) (concat "\"" (gvel--sym-str value) "\""))
		      ((pred listp)
		       (concat "<" (xmlgen value)  ">"))
		      ((pred arrayp)
		       (concat "\"" (gvel--create-record value) "\""))
		      (_ (error "Unhandled type %S" value)))))
    (when strictp
      (insert "strict "))
    (if directedp
	(insert "digraph")
      (insert "graph"))
    (insert " ")
    (when name
      (insert "\"")
      (insert (gvel--sym-str name))
      (insert "\" "))
    (insert "{\n")
    (dolist (element graph)
      (if (not (listp element))
	  (error "Non-list element in sub-tree.")
	(pcase (car element)
	  ('graph (parse-graph element))
	  ('node (parse-node element))
	  ('edge (parse-edge element))
	  ((pred listp) (parse-attribute element t))
	  (_ (error "Unhandled type %S" (car element))))
	(insert "\n")))
    (insert "}")))

;;;###autoload
(defun gvel->dot (&optional name directedp strictp &rest graph)
  "Generates dot code in a new buffer.  Use `graphviz-dot-mode'
to run the generated code. NAME is the graph's name. DIRECTEDP is
t or nil (directed or undirected). STRICTP is t or nil; t will
add the 'strict' keyword to the output graph. GRAPH is zero or
more s-expressions that are the graph's contents."
  (let ((buffer (generate-new-buffer "gvel")))
    (switch-to-buffer buffer)
    (graphviz-dot-mode)
    (apply 'gvel-->dot name directedp strictp graph)))

;;;###autoload
(defun gvel-tree->dot (graph)
  "Applies GRAPH to `gvel->dot'. GRAPH is an s-expression. The
first three elements of GRAPH are passed to `gvel->dot's optional
arguments. The rest is passed to &rest."
  (unless (listp graph) (error "GRAPH must be a list"))
  (apply 'gvel->dot
	 (car graph)
	 (cadr graph)
	 (caddr graph)
	 (cdddr graph)))

(provide 'gvel)
;;; gvel ends here
