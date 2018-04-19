;;; gvel-org.el --- Graph links between Org files -*- lexical-binding: t; -*-

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

;; Keywords: dot dot-language dotlanguage graphviz graphs org

(require 'gvel)
(require 'org)

;;;###autoload
(defun gvel-org-links (files link-type graph-style node-style edge-style)
  "Graph links between `org-mode' files. FILES is a list of Org
files. LINK-TYPE is the org link :type property to match, a
regular expression. GRAPH-STYLE is an s-expression describing the
Graphviz graph's graph attributes. NODE-STYLE are the node
attributes, and EDGE-STYLE are the edge attributes."
  (let ((file-table (make-hash-table))
	(graph '()))
    (dolist (f files)
      (let ((file (file-truename f))
	    (file-base (file-name-base f)))
	(condition-case err
	    (with-temp-buffer
	      (insert-file-contents file)
	      (puthash file-base
		       (append (gethash file-base file-table '())
			       (delq nil
				     (org-element-map
					 (org-element-parse-buffer)
					 'link
				       (lambda (hl)
					 (when (string-match link-type (org-element-property :type hl))
					   (list
					    (substring (org-element-property :raw-link hl) 5)
					    (with-temp-buffer
					      (insert (or (third hl) (org-element-property :path hl)))
					      (buffer-substring-no-properties (point-min) (point-max)))))))))
		       file-table))
	  (error (princ (format "Error processing %s: %s" file err))))))
    (maphash #'(lambda (key values)
		 (push `(node ,(format "\"%s\"" key)) graph))
	     file-table)
    (maphash #'(lambda (key values)
		 (setq graph (append graph (mapcar #'(lambda (value)
						       `(edge [,(format "\"%s\"" key) ,(format "\"%s\"" (first value))]))
						     values))))
	     file-table)
    (append `("orgLinks" t nil
	      (,graph-style graph)
	      (,node-style node)
	      (,edge-style edge))
	    graph)))

(provide 'gvel-org)
