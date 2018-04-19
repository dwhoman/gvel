;;; gvel-test.el --- Tests gvel.el.

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

(require 'gvel "gvel.el")

(defun gvel-test (correct output graph)
  (let ((dot-file (concat "tests/" output ".dot"))
	(png-file (concat "tests/" output ".png"))
	(correct-file (concat "tests/" correct)))
    (with-temp-file dot-file
      (apply 'gvel-->dot
	     (car graph)
	     (cadr graph)
	     (caddr graph)
	     (cdddr graph)))
    (assert (= 0 (shell-command (concat "dot -Tpng -o " png-file " " dot-file " > /dev/null"))))
    (assert (string= "0" (shell-command-to-string (concat "compare -metric AE " correct-file " " png-file " null:"))))))

(gvel-test "empty-correct.png" "empty" '())

(gvel-test "empty-strict-correct.png" "empty-strict" '("a" t t))

(gvel-test "empty-subgraph-correct.png" "empty-subgraph" '(nil nil nil (graph a)))

(gvel-test "struct1-correct.png" "struct1"
	   '(nil nil nil (graph a
				(node b
				      (((fillcolor . orange:blue)
					(shape . record)
					("label" . [("left" "f0") ("mid dle" "f1") ("right" "f2")]))))
				(edge [b d e]))))
(gvel-test "struct2-correct.png" "struct2"
	   '(nil nil nil (graph a
				(node b
				      (((fillcolor . orange:blue)))
				      (((shape . record)))
				      ((("label" . [1 "mid dle" right]))))
				(edge [b d e]))))

(gvel-test "struct3-correct.png" "struct3"
	   '("structs" t nil
	     (((shape . record)) node)
	     (node struct1
		   (((label . [("left" f0) ("mid dle" f1) ("right" "f2")]))))
	     (node struct2
		   (((label . [(one f0) (two f1)]))))
	     (node struct3
		   (((label . ["hello\nworld" [b [c (d here) e] f] g h]))))
	     (edge [struct1:f1 struct2:f0])
	     (edge [struct1:f2 struct3:here])))

(gvel-test "struct4-correct.png" "struct4"
	   '(R t nil
	       (((rankdir . LR)))
	       (((style . rounded)) node)
	       (node node1
		     (((shape . box))))
	       (node node2
		     (((fillcolor . yellow)
		       (style . "rounded,filled")
		       (shape . diamond))))
	       (node node3
		     (((shape . record)
		       (label . [[a b c]]))))
	       (edge [node1 node2 node3])))

(gvel-test "html1-correct.png" "html1"
	   '(structs t nil
		     (((shape . plaintext)) node)
		     (node struct1
			   (((label . (table :border "0" :cellborder "1" :cellspacing "0"
					     (tr
					      (td "left")
					      (td :port "f1" "mid dle")
					      (td :port "f2" "right")))))))
		     (node struct2
			   (((label . (table :border "0" :cellborder "1" :cellspacing "0"
					     (tr
					      (td :port "f0" "one")
					      (td "two")))))))
		     (node struct3
			   (((label . (table :border "0" :cellborder "1" :cellspacing "0" :cellpadding "4"
					    (tr
					     (td :rowspan "3" "hello" (br) "world")
					     (td :colspan "3" "b")
					     (td :rowspan "3" "g")
					     (td :rowspan "3" "h"))
					    (tr
					     (td "c")
					     (td :port "here" "d")
					     (td "e"))
					    (tr
					     (td :colspan "3" "f")))))))
		     (edge [struct1:f1 struct2:f0])
		     (edge [struct1:f2 struct3:here])))

(gvel-test "switch-correct.png" "switch"
	   '(G t nil
	       (((center . 1)
		 (rankdir . LR)
		 (bgcolor . "#808080"))
		graph)
	       (((dir . none)) edge)
	       (((width . 0.3)
		 (height . 0.3)
		 (label . ""))
		node)
	       (((shape . circle)
		 (style . invis))
		node
		(node 1)
		(node 2)
		(node 3)
		(node 4)
		(node 5)
		(node 6)
		(node 7)
		(node 8)
		(node 10)
		(node 20)
		(node 30)
		(node 40)
		(node 50)
		(node 60)
		(node 70)
		(node 80))
	       (((shape . circle)) node
		(node a)
		(node b)
		(node c)
		(node d)
		(node e)
		(node f)
		(node g)
		(node h)
		(node i)
		(node j)
		(node k)
		(node l)
		(node m)
		(node n)
		(node o)
		(node p)
		(node q)
		(node r)
		(node s)
		(node t)
		(node u)
		(node v)
		(node w)
		(node x))
	       (((shape . diamond)) node
		(node A)
		(node B)
		(node C)
		(node D)
		(node E)
		(node F)
		(node G)
		(node H)
		(node I)
		(node J)
		(node K)
		(node L)
		(node M)
		(node N)
		(node O)
		(node P)
		(node Q)
		(node R)
		(node S)
		(node T)
		(node U)
		(node V)
		(node W)
		(node X))
	       (edge [1 a (graph (node A) (node B))]
		     (((color . "#0000ff"))))
	       (edge [2 b (graph (node B) (node A))]
		     (((color . "#ff0000"))))
	       (edge [3 c (graph (node C) (node D))]
		     (((color . "#ffff00"))))
	       (edge [4 d (graph (node D) (node C))]
		     (((color . "#00ff00"))))
	       (edge [5 e (graph (node E) (node F))]
		     (((color . "#000000"))))
	       (edge [6 f (graph (node F) (node E))]
		     (((color . "#00ffff"))))
	       (edge [7 g (graph (node G) (node H))]
		     (((color . "#ffffff"))))
	       (edge [8 h (graph (node H) (node G))]
		     (((color . "#ff00ff"))))
	       (((color . "#ff0000:#0000ff")) edge
		(edge [A i (graph (node I) (node K))])
		(edge [B j (graph (node J) (node L))]))
	       (((color . "#00ff00:#ffff00")) edge
		(edge [C k (graph (node K) (node I))])
		(edge [D l (graph (node L) (node J))]))
	       (((color . "#00ffff:#000000")) edge
		(edge [E m (graph (node M) (node O))])
		(edge [F n (graph (node N) (node P))]))
	       (((color . "#ff00ff:#ffffff")) edge
		(edge [G o (graph (node O) (node M))])
		(edge [H p (graph (node P) (node N))]))
	       (((color . "#00ff00:#ffff00:#ff0000:#0000ff")) edge
		(edge [I q (graph (node Q) (node U))])
		(edge [J r (graph (node R) (node V))])
		(edge [K s (graph (node S) (node W))])
		(edge [L t (graph (node T) (node X))]))
	       (((color . "#ff00ff:#ffffff:#00ffff:#000000")) edge
		(edge [M u (graph (node U) (node Q))])
		(edge [N v (graph (node V) (node R))])
		(edge [O w (graph (node W) (node S))])
		(edge [P x (graph (node X) (node T))]))
	       (((color . "#ff00ff:#ffffff:#00ffff:#000000:#00ff00:#ffff00:#ff0000:#0000ff")) edge
		(edge [Q 10])
		(edge [R 20])
		(edge [S 30])
		(edge [T 40])
		(edge [U 50])
		(edge [V 60])
		(edge [W 70])
		(edge [X 80]))))
