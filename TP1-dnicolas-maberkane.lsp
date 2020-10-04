"Exercice 1"

(defun reverseA(arg1 arg2 arg3)
    (list arg3 arg2 arg1)
)

(defun reverseB(L)
    (let ((listReversed '()))
    (dolist (elem L)
        (push elem listReversed)
    )
    listReversed
))
(defun reverseC(L)
    (if (> (length L) 0)
        (append (reverseC (cdr L)) (list (car L))) 
    )
)

(defun double(L)
    (let ((listDoubled '()))
    (dolist (elem (reverse L))
        (push elem listDoubled)
        (if (atom elem)
            (push elem listDoubled)
        )
    )
    listDoubled
))
(defun nombres3 (L)
    (if (and (and (numberp (car L)) (numberp (cadr L))) (numberp (caddr L))) 
        "BRAVO" 
        "PERDU"
    )
)
(defun grouper(L1 L2)
    (let ((groupedList '()))
    (loop for idx from 1 to (length L1)
        do (push (list (nth (- (length L1) idx) L1) (nth (- (length L2) idx) L2)) groupedList)
    )
    groupedList
))
(defun monReverse(L)
    (if (> (length L) 0)
        (append (reverseC (cdr L)) (list (car L))) 
    )
)
(defun palindrome(L)
    (equal (reverse L) L)
)
"Exercice 2"
(defun list-triple-couple(L)
    (mapcar (lambda (x) (list x (* 3 x))) L)
)

"Exercice 3"
(defun cles(a-list)
    (mapcar (lambda (assoc) (car assoc)) a-list)
)
(defun creation(listeCles listeVal)
    (grouper listeCles listeVal)
)
(defun my-assoc (cle a-list)
    (mapcar (lambda (x) 
        (if (not (equal (member cle x) nil)) 
            (member cle x)
        )
    ) a-list)
)


"Exercice 4"

"Question 1"

(defun bonformat(tombe)
    (if (and (listp tombe) (equal (length tombe) 5) (listp (caddr tombe)))
        t 
        nil
    )
)
(defun nom (tombe)
    (if (bonformat tombe)
        (car tombe)
        "Erreur format données tombe"
    )
)

(defun an-inhum (tombe)
    (if (bonformat tombe)
        (nth 1 tombe)
        "Erreur format données tombe"
    )
)
(defun num (tombe)
    (if (bonformat tombe)
        (cadr (caddr tombe))
        "Erreur format données tombe"
    )
)

(defun rangee (tombe)
    (if (bonformat tombe)
        (car (caddr tombe))
        "Erreur format données tombe"
    )
)

(defun debut-loc (tombe)
    (if (bonformat tombe)
        (nth 3 tombe)
        "Erreur format données tombe"
    )
)
(defun duree-loc (tombe)
    (if (bonformat tombe)
        (car (last tombe))
        "Erreur format données tombe"
    )
)
(defun emplacement (tombe)
    (if (bonformat tombe)
        (nth 2 tombe)
        "Erreur format données tombe"
    )
)

"Question 2 "

(defun qui-est-la (emplacement cimetiere)
    (if (bonformat (car cimetiere))
        (if (equal emplacement (emplacement (car cimetiere)))
            (nom (car cimetiere))
            (qui-est-la emplacement (cdr cimetiere)) 
            )
        (if (listp (nth 1 cimetiere))
            (qui-est-la emplacement (nth 1 cimetiere))
            "Emplacement non attribué"
        )
        
    )
)

"Question 3"

(defun prevoyant? (tombe)
    (> (an-inhum tombe) (debut-loc tombe))
)

"Question 4"

(defun nb-prevoyants (cimetiere)
    (setq cim (cadr cimetiere))
    (setq nb 0)
    (loop for i from 0 to (- (list-length cim) 1)
        do 
            (if (prevoyant? (nth i cim))
                (setq nb (+ nb 1))
                (setq nb nb)
            )
    )
    nb
)

"Question 5"

(defun annuaire (cimetiere rangee)
    (setq cim (cadr cimetiere))
    (setq noms '())
    (loop for i from 0 to (- (list-length cim) 1)
        do 
            (if (equal rangee (rangee (nth i cim)))
                (setq noms 
                    (push 
                        (nom (nth i cim )) noms)
                        )
            )
            (setq noms noms)
    )
    noms
)

"Question 6"

(defun doyen-benjamin (cimetiere )
    (setq cim (cadr cimetiere))
    (setq doyen (car cim))
    (setq benjamin (car cim))
    (loop for i from 1 to (- (list-length cim) 1)
        do 
            (if (> (an-inhum doyen) (an-inhum (nth i cim)))
                (setq doyen 
                    (nth i cim)
                )
                (setq doyen doyen)
            )
            (if (< (an-inhum benjamin) (an-inhum (nth i cim)))
                (setq benjamin (nth i cim))
                (setq doyen doyen)
            )
    )
    (list doyen benjamin)
)


