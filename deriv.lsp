(defun deriv_add(l var) "u' + v'"
    (list '+ (deriv (cadr l) var) (deriv (caddr l) var)
    )
)
(defun deriv(l var)
    "pour la puissance, on prend expt afin que ça puisse éventuellement être évalué"
    (if (listp l)
        (cond 
            ((equal (car l) '+) (deriv_add l var))
            ((equal (car l) '-) (deriv_sub l var))
            ((equal (car l) '*) (deriv_mul l var))
            ((equal (car l) '/) (deriv_div l var))
            ((equal (car l) 'expt) (deriv_pow l var)) 
            ((equal (car l) 'exp) (deriv_exp l var))
            ((equal (car l) 'cos) (deriv_cos l var))
            ((equal (car l) 'sin) (deriv_sin l var))
        )
    (deriv_term l var))
)
(defun deriv_term (terme var)
"Dérivation d'un terme par rapport à une variable var."
(if (equal var terme)
        1
        0
    )
)
(defun deriv_mul(l var)
    "u'v + uv'"
    (list '+ (list '* (deriv (cadr l) var) (caddr l)) 
            (list '* (deriv(caddr l) var) (cadr l))
    )
) 

(defun deriv_pow(l var)
    "n * u^(n-1) * u'"
    (list '* (list '* (caddr l) 
                      (list 'expt (cadr l) (- (caddr l) 1))) 
             (deriv (cadr l) var)) 
)
(defun deriv_exp(l var)
    "u' * e^u"
    (list '* (deriv (cadr l) var) 
             (list 'exp (cadr l)))
)
(defun deriv_cos(l var)
    "-u' * sin(u)"
    (list '* (list '* -1 
                    (deriv (cadr l) var)) 
             (list 'sin (cadr l)))
)
(defun deriv_sin(l var)
    "u' * cos(u)"
    (list '* (deriv (cadr l) var) 
             (list 'cos (cadr l)))
)