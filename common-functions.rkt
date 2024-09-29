#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

; Funcția primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Folosiți recursivitate pe coadă.
(define (longest-common-prefix-tail w1 w2 result)
	(if (or (eq? w1 null) (eq? w2 null))
		result
		(if (char=? (car w1) (car w2))
			(longest-common-prefix-tail (cdr w1) (cdr w2) (append result (list (car w1))))
			result
		)
	)
)

(define (longest-common-prefix w1 w2)
	(let* ((common-prefix (longest-common-prefix-tail w1 w2 '()))
		   (len (length common-prefix))
		   (partial_w1 (list (drop w1 len)))
		   (partial_w2 (list (drop w2 len)))
		  )
		(append (append (list common-prefix) partial_w1) partial_w2)
	)
)

; Functie recursiva care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.
(define (recursive-lcpol word words)
	(if (eq? words null)
		word
		(recursive-lcpol (longest-common-prefix-tail word (car words) '()) (cdr words))
	)
)

(define (longest-common-prefix-of-list words)
	(if (eq? words null)
		'()
		(recursive-lcpol (car words) (cdr words))
	)
)

;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)


; Funcția match-pattern-with-label primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)
; Obs: deși exemplele folosesc stringuri pentru claritate, vă
; reamintim că în realitate lucrăm cu liste de caractere.

(define (first-matching-branch st pattern)
	(if (eq? st null)
		null
		(if (char=? (car (get-branch-label (first-branch st))) (car pattern))
			(first-branch st)
			(first-matching-branch (other-branches st) pattern)
		)
	)
)

(define (match-pattern-with-label st pattern)
	(define current-branch (first-matching-branch st pattern))

	(if (eq? current-branch null)
		(list false '())
		(let* ((current-label (get-branch-label current-branch))
			   (common-prefix (longest-common-prefix-tail current-label pattern '()))
			   (len_pattern (length pattern))
			   (len_prefix (length common-prefix))
			   (len_label (length current-label))
			   (new_pattern (drop pattern len_prefix))
			   (subtree (other-branches current-branch))
			  )
			(if (eq? len_prefix len_pattern)
				true
				(if (eq? len_prefix len_label)
					(list current-label new_pattern subtree)
					(list false common-prefix)
				)
			)
		)
	)
)

; Funcția st-has-pattern? primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.
(define (st-has-pattern? st pattern)
	(if (eq? pattern null)
		false
		(let* ((result (match-pattern-with-label st pattern)))
			(if (eq? result true)
				true
				(if (eq? (car result) false)
					false
					(st-has-pattern? (caddr result) (cadr result))
				)
			)
		)
	)
)
