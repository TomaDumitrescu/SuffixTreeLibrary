#lang racket
(require "suffix-tree.rkt")
(require "common-functions.rkt")
(require "constructors.rkt")

(provide (all-defined-out))

;; Aplicatiile
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text


; Funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).
(define (substring? text pattern)
	(let ((st (text->ast text))) (equal? (st-has-pattern? st pattern) true))
)

; Funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).

(define (max-sequence sequences)
	(foldl (λ (seq acc) (if (< (length acc) (length seq)) seq acc)) null sequences)
)

(define (full-match st1 pattern acc)
	(if (not (null? pattern))
		(let* ((match-step (match-pattern-with-label st1 pattern)))
			(cond
				((equal? match-step true) (append acc (car (get-branch-label st1))))
				((equal? (car match-step) false)
					(if (null? (cadr match-step)) acc (append acc (cadr match-step))))
				(else (full-match (caddr match-step) (cadr match-step) (append acc (car match-step))))
			)
		)
		acc
	)
)

(define (longest-common-substring text1 text2)
	(let* ((st1 (text->ast text1))
		   (suffixes2 (get-suffixes (remove #\$ text2)))
		   (sequences (let iter-suff ((suffixes2 suffixes2))
				(let* ((current-suffix (car suffixes2))
					   (sequence (full-match st1 current-suffix '()))
					   (formatted-sequence (filter (λ (ch) (not (null? ch))) sequence))
					  )
					(if (>= (length suffixes2) 2)
						(cons formatted-sequence (iter-suff (cdr suffixes2)))
						formatted-sequence
					)
				))
		   )
		   (correct-sequences (filter (λ (ch) (and (list? ch) (not (null? ch)))) sequences))
		  )
		  (max-sequence correct-sequences)
	)
)


; Funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.

(define (substring-on-tree st len word wordlist)
	(cond
		((st-empty? st) (append wordlist '((#f))))
		((<= len (length word)) (append wordlist (list (take word len))))
		(else
			(let iter-branches ((st st))
				(let* ((branch (first-branch st))
					   (label (get-branch-label branch))
					   (subtree (get-branch-subtree branch))
					  )
					  (append (substring-on-tree subtree len (append word label) wordlist)
						(if (>= (length st) 2) (iter-branches (other-branches st)) '()))
				)
			)
		)
	)
)

(define (first-len-word wordlist len)
	(foldl (λ (word result) (if (and (= (length word) len) (equal? result #f))
			word result)) #f wordlist)
)

(define (repeated-substring-of-given-length text len)
	(first-len-word (substring-on-tree (text->cst text) len '() '()) len)
)
