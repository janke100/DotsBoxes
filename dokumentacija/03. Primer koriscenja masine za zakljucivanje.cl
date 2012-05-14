;; PRIMER SE ZASNIVA NA SLEDECIM ISKAZIMA
;;    Sasa voli sve vrste hrane.
;;    Jabuke su hrana.
;;    Piletina je hrana.
;;    Hrana je sve ono sto neko jede i ne otruje se.
;;    Srdan jede kikiriki i jos je ziv.
;;    Ceca jede sve sto Srdan jede.

;; IDENTIFIKOVANA SU SLEDECA PRAVILA
;;    IF Hrana(x) THEN Voli(Sasa, x)
;;    IF Jede(x, y) AND Ne-otruje-se(x, y) THEN Hrana(y)
;;    IF Jede(Srdjan, x) THEN Jede(Ceca, x)
;; I CINJENICE
;;    Hrana(Jabuka)
;;    Hrana(Piletina)
;;    Jede(Srdjan, Kikiriki)
;;    Ne-otruje-se(Srdjan, Kikiriki)

;; PREDEFINISANI PREDIKATI

(defun !eq (a b)
  (equal a b))

(defun !ne (a b)
  (not (equal a b)))


;; PREVEDENO U ODGOVARAJUCI FORMAT PRAVILA I CINJENICE SU SLEDECI

(defparameter *T1-RULES* '(
	(if (Hrana ?x) then (Voli 'Sasa ?x))
	(if (and (Jede ?y ?x) (Ne-otruje-se ?y ?x)) then (Hrana ?x))	   
	(if (Jede 'Srdjan ?x) then (Jede 'Ceca ?x))
	))
                   
(defparameter *T1-FACTS* '(
	(Hrana 'Jabuka)
	(Hrana 'Piletina)
	(Jede 'Srdjan 'Kikiriki)
	(Ne-otruje-se 'Srdjan 'Kikiriki)
	))


;; PRIPREMA PRAVILA I CINJENICA ZA KASNIJE UPITE 
;; mora se pozvati obavezno na pocetku, 
;; ili kad se promene pravila ili cinjenice
(prepare-knowledge *T1-RULES* *T1-FACTS* 10)

;; Provera da li Sasa voli piletinu i kikiriki
;; moze da vrati (NIL)  sto znaci da ili NIL - sto znaci ne
;;(infer '(AND (Voli 'Sasa 'Piletina) (Voli 'Sasa 'Kikiriki)))

;; Upit koji vraca sta sve Sasa voli
;;(infer '(Voli 'Sasa ?x))
;; Vraca listu razlicitih lista smena, ako vrati NIL znaci da nema rezultata
;; -> (((?X 'JABUKA)) ((?X 'PILETINA)) ((?X 'KIKIRIKI)))
;; znacenje je da ?x moze biti jabuka, piletina i kikiriki

;; Upit koji vraca ko sve voli piletinu i kikiriki
;;(infer '(AND (Voli ?x 'Piletina) (Voli ?x 'Kikiriki)))

;; Primeri za dobijanje broja rezultata za upite
;;(count-results '(AND (Voli 'Sasa 'Piletina) (Voli 'Sasa 'Kikiriki)))
;;(count-results '(Voli 'Sasa ?x))
;;(count-results '(AND (Voli ?x 'Piletina) (Voli ?x 'Kikiriki)))

;; Primer sa koriscenjem predefinisanih predikata
;;(infer '(AND (Voli 'Sasa ?x) (Voli 'Sasa ?y) (Voli 'Sasa ?z) (!ne ?x ?y) (!ne ?y ?z) (!ne ?x ?z)))
;; -> (((?X 'JABUKA)   (?Y 'PILETINA) (?Z 'KIKIRIKI)) ((?X 'JABUKA)   (?Y 'KIKIRIKI) (?Z 'PILETINA))
;;     ((?X 'PILETINA) (?Y 'JABUKA)   (?Z 'KIKIRIKI)) ((?X 'PILETINA) (?Y 'KIKIRIKI) (?Z 'JABUKA))
;;     ((?X 'KIKIRIKI) (?Y 'JABUKA)   (?Z 'PILETINA)) ((?X 'KIKIRIKI) (?Y 'PILETINA) (?Z 'JABUKA)))



