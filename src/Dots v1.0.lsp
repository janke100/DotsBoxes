;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;====AUTORI====;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Milan Stojanovic	11324
; Milos Milenkovic	11211
; Aleksandar Tosovic	11340


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;====PODACI (GLOBALNE PROMENLJIVE)====;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; asocijativna lista za prevodjenje slova u brojeve
(setf translation '((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7) (h 8) (i 9) (j 10)
					(k 11) (l 12) (m 13) (n 14) (o 15) (p 16) (q 17) (r 18) (s 19)
					(t 20) (u 21) (v 22) (w 23) (x 24) (y 25) (z 26)))

; asocijativna lista za prevodjenje brojeva u slova
(setf inverse-translation '((1 a) (2 b) (3 c) (4 d) (5 e) (6 f) (7 g) (8 h) (9 i) (10 j)
							(11 k) (12 l) (13 m) (14 n) (15 o) (16 p) (17 q) (18 r) (19 s)
							(20 t) (21 u) (22 v) (23 w) (24 x) (25 y) (26 z)))

; velicina table (broj tacaka u vrsti)
(setf board-size 0)

; promenljiva koja pamti da li se menja igrac nakon odigranog poteza, t.j. da li je bilo poena
(setf change-next-player t)

; struktura koja pamti stanje igre; stanje cine lista poteza, lista poena i trenutni igrac
(defstruct game-state
			moves	; lista odigranih poteza u formatu npr. (((2 1) t) ((4 3) t) ((2 5) '())...); svaki potez ima dva elementa; prvi element je lista sa indeksom kolone i vrste, a drugi je ili 't ako je potez odigran ili '() ako nije; indeksi su jedan paran, a jedan neparan
			points ; lista postignutih poena (tj. ikseva i okseva) u formatu npr. ((2 2) x) ((4 4) o) ((2 4) '())...); svaki poen ima 2 elementa; prvi element je lista sa indeksom kolone i vrste, a drugi je ili 'x ili 'o ako je poen postignut ili '() ako nije; oba indeksa su parna; player je uvek x, a computer je o
			next-player) ; pokazuje koji je igrac na potezu: 1 - player, 2 - computer

; pravo stanje na tabli u toku partije
(setf main-state
	(make-game-state
		:moves '()
		:points '()
		:next-player 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;====FUNKCIJE ZA MAIN====;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; glavna f-ja
(defun main ()
	(create-board (enter-board-size) (enter-first-player))
	(draw-board main-state)
	(dotimes (i (* 2 board-size (1- board-size))) ; petlja se vrti onoliko puta koliko je maximum mogucih poteza, tj. 2*board-size*(board-size-1)
		(if (= (game-state-next-player main-state) 1)	(set-main-state (update-state main-state (enter-player-move)))	; update-state samo vraca novo azurirano stanje, a set-main-state ga pamti u main-state
														(set-main-state (update-state main-state (enter-computer-move))))
		(draw-board main-state))
	(end-game)
	(if (play-again) (main)))


; ucitava sa tastature velicinu table (broj tacaka u vrsti), proverava ispravnost ulaza i vraca tu vrednost
(defun enter-board-size ()
	(format t "~%~%Please enter board size (number of dots per row) between 3 and 13: ")
	(let ((size (read)))
		(cond 	((and (numberp size) (>= size 3) (<= size 13)) size)
				(t (format t "~%~%Wrong input!") (enter-board-size)))))


; ucitava sa tastature ko prvi igra (1 - player, 2 - computer), proverava ispravnost ulaza i vraca 1 ili 2
(defun enter-first-player ()
	(format t "~%~%Enter '1' (player makes the first move) or '2' (computer makes the first move): ")
	(let ((first (read)))
		(cond 	((or (equal first 1) (equal first 2)) first)
				(t (format t "~%~%Wrong input!") (enter-first-player)))))


; prima dimenziju table (broj tacaka u vrsti) i ko igra prvi (1 ili 2) i inicijalizuje tablu
(defun create-board (size first)
	(setf board-size size)
	(setf main-state
		(make-game-state
			:moves '()
			:points '()
			:next-player first))
	(dotimes (i (1- (* 2 board-size)))
		(dotimes (j (1- (* 2 board-size)))
			(cond	((and (evenp (1+ j)) (evenp (1+ i))) (setf (game-state-points main-state) (cons (list (list (1+ j) (1+ i)) '()) (game-state-points main-state))))	; ako su indeksi parni inicijalizujes poen (neosvojen poen ima '() umestoo 'x ili 'o)
					((or (and (evenp (1+ j)) (oddp (1+ i))) (and (oddp (1+ j)) (evenp (1+ i)))) (setf (game-state-moves main-state) (cons (list (list (1+ j) (1+ i)) '()) (game-state-moves main-state))))	; ako su indeksi jedan paran a jedan neparan inicijalizujes potez (neodigran potez ima '() umesto t)
					(t '())))))


; prima stanje (game-state) s i stampa ga na izlaz; prikaz stanja je implementiran na drugi nacin, koji je prikazan na desnoj slici na slajdu; krecemo se kroz matricu (tablu) i za svaku poziciju ispitujemo da li postoji u listama poteza i poena
(defun draw-board (s)
	(format t "~%~%~%")
	(do ((i (1- (* 2 board-size)) (1- i))) ((< i 0) '()) ; i i j se vrte po 2*board-size puta (po 2*board-size-1 za sve pozicije na tabli i plus po jednom za oznake kolona i vrsta)
		(format t "~%")
		(if (= 0 i) (do ((j 0 (1+ j))) ((> j (1- (* 2 board-size))) '())	; stampa se vrsta sa oznakama kolona (slova)
						(if (= j 0) (format t "~2A " #\ ) (format t "~A " (cadr (assoc j inverse-translation)))))	; j se prevodi u slovo
					(do	((j 0 (1+ j))) ((> j (1- (* 2 board-size))) '())
						(cond	((= 0 j) (format t "~2A " i))	; stampa kolonu sa oznakama vrsta
								((and (oddp i) (oddp j)) (format t "* ")) ; ukoliko su oba indeksa neparna, u tom polju na tabli je tacka (*)
								((and (evenp i) (evenp j)) (let ((sign (cadr (assoc (list j i) (game-state-points s) :test #'equal))))
																(if (equal sign '()) (format t "  ") (format t "~A " sign))))	; ukoliko su oba indeksa parna, u tom polju na tabli su iks ili oks ako ih ima, tj. blanko znak ako ih nema
								((and (oddp i) (evenp j)) (if (cadr (assoc (list j i) (game-state-moves s) :test #'equal)) (format t "- ") (format t "  ")))	; stampa horizontalni potez ako je odigran
								(t (if (cadr (assoc (list j i) (game-state-moves s) :test #'equal)) (format t "| ") (format t "  ")))))))	; stampa vertikalni potez ako je odigran
	(format t "~%~%"))


; ucitava potez sa tastature, u formatu npr. (b 1), proverava ispravnost ulaza, prevodi potez u (2 1) i vraca prevedeni potez
(defun enter-player-move ()
	(format t "~%~%Enter your move (e.g. '(i 10)'): ")
	(let ((new-move (read)))	; treba proveriti 1) dal je lista 2) dal ima 2 elementa 3) dal je prvi element slovo a drugi broj 4) dal su oba elementa u opsegu 5) dal je jedan paran a jedan neparan (jer u suprotnom se radi ili o tacki ili o iksu ili o oksu) 6) dal vec postoji taj potez
		(cond	((atom new-move) (format t "~%~%Wrong input format!") (enter-player-move))
				((not (equal (length new-move) 2)) (format t "~%~%Wrong input format!") (enter-player-move))
				((or (not (assoc (car new-move) translation)) (not (numberp (cadr new-move)))) (format t "~%~%Wrong input format!") (enter-player-move))
				((or (> (cadr (assoc (car new-move) translation)) (1- (* 2 board-size))) (> (cadr new-move) (1- (* 2 board-size)))) (format t "~%~%Wrong input! Move is out of bounds!") (enter-player-move))
				((or (and (evenp (cadr (assoc (car new-move) translation))) (evenp (cadr new-move))) (and (oddp (cadr (assoc (car new-move) translation))) (oddp (cadr new-move)))) (format t "~%~%Wrong input! Move is invalid!") (enter-player-move))
				((cadr (assoc (list (cadr (assoc (car new-move) translation)) (cadr new-move)) (game-state-moves main-state) :test #'equal)) (format t "~%~%Move ~A already exists!" new-move) (enter-player-move))
				(t (list (cadr (assoc (car new-move) translation)) (cadr new-move))))))	; prevedi prvi indeks u broj i vrati potez


; prima tekuce stanje (strukturu game-state) i novi potez (u formatu npr. (2 1)) i vraca novo stanje sa azuriranim listama poteza i poena i adekvatno namestenim next-player-om
(defun update-state (s move)
	(let*	((new-mvs (update-moves s move))
			(new-pnts (update-points s move))
			(new-np (update-next-player (game-state-next-player s))))
		(make-game-state
			:moves new-mvs
			:points new-pnts
			:next-player new-np)))


; odredjuje koji igrac je sledeci na potezu; zove se nakon azuriranja liste poena na osnovu novog poteza; prima info o tome ko je bio na potezu (1 ili 2); vraca ko treba da bude na potezu (1 ili 2)
(defun update-next-player (current-player)
	(if change-next-player (+ (mod current-player 2) 1) current-player)) ; ukoliko nije bilo novih poena treba promeniti igraca, a ukoliko jeste igrac se ne menja


; prima stanje (strukturu game-state) i novi potez (u formatu npr. (2 1))  i vraca azuriranu listu poteza u "punom" formatu (kao sto je game-state-moves)
(defun update-moves (s move)
	(cons (list move t) (remove (list move '()) (game-state-moves s) :test #'equal)))


; prima stanje (strukturu game-state) i novi potez (u formatu npr. (2 1)); proverava okolinu poteza da vidi dal su tim potezom zatvoreni neki kvadrati i vraca azuriranu listu poena u "punom" formatu (kao sto je game-state-points) (ako novi potez ne postize nove poene, vraca nepromenjenu listu poena)
(defun update-points (s move)
	(setf change-next-player t) ; postavlja change-next-player na t jer se pretpostavlja da se menja igrac
	(if (oddp (car move)) (update-vertical s move) (update-horizontal s move)))	; proverava da li je potez horizontalan ili vertikalan


; prima stanje i vertikalan potez; proverava kvadrate desno i levo od poteza i vraca azuriranu listu poena
(defun update-vertical (s move)
	(cond	((= 1 (car move)) (update-right s move)) ; ako je vertikalan potez u prvoj koloni, proverava se samo udesno da li je zatvoren kvadrat
			((= (1- (* 2 board-size)) (car move)) (update-left s move)) ; ako je u zadnjoj koloni, proverava se samo ulevo
			(t (update-left (make-game-state :moves (game-state-moves s) :points (update-right s move) :next-player (game-state-next-player s)) move)))) ; ako je unutrasnji, proverava se i ulevo i udesno


; prima stanje i vertikalan potez; proverava kvadrat levo od poteza i vraca azuriranu listu poena
(defun update-left (s move)
	(cond ((and	(cadr (assoc (list (- (car move) 2) (cadr move)) (game-state-moves s) :test #'equal)) ; da li postoji potez levo
				(cadr (assoc (list (1- (car move)) (1- (cadr move))) (game-state-moves s) :test #'equal)) ; da li postoji potez dole-levo
				(cadr (assoc (list (1- (car move)) (1+ (cadr move))) (game-state-moves s) :test #'equal))) ; da li postoji potez gore-levo
			(setf change-next-player nil) ; postavlja change-next-player na '() jer je postignut poen
			(cons (list (list (1- (car move)) (cadr move)) (if (= (game-state-next-player s) 1) 'x 'o)) (remove (list (list (1- (car move)) (cadr move)) '()) (game-state-points s) :test #'equal))) ; ako je zatvoren kvadrat ulevo, vraca se azurirana lista poena
		(t (game-state-points s)))) ; ako nije zatvoren kvadrat ulevo, vraca se nepromenjena lista poena


; prima stanje i vertikalan potez; proverava kvadrat desno od poteza i vraca azuriranu listu poena
(defun update-right (s move)
	(cond ((and	(cadr (assoc (list (+ (car move) 2) (cadr move)) (game-state-moves s) :test #'equal)) ; da li postoji potez desno
				(cadr (assoc (list (1+ (car move)) (1- (cadr move))) (game-state-moves s) :test #'equal)) ;da li postoji potez dole-desno
				(cadr (assoc (list (1+ (car move)) (1+ (cadr move))) (game-state-moves s) :test #'equal))) ;da li postoji potez gore-desno
			(setf change-next-player nil) ; postavlja change-next-player na '() jer je postignut poen
			(cons (list (list (1+ (car move)) (cadr move)) (if (= (game-state-next-player s) 1) 'x 'o)) (remove (list (list (1+ (car move)) (cadr move)) '()) (game-state-points s) :test #'equal))) ; ako je zatvoren kvadrat udesno, vraca se azurirana lista poena
		(t (game-state-points s)))) ; ako nije zatvoren kvadrat udesno, vraca se nepromenjena lista poena


; prima stanje i horizontan potez; proverava kvadrate iznad i ispod poteza i vraca azuriranu listu poena
(defun update-horizontal (s move)
	(cond	((= 1 (cadr move)) (update-up s move)) ; ako je horizontalan potez u prvoj vrsti, proverava se samo iznad da li je zatvoren kvadrat
			((= (1- (* 2 board-size)) (cadr move)) (update-down s move)) ; ako je u zadnjoj vrsti, proverava se samo ispod
			(t (update-up (make-game-state :moves (game-state-moves s) :points (update-down s move) :next-player (game-state-next-player s)) move)))) ; ako je unutrasnji, proverava se i iznad i ispod


; prima stanje i horizontan potez; proverava kvadrat iznad poteza i vraca azuriranu listu poena
(defun update-up (s move)
	(cond ((and	(cadr (assoc (list (car move) (+ (cadr move) 2)) (game-state-moves s) :test #'equal)) ; da li postoji potez iznad
				(cadr (assoc (list (1- (car move)) (1+ (cadr move)))  (game-state-moves s) :test #'equal)) ; da li postoji potez levo-gore
				(cadr (assoc (list (1+ (car move)) (1+ (cadr move)))  (game-state-moves s) :test #'equal))) ; da li postoji potez desno-gore
			(setf change-next-player nil) ; postavlja change-next-player na '() jer je postignut poen
			(cons (list (list (car move) (1+ (cadr move))) (if (= (game-state-next-player s) 1) 'x 'o)) (remove (list (list (car move) (1+ (cadr move))) '()) (game-state-points s) :test #'equal))) ; ako je zatvoren kvadrat iznad, vraca se azurirana lista poena
		(t (game-state-points s)))) ; ako nije zatvoren kvadrat iznad, vraca se nepromenjena lista poena


; prima stanje i horizontan potez; proverava kvadrat ispod poteza i vraca azuriranu listu poena
(defun update-down (s move)
	(cond ((and	(cadr (assoc (list (car move) (- (cadr move) 2)) (game-state-moves s) :test #'equal)) ; da li postoji potez ispod
				(cadr (assoc (list (1- (car move)) (1- (cadr move)))  (game-state-moves s) :test #'equal)) ;da li postoji potez levo-dole
				(cadr (assoc (list (1+ (car move)) (1- (cadr move)))  (game-state-moves s) :test #'equal))) ;da li postoji potez desno-dole
			(setf change-next-player nil) ; postavlja change-next-player na '() jer je postignut poen
			(cons (list (list (car move) (1- (cadr move))) (if (= (game-state-next-player s) 1) 'x 'o)) (remove (list (list (car move) (1- (cadr move))) '()) (game-state-points s) :test #'equal))) ; ako je zatvoren kvadrat ispod, vraca se azurirana lista poena
		(t (game-state-points s)))) ; ako nije zatvoren kvadrat ispod, vraca se nepromenjena lista poena


; prima novo stanje (strukturu game-state) i postavlja globalnu promenljivu main-state
(defun set-main-state (updated-state)
	(setf main-state updated-state))


; zove se kad se odigraju svi potezi; uporedjuje broj ikseva i okseva i stampa pobednika
(defun end-game ()
	(let ((x (xo-count 'x (game-state-points main-state))) (o (xo-count 'o (game-state-points main-state))))
		(cond	((= x o) (format t "~%~%The game is tied!"))
				((> x o) (format t "~%~%You won!"))
				(t (format t "~%~%You lost!")))))


; broji pojavljivanje prvog argumenta u drugom (sign je ili 'x ili 'o a xo-list je oblika '(((2 4) x) ((6 6) o) ( (4 2) x)...))
(defun xo-count (sign xo-list)
	(cond	((null xo-list) 0)
			((equal sign (cadar xo-list)) (+ 1 (xo-count sign (cdr xo-list))))
			(t (xo-count sign (cdr xo-list)))))


; pita korisnika da li zeli novu partiju i vraca t ili '()
(defun play-again ()
	(format t "~%~%Play Again? (type 'y' or 'n'): ")
	(let ((x (read)))
		(cond	((equal x 'y) t)
				((equal x 'n) '())
				(t (format t "~%~%Wrong input!") (play-again)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;====FUNKCIJE ZA VI====;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; proverava da li je stanje ciljno; prima stanje (strukturu game-state), proverava da li u listi poteza ima neodigranih poteza i ako nema vraca true, u suprotnom vraca nil
(defun end-state-p (s)
	(if (not (member '() (game-state-moves s) :test #'equal :key #'cadr)) t))


; odredjuje sve neodigrane poteze stanja; prima listu poteza u "punom" formatu (kao sto je game-state-moves) i vraca listu neodigranih poteza u formatu npr. ((1 2) (1 4) (4 5)...)
(defun get-remaining-moves (moves-list)
	(cond	((null moves-list) '())
			((cadar moves-list) (get-remaining-moves (cdr moves-list))) ; ako je potez odigran, tj ako mu je drugi element t, on se preskace
			(t (cons (caar moves-list) (get-remaining-moves (cdr moves-list)))))) ; ako potez nije odigran njegov prvi element, tj. njegovi indeksi, se dodaju u listu neodigranih


; prima pravu listu i vraca listu atoma; koristicemo je za povratnu vrednost f-je generate-successors
(defun flatten (l)
	(cond	((null l) '())
			((atom l) (list l))
			((atom (car l))
				(cons (car l) (flatten (cdr l))))
			(t (append	(flatten (car l))
						(flatten (cdr l))))))


; prima stanje (strukturu game-state) i listu dozvoljenih poteza za to stanje (u formatu koji vraca get-remaining-moves: potez je npr. (2 1)), i vraca listu svih potomaka tog stanja; !!! bitno: potomci stanja su sva stanja koja NISU zatvorila kvadrat PLUS svi potomci onih stanja koja JESU zatvorila kvadrat!!! povratna vrednost ce morati da se flatten-uje
(defun generate-successors (s rm)
	(cond ((null rm) s)	; ako stanje nema vise poteza koji se mogu odigrati, znaci da je ciljno i to stanje se vraca
			(t (mapcar #'(lambda (x) (let ((updated-s (update-state s x)))	; za svaki dozvoljeni potez kreira novo stanje
										(if change-next-player	; da li je novo stanje zatvorilo neki kvadrat
											updated-s	; ako nije, vraca se to novo stanje
											(generate-successors updated-s (remove x rm :test #'equal)))))	; ako jeste, traze se potomci tog novog stanja
					rm))))


;---------------------------------za testiranje generate-successors: stampa zadatu listu stanja rekurzivno-------------------------------------------------------------------
; (draw-states (flatten (generate-successors main-state (get-remaining-moves (game-state-moves main-state)))))	; poziv za proveru potomaka
(defun draw-states (states-list)
	(cond	((null states-list) '())
			(t	(format t "~%~%~%")
				(do ((i (1- (* 2 board-size)) (1- i))) ((< i 0) '()) ; i i j se vrte po 2*board-size puta (po 2*board-size-1 za sve pozicije na tabli i plus po jednom za oznake kolona i vrsta)
					(format t "~%")
					(if (= 0 i) (do ((j 0 (1+ j))) ((> j (1- (* 2 board-size))) '())	; stampa se vrsta sa oznakama kolona (slova)
									(if (= j 0) (format t "~2A " #\ ) (format t "~A " (cadr (assoc j inverse-translation)))))	; j se prevodi u slovo
								(do	((j 0 (1+ j))) ((> j (1- (* 2 board-size))) '())
									(cond	((= 0 j) (format t "~2A " i))	; stampa kolonu sa oznakama vrsta
											((and (oddp i) (oddp j)) (format t "* ")) ; ukoliko su oba indeksa neparna, u tom polju na tabli je tacka (*)
											((and (evenp i) (evenp j)) (let ((sign (cadr (assoc (list j i) (game-state-points (car states-list)) :test #'equal))))
																			(if (equal sign '()) (format t "  ") (format t "~A " sign))))	; ukoliko su oba indeksa parna, u tom polju na tabli su iks ili oks ako ih ima, tj. blanko znak ako ih nema
											((and (oddp i) (evenp j)) (if (cadr (assoc (list j i) (game-state-moves (car states-list)) :test #'equal)) (format t "- ") (format t "  ")))	; stampa horizontalni potez ako je odigran
											(t (if (cadr (assoc (list j i) (game-state-moves (car states-list)) :test #'equal)) (format t "| ") (format t "  ")))))))	; stampa vertikalni potez ako je odigran
				(format t "~%~%")
				(format t "~%Next to play is: ~A" (game-state-next-player (car states-list)))
				(draw-states (cdr states-list)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


; f-ja kojom computer izabere novi potez u formatu npr. (2 1); poziva min-max f-ju za tekuce stanje i sa parametrom dubine (za sada fiksno 3) i nalazi sledeci potez na osnovu stanja koje mu ona vrati
(defun enter-computer-move ()
	(format t "~%~%Computer thinking...~%~%")
	(let ((new-state (min-max-alfa-beta main-state 2)))
		(car (nth (- (length (get-remaining-moves (game-state-moves main-state))) (length (get-remaining-moves (game-state-moves new-state))) 1) (game-state-moves new-state)))))


; min-max f-ja sa alfa-beta odsecanjem; prima stanje table i dubinu trazenja, poziva f-ju max-value (computer je uvek max, a player je min) i vraca stanje u koje treba preci
(defun min-max-alfa-beta (state depth)
	(cadar (max-value state depth '(() -999) '(() 999)))) ; alfa i beta su u formatu ((put do stanja)  vrednost), polazna vrednost za alfa je -999, a za beta 999


; nalazi najbolju vrednost za igraca max; prima stanje, dubinu trazenja, alfa i beta , a vraca put do stanja koje ima najbolju vrednost i tu vrednost
(defun max-value (state depth alfa beta)
    (cond  ((or (end-state-p state) (zerop depth)) ; ispituje da li je stanje ciljno ili  da li je dostignuta zadata dubina trazenja 
            (list (list state) (eval-state state))) ; ako je ispunjen jedan uslov, vraca se to stanje i njegova vrednost u formatu ((state) value)
		   ; ako uslovi nisu ispunjeni, za potomke stanja state se iterativno poziva f-ja min-value
           (t  (let ((successors-list (flatten (generate-successors state (get-remaining-moves (game-state-moves state))))) ; lista svih potomaka stanja state
					 (alfa-state alfa)
					 (beta-state beta))
				(dolist (e successors-list (if (member (caar alfa-state) successors-list :test #'equal) ; na kraju iteracije se ispituje da li je prvo stanje u alfa potomak trenutnog stanja (state)
											   (list (cons state (car alfa-state)) (cadr alfa-state)) ; ako jeste trenutno stanje se dodaje putanji u alfa i to se vraca zajedno sa vrednoscu alfa
											   alfa-state)) ; ako nije vraca se alfa
                    (let ((successor (min-value e (1- depth) alfa-state beta-state))) ; dubina trazenja se dekrementuje za potomke
						(if (> (cadr successor) (cadr alfa-state)) (setf alfa-state successor))) ; svaki put kada min-value za nekog potomka vrati vrednost koja je veca od trenutne vrednosti alfa ta vrednost se upisuje u alfa, zajedno sa putanjom stanja do te vrednosti
                    (when (>= (cadr alfa-state) (cadr beta-state)) ; ako vrednost alfa postane veca ili jednaka vrednosti beta prekida se iteracija i vraca beta
						(return beta-state)))))))


; nalazi najbolju vrednost za igraca min; prima stanje, dubinu trazenja, alfa i beta, a vraca put do stanja koje ima najbolju vrednost i tu vrednost
(defun min-value (state depth alfa beta)
	(cond  ((or (end-state-p state) (zerop depth)) ; ispituje da li je stanje ciljno ili  da li je dostignuta zadata dubina trazenja 
            (list (list state) (eval-state state))) ; ako je ispunjen jedan uslov, vraca se to stanje i njegova vrednost u formatu ((state) value)
		   ; ako uslovi nisu ispunjeni, za potomke stanja state se iterativno poziva f-ja max-value
           (t  (let ((successors-list (flatten (generate-successors state (get-remaining-moves (game-state-moves state))))) ; lista svih potomaka stanja state
					 (alfa-state alfa)
					 (beta-state beta))
				(dolist (e successors-list (if (member (caar beta-state) successors-list :test #'equal) ; na kraju iteracije se ispituje da li je prvo stanje u beta potomak trenutnog stanja (state)
											   (list (cons state (car beta-state)) (cadr beta-state)) ; ako jeste trenutno stanje se dodaje putanji u beta i to se vraca zajedno sa vrednoscu beta
											   beta-state)) ; ako nije vraca se beta
					(let ((successor (max-value e (1- depth) alfa-state beta-state))) ; dubina trazenja se dekrementuje za potomke
						(if (< (cadr successor) (cadr beta-state)) (setf beta-state successor))) ; svaki put kada max-value za nekog potomka vrati vrednost koja je manja od trenutne vrednosti beta ta vrednost se upisuje u beta, zajedno sa putanjom stanja do te vrednosti
					(when (<= (cadr beta-state) (cadr alfa-state)) ; ako vrednost beta postane manja ili jednaka vrednosti alfa prekida se iteracija i vraca alfa
						(return alfa-state)))))))


;(setf facts nil)		; cinjenice ne unosimo "rucno" vec imamo f-ju koja ih generise na osnovu stanja table

; pravila unosimo "rucno"
(setf rules '((if (on ?i ?j o) then (score-o ?i ?j))
              (if (on ?i ?j x) then (score-x ?i ?j))))


; prevodi stanje table u listu cinjenica
(defun generate-facts (state)
	(make-facts (game-state-points state)))


; kreira listu cinjenica u formatu '((on 2 4 x) (on 4 4 o)...) na osnovu liste postignutih poena
(defun make-facts (points)
	(cond
		((null points) '())
		((eq (cadar points) '()) (make-facts (cdr points)))
		(t (cons (list 'on (caaar points) (cadaar points) (cadar points)) (make-facts (cdr points))))))


; vrsi procenu stanja kada se dostigne zadata dubina trazenja; racuna razliku izmedju postignutih 'o' i 'x' poena)
(defun eval-state (state)
	(let ((facts (generate-facts state)))
		 (prepare-knowledge rules facts 10)
		 (- (count-results '(score-o ?i ?j)) (count-results '(score-x ?i ?j)))))	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;                                                                                                ;;;;;
;;;;;                                       INFERENCE ENGINE                                         ;;;;;
;;;;;                                                                                                ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POMOCNE FUNKCIJE

;; provera da li je parametar s izvorna promenljiva (simbol koji pocinje sa ?)
(defun true-var? (s) 
  (if (symbolp s)
      (equal #\? (char (symbol-name s) 0))
    nil))

;; provera da li je parametar s promenljiva (simbol koji pocinje sa ? ili %)
(defun var? (s) 
  (if (symbolp s)
      (let ((c (char (symbol-name s) 0)))
        (or (equal c #\?) (equal c #\%)))
    nil))

;; provera da li je parametar s funkcija (simbol koji pocinje sa =)
(defun func? (s) 
  (if (symbolp s)
      (equal #\= (char (symbol-name s) 0))
    nil))

;; provera da li je parametar s predefinisani predikat (simbol koji pocinje sa !)
(defun predefined-predicate? (s)
  (if (symbolp s)
      (equal #\! (char (symbol-name s) 0))
    nil))

;; provera da li je parametar s konstanta (ako nije promenljiva ili funkcija onda je konstanta)
(defun const? (s)
  (not (or (var? s) (func? s))))

;; rekurzivna provera da li je parametar f funkcija od parametra x
(defun func-of (f x)
  (cond
   ((null f) ; kraj rekurzije
    t)
   ((atom f)
    (equal f x))
   (t
    (or (func-of (car f) x) (func-of (cdr f) x)))))

;; provera da li funkcija f ima promenljivih
(defun has-var (f)
  (cond
   ((null f) 
    nil)
   ((atom f)
    (var? f))
   (t
    (or (has-var (car f)) (has-var (cdr f))))))

;; funkcija koja vraca konsekvencu pravila
(defun rule-consequence (r)
  (car (last r)))

;; funkcija koja vraca premisu pravila
(defun rule-premises (r)
  (let ((p (cadr r)))
    (if (and (listp p) (equal (car p) 'and))
        (cdr p)
      (list p))))
      
;; funkcija koja vrsi prebacivanje upita u interni format (izbacuje 'and)
(defun format-query (q)
  (if (and (listp q) (equal (car q) 'and))
      (cdr q)
    (list q)))
    
;; izracunavanje istinitosne vrednosti predefinisanog predikata
(defun evaluate-predicate (p ls)
  (if (has-var p) nil  ; ako poseduje slobodne promenljive vraca nil (nije validna situacija)
    (if (eval p) 
        (list ls) ; ako predikat vazi vraca ulaznu listu smena
      nil))) ; u suprotnom vraca nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFEJSNE FUNKCIJE I GLOBALNE PROMENLJIVE

(defparameter *FACTS* nil)
(defparameter *RULES* nil)
(defparameter *MAXDEPTH* 10)

;; priprema *FACTS*, *RULES* i *MAXDEPTH*
(defun prepare-knowledge (lr lf maxdepth)
  (setq *FACTS* lf *RULES* (fix-rules lr) *MAXDEPTH* maxdepth))

;; vraca broj rezulata izvodjenja
(defun count-results (q)
  (length (infer- (format-query q) '(nil) 0)))

;; vraca listu lista smena
(defun infer (q)
  (filter-results (infer- (format-query q) '(nil) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNKCIJE KOJE VRSE DODELU NOVIH JEDINSTVENIH PROMENLJIVIH PRAVILIMA

(defun fix-rules (lr)
  (if (null lr) nil
    (cons (fix-rule (car lr)) (fix-rules (cdr lr)))))

(defun fix-rule (r)
  (let ((ls (make-rule-ls r nil)))
    (apply-ls r ls)))

(defun make-rule-ls (r ls)
  (cond
   ((null r)
    ls)
   ((var? r)
    (let ((a (assoc r ls)))
      (if (null a)
          (cons (list r (gensym "%")) ls)
        ls)))
   ((atom r)
    ls)   
   (t
    (make-rule-ls (cdr r) 
                  (make-rule-ls (car r) ls)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNKCIJE KOJE VRSE PRIPREMU REZULTATA (IZBACUJU SMENE KOJE SE ODNOSE NA INTERNE PROMENLJIVE)

(defun filter-results (lls)
  (if (null lls) nil
    (cons (filter-result (car lls)) (filter-results (cdr lls)))))

(defun filter-result (ls)
  (if (null ls) nil
    (if (true-var? (caar ls))
        (cons (car ls) (filter-result (cdr ls)))
      (filter-result (cdr ls)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNKCIJE KOJE SE KORISTE U IZVODJENJU

;; glavna funkcija za izvodjenje, vraca listu lista smena
;; lq - predikati upita
;; lls - lista listi smena (inicijalno lista koja sadrzi nil)
;; depth - tekuca dubina (inicijalno 0)
(defun infer- (lq lls depth)
  (if (null lq) lls
    (let ((lls-n (infer-q (car lq) lls depth)))
      (if (null lls-n) nil
        (infer- (cdr lq) lls-n depth)))))

;; izvodjenje za jedan predikat iz upita, vraca listu lista smena
(defun infer-q (q lls depth)
  (if (null lls) nil
    (let ((lls-n (infer-q-ls q (car lls) depth)))
      (if (null lls-n)
          (infer-q q (cdr lls) depth)
        (append lls-n (infer-q q (cdr lls) depth))))))

;; izvodjenje za jedan predikat sa jednom listom smena, vraca listu lista smena
(defun infer-q-ls (q ls depth)
  (if (predefined-predicate? (car q))
      (evaluate-predicate (apply-ls q ls) ls)
    (if (< depth *MAXDEPTH*)
        (append (infer-q-ls-lf q *FACTS* ls) (infer-q-ls-lr q *RULES* ls depth))
      (infer-q-ls-lf q *FACTS* ls))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; izvodjenje nad bazom cinjenica lf, vraca listu lista smena
(defun infer-q-ls-lf (q lf ls)
  (if (null lf) nil
    (let ((ls-n (infer-q-ls-f q (car lf) ls)))
      (if (null ls-n)
          (infer-q-ls-lf q (cdr lf) ls)
        (if (null (car ls-n)) ls-n
          (append ls-n (infer-q-ls-lf q (cdr lf) ls)))))))

;; izvodjenje sa jednom cinjenicom, vraca listu sa listom smena
(defun infer-q-ls-f (q f ls)
  (if (= (length q) (length f)) ; provera na istu duzinu
      (infer-q-ls-f- q f ls)
    nil))

;; izvodjenje sa jednom cinjenicom, vraca listu sa listom smena
(defun infer-q-ls-f- (q f ls)
  (if (null q) (list ls)
    (let ((nq (apply-and-eval (car q) ls)) (nf (car f)))
      (if (var? nq) 
          (infer-q-ls-f- (cdr q) (cdr f) (append ls (list (list nq nf))))
        (if (equal nq nf) 
            (infer-q-ls-f- (cdr q) (cdr f) ls)
          nil)))))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; izvodjenje nad bazom pravila, vraca listu lista smena
(defun infer-q-ls-lr (q lr ls depth)
  (if (null lr) nil
    (let ((ls-n (infer-q-ls-r q (car lr) ls depth)))
      (if (null ls-n)
          (infer-q-ls-lr q (cdr lr) ls depth)
        (if (null (car ls-n)) ls-n
          (append ls-n (infer-q-ls-lr q (cdr lr) ls depth)))))))

;; izvodjenje sa jednim pravilom, vraca listu sa listom smena
(defun infer-q-ls-r (q r ls depth)
  (let ((c (rule-consequence r)))
    (if (= (length q) (length c))
        (let ((lsc (unify q c nil ls)))
          (if (null lsc) nil
            (infer- (apply-ls (rule-premises r) (car lsc)) (cdr lsc) (1+ depth))))
      nil)))

;; unifikacija predikata upita q i konsekvence pravila c primenom liste smena ls, vraca listu smena
(defun unify (q c uls ls)
  (if (or (null q) (null c))
      (if (and (null q) (null c)) (list uls ls) nil)
    (let ((eq (car q)) (ec (car c)))
      (cond
       ((equal eq ec)
        (unify (cdr q) (cdr c) uls ls))
       ((var? eq)
        (cond
         ((var? ec)
          (let ((a (assoc ec uls)))
            (cond
             ((null a)              
              (unify (cdr q) (cdr c) (cons (list ec eq) uls) ls))
             ((equal (cadr a) eq)
              (unify (cdr q) (cdr c) uls ls))
             (t
              nil))))
         ((func? ec)
          nil)
         (t ;; const
          (let ((a (assoc eq ls)))
            (cond
             ((null a)
              (unify (cdr q) (cdr c) uls (cons (list eq ec) ls)))
             ((equal (cadr a) ec)
              (unify (cdr q) (cdr c) uls ls))
             (t 
              nil))))))
       ((func? eq)
        (cond
         ((var? ec)
          (if (func-of eq ec) nil
            (let ((a (assoc ec uls)))
              (cond
               ((null a)              
                (unify (cdr q) (cdr c) (cons (list ec eq) uls) ls))
               ((equal (cadr a) eq)
                (unify (cdr q) (cdr c) uls ls))
               (t
                nil)))))
         ((func? ec)
          nil)
         (t ;; const
          (let ((f (apply-ls eq ls)))
            (if (has-var f) nil
              (if (equal (eval f) ec)
                  (unify (cdr q) (cdr c) uls ls)
                nil))))))
       (t ;; const
        (cond
         ((var? ec)
          (let ((a (assoc ec uls)))
            (cond
             ((null a)              
              (unify (cdr q) (cdr c) (cons (list ec eq) uls) ls))
             ((equal (cadr a) eq)
              (unify (cdr q) (cdr c) uls ls))
             (t
              nil))))
         (t ;; func or const
          nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRIMENA LISTE SMENA I IZRACUNAVANJE IZRAZA

(defun apply-and-eval (x ls)
  (if (var? x)
      (apply-ls x ls)
    (if (and (listp x) (func? (car x)))
        (eval (apply-ls x ls)) 
      x)))

;; primena liste smena ls na izraz x
(defun apply-ls (x ls)
  (cond
   ((null x)
    x)
   ((var? x)
    (let ((ax (assoc x ls)))
      (if (null ax) x
        (cadr ax))))
   ((atom x)
    x)
   (t
    (cons (apply-ls (car x) ls) (apply-ls (cdr x) ls)))))