(defmodule STRATEGY
   (import MAIN ?ALL)
   (export ?ALL))

;;; --- FUNZIONI DI SUPPORTO ALLA NAVIGAZIONE ---

(deffunction choose-home-action (?cx ?cy ?lx ?ly ?bx ?by)
   (bind ?best-dist 999999)
   (bind ?best-move none)
   (do-for-all-facts ((?c cell)) TRUE
      (if (and (member$ ?c:status (create$ walked home))
               (is-adjacent-4 ?cx ?cy ?c:x ?c:y)
               (neq ?c:status wall)) then
         (bind ?dist (manhattan ?c:x ?c:y ?bx ?by))
         (if (and (or (neq ?c:x ?lx) (neq ?c:y ?ly)) (< ?dist ?best-dist)) then
            (bind ?best-dist ?dist)
            (bind ?best-move (get-direction ?cx ?cy ?c:x ?c:y)))))
   (if (neq ?best-move none) then (return ?best-move))
   (return (get-direction ?cx ?cy ?lx ?ly)))

(deffunction choose-stupid-backtrack (?cx ?cy ?lx ?ly)
   ;; L'Agente B è privo di memoria globale. Non sa pianificare un percorso.
   ;; Se la cella da cui proviene (last) è adiacente, ci torna dentro ciecamente.
   (if (and (neq ?lx -1) (neq ?ly -1) (is-adjacent-4 ?cx ?cy ?lx ?ly)) then
      (return (get-direction ?cx ?cy ?lx ?ly)))
   
   ;; Altrimenti, se non può tornare nel last, prende il primo vicino calpestato che trova
   (do-for-all-facts ((?c cell))
      (and (is-adjacent-4 ?cx ?cy ?c:x ?c:y)
           (member$ ?c:status (create$ walked home)))
      (return (get-direction ?cx ?cy ?c:x ?c:y)))
   (return none))

(deffunction choose-local-random-adjacent (?cx ?cy ?lx ?ly ?type)
   (bind ?choices (create$))
   (do-for-all-facts ((?c cell))
      (and (eq ?c:status unknown)
           (is-adjacent-4 ?cx ?cy ?c:x ?c:y)
           (or (neq ?c:x ?lx) (neq ?c:y ?ly))
           (if (eq ?type safe) then (= ?c:risk 0.0) else (> ?c:risk 0.0)))
      (bind ?choices (create$ ?choices ?c:x ?c:y)))
   (if (= (length$ ?choices) 0) then
      (do-for-all-facts ((?c cell))
         (and (eq ?c:status unknown)
              (is-adjacent-4 ?cx ?cy ?c:x ?c:y)
              (if (eq ?type safe) then (= ?c:risk 0.0) else (> ?c:risk 0.0)))
         (bind ?choices (create$ ?choices ?c:x ?c:y))))
   (if (> (length$ ?choices) 0) then
      (bind ?candidate-count (div (length$ ?choices) 2))
      (bind ?selected-index (random 1 ?candidate-count))
      (bind ?base (* (- ?selected-index 1) 2))
      (return (create$ (nth$ (+ ?base 1) ?choices)
                       (nth$ (+ ?base 2) ?choices))))
   (return (create$)))

(deffunction choose-hazard-action (?cx ?cy)
   (if (and (>= ?cy 1)
            (any-factp ((?c cell))
               (and (= ?c:x ?cx) (= ?c:y (- ?cy 1)) (eq ?c:status unknown)))) then
      (return pit-north))
   (if (any-factp ((?c cell))
            (and (= ?c:x ?cx) (= ?c:y (+ ?cy 1)) (eq ?c:status unknown))) then
      (return pit-south))
   (if (and (>= ?cx 1)
            (any-factp ((?c cell))
               (and (= ?c:x (- ?cx 1)) (= ?c:y ?cy) (eq ?c:status unknown)))) then
      (return pit-west))
   (if (any-factp ((?c cell))
            (and (= ?c:x (+ ?cx 1)) (= ?c:y ?cy) (eq ?c:status unknown))) then
      (return pit-east))
   (return none))

   ;;; --- REGOLE PIANIFICAZIONE AZIONE ---

(defrule notify-objective
   (declare (salience 210))
   (process-turn)
   (objective (kind ?k))
   (agent-state (current-x ?cx) (current-y ?cy))
   =>
   (printout t "[STRATEGY-B] Obiettivo: " ?k " | Pos: [" ?cx "," ?cy "]" crlf))

(defrule grab-gold
   (declare (salience 200))
   (process-turn)
   (percept (glitter TRUE))
   (objective (kind gold))
   (not (action))
   =>
   (printout t "[STRATEGY-B] Oro trovato! GRAB." crlf)
   (assert (action (move grab))))

(defrule signal-hazard-step
   (declare (salience 195))
   (process-turn)
   (objective (kind gold))
   (not (action))
   (percept (x ?cx) (y ?cy) (breeze TRUE))
   (last-action (move ?last))
   (test (eq (str-index "pit" (str-cat ?last)) FALSE))
   =>
   (bind ?probe (choose-hazard-action ?cx ?cy))
   (if (neq ?probe none) then
      (printout t "[STRATEGY-B] Segnalo rischio locale con: " ?probe crlf)
      (assert (action (move ?probe)))))

(defrule move-forward-local-safe
   (declare (salience 110))
   (process-turn)
   (objective (kind gold))
   (not (action))
   (agent-state (current-x ?cx) (current-y ?cy) (last-x ?lx) (last-y ?ly))
   =>
   (bind ?choice (choose-local-random-adjacent ?cx ?cy ?lx ?ly safe))
   (if (> (length$ ?choice) 0) then
      (bind ?nx (nth$ 1 ?choice))
      (bind ?ny (nth$ 2 ?choice))
      (printout t "[STRATEGY-B] Esploro cella adiacente sicura [" ?nx "," ?ny "]" crlf)
      (assert (action (move (get-direction ?cx ?cy ?nx ?ny))))))

(defrule move-stupid-backtrack
   (declare (salience 100))
   (process-turn)
   (objective (kind gold))
   (not (action))
   (agent-state (current-x ?cx) (current-y ?cy) (last-x ?lx) (last-y ?ly))
   =>
   ;; Applica il backtracking miope: l'agente fa un passo indietro locale
   (bind ?move (choose-stupid-backtrack ?cx ?cy ?lx ?ly))
   (if (neq ?move none) then
      (printout t "[STRATEGY-B] Pericolo: arretro ciecamente sulla cella precedente: " ?move crlf)
      (assert (action (move ?move)))))

(defrule move-forward-local-hazard
   (declare (salience 90))
   (process-turn)
   (objective (kind gold))
   (not (action))
   (agent-state (current-x ?cx) (current-y ?cy) (last-x ?lx) (last-y ?ly))
   =>
   (bind ?choice (choose-local-random-adjacent ?cx ?cy ?lx ?ly risky))
   (if (> (length$ ?choice) 0) then
      (bind ?nx (nth$ 1 ?choice))
      (bind ?ny (nth$ 2 ?choice))
      (printout t "[STRATEGY-B] ! AZZARDO LOCALE ! Tento ingresso casuale in [" ?nx "," ?ny "]" crlf)
      (assert (action (move (get-direction ?cx ?cy ?nx ?ny))))))

(defrule home-greedy-return
   (declare (salience 160))
   (process-turn)
   (objective (kind home))
   (cell (status home) (x ?bx) (y ?by))
   (agent-state (current-x ?cx) (current-y ?cy) (last-x ?lx) (last-y ?ly))
   (not (action))
   =>
   (bind ?m (choose-home-action ?cx ?cy ?lx ?ly ?bx ?by))
   (if (neq ?m none) then 
      (assert (action (move ?m)))
   else
      (assert (action (move (get-direction ?cx ?cy ?lx ?ly))))))

(defrule safe-exit-home
   (declare (salience 200))
   (process-turn)
   (objective (kind home))
   (cell (status home) (x ?bx) (y ?by))
   (agent-state (current-x ?bx) (current-y ?by))
   (not (action))
   =>
   (printout t "[STRATEGY-B] QUIT raggiunto." crlf)
   (assert (action (move quit))))

(defrule finalize-action
   (declare (salience -100))
   (process-turn)
   (counter (step ?s))
   ?sync <- (turn-sync (action-step ?as&:(< ?as ?s)))
   (action (move ?m))
   =>
   (do-for-all-facts ((?old last-action)) TRUE (retract ?old))
   (assert (last-action (move ?m)))
   (modify ?sync (action-step ?s))
   (printout t "[STRATEGY-B] Mossa: " ?m crlf))

(defrule finalize-no-action
   (declare (salience -100))
   (process-turn)
   (counter (step ?s))
   ?sync <- (turn-sync (action-step ?as&:(< ?as ?s)))
   (not (action))
   =>
   (do-for-all-facts ((?old last-action)) TRUE (retract ?old))
   (assert (last-action (move none)))
   (modify ?sync (action-step ?s)))

