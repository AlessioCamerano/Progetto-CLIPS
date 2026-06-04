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

(deffunction choose-backtrack-step (?cx ?cy ?lx ?ly)
   (bind ?best-risk 1000000.0)
   (bind ?best-dist 999999)
   (bind ?target-x -1) (bind ?target-y -1)
   (do-for-all-facts ((?t cell))
      (member$ ?t:status (create$ walked home))
      (bind ?has-unknown FALSE)
      (bind ?minr 1000000.0)
      (do-for-all-facts ((?u cell))
         (and (eq ?u:status unknown)
              (is-adjacent-4 ?t:x ?t:y ?u:x ?u:y))
         (bind ?has-unknown TRUE)
         (if (< ?u:risk ?minr) then (bind ?minr ?u:risk)))
      (if (eq ?has-unknown TRUE) then
         (bind ?dist (manhattan ?t:x ?t:y ?cx ?cy))
         (if (or (< ?minr ?best-risk) (and (= ?minr ?best-risk) (< ?dist ?best-dist))) then
            (bind ?best-risk ?minr)
            (bind ?best-dist ?dist)
            (bind ?target-x ?t:x)
            (bind ?target-y ?t:y))))
   (if (and (neq ?target-x -1) (neq ?target-y -1)) then
      (bind ?curd (manhattan ?cx ?cy ?target-x ?target-y))
      (do-for-all-facts ((?n cell))
         (and (is-adjacent-4 ?cx ?cy ?n:x ?n:y)
              (member$ ?n:status (create$ walked home)))
         (bind ?nd (manhattan ?n:x ?n:y ?target-x ?target-y))
         (if (< ?nd ?curd) then (return (get-direction ?cx ?cy ?n:x ?n:y)))) )
   (bind ?best-score 999999)
   (bind ?best-move none)
   (do-for-all-facts ((?c cell))
      (and (is-adjacent-4 ?cx ?cy ?c:x ?c:y)
           (member$ ?c:status (create$ walked home))
           (or (neq ?c:x ?lx) (neq ?c:y ?ly)))
      (bind ?score (manhattan ?c:x ?c:y ?lx ?ly))
      (if (< ?score ?best-score) then
         (bind ?best-score ?score)
         (bind ?best-move (get-direction ?cx ?cy ?c:x ?c:y))))
   (if (neq ?best-move none) then (return ?best-move))
   (if (and (neq ?lx -1) (neq ?ly -1) (is-adjacent-4 ?cx ?cy ?lx ?ly)) then
      (return (get-direction ?cx ?cy ?lx ?ly)))
   (return none))

(deffunction choose-random-lowest-risk-adjacent (?cx ?cy ?lx ?ly)
   (bind ?best-risk 1000000.0)
   (bind ?choices (create$))
   (bind ?global-min-risk 1000000.0)
   (do-for-all-facts ((?all-c cell)) (eq ?all-c:status unknown)
      (if (< ?all-c:risk ?global-min-risk) then
         (bind ?global-min-risk ?all-c:risk)))
   (do-for-all-facts ((?c cell))
      (and (eq ?c:status unknown)
           (is-adjacent-4 ?cx ?cy ?c:x ?c:y)
           (or (neq ?c:x ?lx) (neq ?c:y ?ly))
           (= ?c:risk ?global-min-risk))
      (bind ?choices (create$ ?choices ?c:x ?c:y ?c:risk)))
   (if (= (length$ ?choices) 0) then
      (do-for-all-facts ((?c cell))
         (and (eq ?c:status unknown)
              (is-adjacent-4 ?cx ?cy ?c:x ?c:y)
              (= ?c:risk ?global-min-risk))
         (bind ?choices (create$ ?choices ?c:x ?c:y ?c:risk))))
   (if (> (length$ ?choices) 0) then
      (bind ?candidate-count (div (length$ ?choices) 3))
      (bind ?selected-index (random 1 ?candidate-count))
      (bind ?base (* (- ?selected-index 1) 3))
      (return (create$ (nth$ (+ ?base 1) ?choices)
                       (nth$ (+ ?base 2) ?choices)
                       (nth$ (+ ?base 3) ?choices))))
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
   (printout t "[STRATEGY] Obiettivo: " ?k " | Pos: [" ?cx "," ?cy "]" crlf))

(defrule grab-gold
   (declare (salience 200))
   (process-turn)
   (percept (glitter TRUE))
   (objective (kind gold))
   (not (action))
   =>
   (printout t "[STRATEGY] Oro trovato! GRAB." crlf)
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
      (printout t "[STRATEGY] Segnalo rischio con: " ?probe crlf)
      (assert (action (move ?probe)))))

(defrule move-forward-to-global-best
   (declare (salience 110))
   (process-turn)
   (objective (kind gold))
   (not (action))
   (agent-state (current-x ?cx) (current-y ?cy) (last-x ?lx) (last-y ?ly))
   =>
   (bind ?choice (choose-random-lowest-risk-adjacent ?cx ?cy ?lx ?ly))
   (if (> (length$ ?choice) 0) then
      (bind ?nx (nth$ 1 ?choice))
      (bind ?ny (nth$ 2 ?choice))
      (bind ?risk (nth$ 3 ?choice))
      (if (= ?risk 0.0) then
         (printout t "[STRATEGY] Esploro cella ignota sicura [" ?nx "," ?ny "]" crlf)
      else
         (printout t "[STRATEGY] ! AZIONE RISCHIOSA OTTIMIZZATA ! Tento ingresso nel minimo rischio globale " ?risk " in [" ?nx "," ?ny "]" crlf))
      (assert (action (move (get-direction ?cx ?cy ?nx ?ny))))))

(defrule backtrack-to-global-best
   (declare (salience 100))
   (process-turn)
   (objective (kind gold))
   (not (action))
   (agent-state (current-x ?cx) (current-y ?cy) (last-x ?lx) (last-y ?ly))
   =>
   (bind ?move (choose-backtrack-step ?cx ?cy ?lx ?ly))
   (if (neq ?move none) then
      (printout t "[STRATEGY] Navigazione strategica: backtracking verso la zona a rischio minore: " ?move crlf)
      (assert (action (move ?move)))))

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
   (printout t "[STRATEGY] QUIT raggiunto." crlf)
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
   (printout t "[STRATEGY] Mossa: " ?m crlf))

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

