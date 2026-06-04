(defmodule MAIN
   (export ?ALL))

;;; -------------------- TEMPLATE DI BASE --------------------

(deftemplate counter (slot step (type INTEGER) (default 0)))

(deftemplate percept
   (slot x (type INTEGER)) (slot y (type INTEGER)) (slot steps (type INTEGER))
   (slot breeze (type SYMBOL) (allowed-symbols TRUE FALSE))
   (slot stench (type SYMBOL) (allowed-symbols TRUE FALSE))
   (slot glitter (type SYMBOL) (allowed-symbols TRUE FALSE))
   (slot energy (type INTEGER)))

(deftemplate action
   (slot move (type SYMBOL) (allowed-symbols up down left right grab quit pit-north pit-south pit-west pit-east none)))

(deftemplate agent-state
   (slot current-x (type INTEGER) (default 0))
   (slot current-y (type INTEGER) (default 0))
   (slot last-x (type INTEGER) (default -1))
   (slot last-y (type INTEGER) (default -1)))

(deftemplate last-action
   (slot move (type SYMBOL) (default none)))

(deftemplate cell
   (slot x (type INTEGER))
   (slot y (type INTEGER))
   (slot status (type SYMBOL) (allowed-symbols path walked wall unknown pit wumpus gold home) (default unknown))
   (slot risk (type FLOAT) (default 0.0))
   (slot breeze-processed (type SYMBOL) (allowed-symbols TRUE FALSE) (default FALSE))
   (slot stench-processed (type SYMBOL) (allowed-symbols TRUE FALSE) (default FALSE)))

(deftemplate objective (slot kind (type SYMBOL) (allowed-symbols gold home) (default gold)))

(deftemplate movement-success (slot step (type INTEGER)))

;;; -------------------- FUNZIONI GEOMETRICHE E LOGICHE GLOBALI --------------------

(deffunction get-direction (?cx ?cy ?nx ?ny)
   (if (and (= ?cx ?nx) (= ?ny (+ ?cy 1))) then (return down))
   (if (and (= ?cx ?nx) (= ?ny (- ?cy 1))) then (return up))
   (if (and (= ?cy ?ny) (= ?nx (+ ?cx 1))) then (return right))
   (if (and (= ?cy ?ny) (= ?nx (- ?cx 1))) then (return left))
   (return none))

(deffunction is-adjacent-4 (?cx ?cy ?x ?y)
   (return
      (or (and (= ?x ?cx) (or (= ?y (+ ?cy 1)) (= ?y (- ?cy 1))))
          (and (= ?y ?cy) (or (= ?x (+ ?cx 1)) (= ?x (- ?cx 1)))))))

(deffunction manhattan (?x ?y ?bx ?by)
   (return (+ (abs (- ?x ?bx)) (abs (- ?y ?by)))))

(deffunction boost-random-adjacent-risk (?cx ?cy ?delta ?label)
   (bind ?updated-cells-count 0)
   (do-for-all-facts ((?c cell))
      (and (eq ?c:status unknown)
           (or (and (= ?c:x (+ ?cx 1)) (= ?c:y ?cy))
               (and (= ?c:x (- ?cx 1)) (= ?c:y ?cy))
               (and (= ?c:x ?cx) (= ?c:y (+ ?cy 1)))
               (and (= ?c:x ?cx) (= ?c:y (- ?cy 1)))))
      (bind ?new-risk (min 1.0 (+ (fact-slot-value ?c risk) ?delta)))
      (modify ?c (risk ?new-risk))
      (bind ?updated-cells-count (+ ?updated-cells-count 1))
      (printout t "[REASONING] " ?label " -> Distribuito rischio su [" ?c:x "," ?c:y "]. Nuovo rischio: " ?new-risk crlf))
   (do-for-all-facts ((?p percept)) TRUE
      (bind ?current-energy (fact-slot-value ?p energy))
      (modify ?p (energy (- ?current-energy ?updated-cells-count)))
      (printout t "[ENERGY] Consumati " ?updated-cells-count " step energetici per l'analisi dei rischi. Energia residua: " (- ?current-energy ?updated-cells-count) crlf)))

(deffunction clear-adjacent-risk (?cx ?cy)
   (do-for-all-facts ((?c cell))
      (and (eq ?c:status unknown)
           (or (and (= ?c:x (+ ?cx 1)) (= ?c:y ?cy))
               (and (= ?c:x (- ?cx 1)) (= ?c:y ?cy))
               (and (= ?c:x ?cx) (= ?c:y (+ ?cy 1)))
               (and (= ?c:x ?cx) (= ?c:y (- ?cy 1)))))
      (if (neq ?c:risk 0.0) then
         (modify ?c (risk 0.0))
         (printout t "[REASONING] Cella [" ?c:x "," ?c:y "] SICURA" crlf))))

;;; -------------------- SINCRONIZZAZIONE TURNI --------------------

(deftemplate process-turn)

(deftemplate turn-sync
   (slot percept-step (type INTEGER) (default -1))
   (slot action-step (type INTEGER) (default -1)))

;;; -------------------- STATO INIZIALE --------------------

(deffacts initial-state
   (counter (step 0))
   (cell (x 0) (y 0) (status home))
   (objective (kind gold))
   (agent-state (current-x 0) (current-y 0) (last-x -1) (last-y -1))
   (last-action (move none))
   (turn-sync (percept-step -1) (action-step -1))
   (boot))

;;; -------------------- CONTROL LOOP & SYNCHRONIZATION --------------------

(defrule bootstrap-modules
   (declare (salience 1000))
   ?b <- (boot)
   =>
   (retract ?b)
   (if (not (member$ MAPPING (get-defmodule-list))) then
      (load "agentA/mapping.clp")
      (load "agentA/reasoning.clp")
      (load "agentA/strategy.clp")
   else
      (printout t "[MAIN] Moduli già presenti in memoria. Avvio nuova sessione..." crlf)))

(defrule latch-latest-percept-step
   (declare (salience 110))
   (counter (step ?s))
   (percept (steps ?s))
   ?sync <- (turn-sync (percept-step ?ps&:(< ?ps ?s)))
   =>
   (modify ?sync (percept-step ?s)))

(defrule generate-turn-token
   (declare (salience 100))
   (counter (step ?s))
   (percept (steps ?s))
   (turn-sync (percept-step ?s) (action-step ?as&:(< ?as ?s)))
   (not (process-turn))
   (not (boot))
   =>
   (assert (process-turn))
   (printout t crlf "[MAIN] >>> STEP: " ?s " <<<" crlf))

(defrule cleanup-old-movement-success
   (declare (salience 91))
   (process-turn)
   (counter (step ?current-step))
   ?old <- (movement-success (step ?s&:(< ?s ?current-step)))
   =>
   (retract ?old))

(defrule engine-cycle
   (declare (salience 50))
   (process-turn)
   =>
   (focus MAPPING REASONING STRATEGY))

(defrule cleanup-old-percepts
   (declare (salience 90))
   (process-turn)
   (counter (step ?current-step))
   ?old-percept <- (percept (steps ?s&:(< ?s ?current-step)))
   =>
   (retract ?old-percept))

(defrule end-turn-cleanup
   (declare (salience 10))
   ?f <- (process-turn)
   =>
   (retract ?f)
   (printout t "[MAIN] Turno elaborato. Attesa simulatore..." crlf))
