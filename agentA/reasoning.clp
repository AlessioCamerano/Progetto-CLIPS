(defmodule REASONING 
   (import MAIN ?ALL) 
   (export ?ALL))

;;; -------------------- REGOLE DI RAGIONAMENTO E GESTIONE STATO MENTALE --------------------

(defrule check-energy-limit
   (declare (salience 205))
   (process-turn)
   ?obj <- (objective (kind gold))
   (percept (energy ?e))
   (agent-state (current-x ?cx) (current-y ?cy))
   (cell (status home) (x ?bx) (y ?by))
   =>
   (bind ?dist (manhattan ?cx ?cy ?bx ?by))
   (if (<= ?e (+ ?dist 5)) then
      (printout t "[REASONING] !!! ENERGIA AL LIMITE !!! Rientro forzato." crlf)
      (modify ?obj (kind home))))

(defrule switch-to-home
   (declare (salience 195))
   (process-turn)
   (last-action (move grab))
   ?obj <- (objective (kind gold))
   =>
   (printout t "[REASONING] Oro raccolto correttamente. Cambio obiettivo in: home" crlf)
   (modify ?obj (kind home)))

(defrule deduce-safe
   (declare (salience 100))
   (process-turn)
   (percept (x ?cx) (y ?cy) (breeze FALSE) (stench FALSE))
   =>
   (clear-adjacent-risk ?cx ?cy))

(defrule reinforce-breeze-risk
   (declare (salience 90))
   (process-turn)
   (percept (x ?cx) (y ?cy) (breeze TRUE))
   ?src <- (cell (x ?cx) (y ?cy) (breeze-processed FALSE))
   =>
   (boost-random-adjacent-risk ?cx ?cy 0.2 BREEZE)
   (modify ?src (breeze-processed TRUE)))

(defrule reinforce-stench-risk
   (declare (salience 90))
   (process-turn)
   (percept (x ?cx) (y ?cy) (stench TRUE))
   ?src <- (cell (x ?cx) (y ?cy) (stench-processed FALSE))
   =>
   (boost-random-adjacent-risk ?cx ?cy 0.2 STENCH)
   (modify ?src (stench-processed TRUE)))
