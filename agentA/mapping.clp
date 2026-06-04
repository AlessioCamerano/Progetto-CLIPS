(defmodule MAPPING
   (import MAIN ?ALL)
   (export ?ALL))

;;; -------------------- REGOLE DI PERCEZIONE E AGGIORNAMENTO GRIGLIA --------------------

(defrule update-state-on-success
   (declare (salience 100))
   (process-turn)
   (counter (step ?s))
   (percept (x ?nx) (y ?ny) (steps ?s))
   ?state <- (agent-state (current-x ?ox) (current-y ?oy))
   (test (or (neq ?nx ?ox) (neq ?ny ?oy)))
   =>
   (printout t "[PERCEIVE] Spostamento riuscito in [" ?nx "," ?ny "]" crlf)
   (assert (movement-success (step ?s)))
   (modify ?state (current-x ?nx) (current-y ?ny) (last-x ?ox) (last-y ?oy))
   (do-for-all-facts ((?from cell)) (and (= ?from:x ?ox) (= ?from:y ?oy))
      (if (and (neq ?from:status home) (or (neq ?from:status walked) (neq ?from:risk 0.0))) then
         (modify ?from (status walked) (risk 0.0) 
                       (breeze-processed ?from:breeze-processed) 
                       (stench-processed ?from:stench-processed))))
   (do-for-all-facts ((?c cell)) (and (= ?c:x ?nx) (= ?c:y ?ny))
      (if (and (neq ?c:status home) (or (neq ?c:status walked) (neq ?c:risk 0.0))) then
         (modify ?c (status walked) (risk 0.0)
                    (breeze-processed ?c:breeze-processed)
                    (stench-processed ?c:stench-processed)))))

(defrule detect-wall-collision
   (declare (salience 101))
   (process-turn)
   (counter (step ?s))
   (percept (x ?px) (y ?py) (steps ?s))
   (agent-state (current-x ?px) (current-y ?py))
   (last-action (move ?m&up|down|left|right))
   (not (movement-success (step ?s)))
   =>
   (bind ?wx ?px) (bind ?wy ?py)
   (if (eq ?m right) then (bind ?wx (+ ?px 1)))
   (if (eq ?m left)  then (bind ?wx (- ?px 1)))
   (if (eq ?m up)    then (bind ?wy (- ?py 1)))
   (if (eq ?m down)  then (bind ?wy (+ ?py 1)))
   (do-for-all-facts ((?c cell)) (and (= ?c:x ?wx) (= ?c:y ?wy))
      (if (eq ?c:status unknown) then
         (printout t "[PERCEIVE] CORREZIONE: Cella [" ?wx "," ?wy "] è un MURO reale." crlf)
         (modify ?c (status wall) (risk 0.0)))))

(defrule expand-map
   (declare (salience 100))
   (process-turn)
   (agent-state (current-x ?cx) (current-y ?cy))
   =>
   (if (not (any-factp ((?c cell)) (and (= ?c:x (+ ?cx 1)) (= ?c:y ?cy)))) then 
       (assert (cell (x (+ ?cx 1)) (y ?cy) (status unknown))))
   (if (>= (- ?cx 1) 0) then 
       (if (not (any-factp ((?c cell)) (and (= ?c:x (- ?cx 1)) (= ?c:y ?cy)))) then 
           (assert (cell (x (- ?cx 1)) (y ?cy) (status unknown)))))
   (if (not (any-factp ((?c cell)) (and (= ?c:x ?cx) (= ?c:y (+ ?cy 1))))) then 
       (assert (cell (x ?cx) (y (+ ?cy 1)) (status unknown))))
   (if (>= (- ?cy 1) 0) then 
       (if (not (any-factp ((?c cell)) (and (= ?c:x ?cx) (= ?c:y (- ?cy 1))))) then 
           (assert (cell (x ?cx) (y (- ?cy 1)) (status unknown))))))
