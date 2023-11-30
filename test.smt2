(declare-sort Loc 0)      ;; Declaration of location sort. Currentlnullptr fixed to this form by Astral.
(declare-heap (Loc Loc))  ;; Declaration of heap sort. Currentlnullptr fixed to this form by Astral.

;; Declaration of location variables
(declare-const x Loc)
(declare-const x_target Loc)
(declare-const nullptr Loc)

;; Input formula
(assert
  (sep
    emp
    (pto x x_target)
    (= nullptr nil)
    (distinct x nullptr)
  )
)

(check-sat)
