(declare-const x Int)
(declare-const y Int)
(declare-const v Int)

(assert (not (>= v 0)))
(assert (> x y))
(assert (= v x))

(check-sat)
(get-model)

