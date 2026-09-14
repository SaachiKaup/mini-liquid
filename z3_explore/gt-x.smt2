(declare-const x Int)
(declare-const y Int)
(declare-const v Int)

(assert (>= v x))
(assert (> x y))
(assert (= v x))

(check-sat)
(get-model)

