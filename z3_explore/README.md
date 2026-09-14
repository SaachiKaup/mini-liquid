# Z3 Solver Results

## v > 0

Run the following -

```shell
z3 not-positive.smt2
```

Result -
```text
sat
(
  (define-fun y () Int
    (- 2))
  (define-fun x () Int
    (- 1))
  (define-fun v () Int
    (- 1))
)
```

## v >= x

Run the following -

```shell
z3 gt-x.smt2 
```

Result -
```text
sat
(
  (define-fun x () Int
    1)
  (define-fun y () Int
    0)
  (define-fun v () Int
    1)
)
```

## v >= y

Run the following -

```shell
z3 lt-y.smt2 
```

Note - In this program, the reverse is asserted i.e
```
(not (>= v y))
```

So the result is-
```text
unsat
(error "line 10 column 10: model is not available")
```
