let rec fibonacci n =
  if n < 1 then 0 else
    if n < 2 then 1 else
      let first = fibonacci (n - 1) in
      let second = fibonacci (n - 2) in
      first + second
