open import "types"

def logt (x : i64) : f64 =
  (/) (f64.i64 x |> f64.log) (f64.i64 degree |> f64.log)


-- The maximum number of nodes in a tree is (2*t)^h - 1
def max_nodes (height: i64) =
  (/) (1 - c**(height + 1)) (1 - c)


-- The maximum number of keys containable in a tree is (2*t)^(h+1) - 1
def max_keys (height: i64) =
  (2 * degree)**(height+1) + (-1)


-- Upper bound on height of a tree with `n` keys
def worst_case_height n =
   (n + 1) / 2 |> logt |> i64.f64 <-< f64.ceil


-- Upper bound on nodes of a tree with `n` keys
def worst_case_size (n : i64) =
  worst_case_height n |> max_nodes
