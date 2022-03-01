-- ==
-- entry: construct_tree_from_keyvals_bench
-- random input {[100]i64 [100]i64} auto output
-- random input {[1000]i64 [1000]i64} auto output
-- random input {[10000]i64 [10000]i64} auto output
-- random input {[100000]i64 [100000]i64} auto output
-- random input {[1000000]i64 [1000000]i64} auto output
-- random input {[10000000]i64 [10000000]i64} auto output
open import "../lib/github.com/diku-dk/sorts/radix_sort"
open import "types"
open import "btree-ops"


let logt (t : i64) : (i64->f64) = (\x -> (f64.log <| f64.i64 x) / (f64.log <| f64.i64 t))

-- The minimum number of nodes in a tree is (2*t)^h - 1
let min_tree_size (height: i64) = 2 * degree**height |> (+) (-1)

-- Upper bound on height of a tree with `n` keys
-- TODO: Maybe use `ceil` ?
let worst_case_height n = i64.f64 <-< f64.round <| logt degree <| (n + 1) / 2

-- Upper bound on nodes of a tree with `n` keys
let worst_case_size (n : i64) = worst_case_height n |> min_tree_size

-- Returns a "somewhat optimal" height of a tree that must be able to contain a
-- minimum of `n` keys
--  The goal is to get an idea of what hight is optimal in order to contain `n`
--  keys. Keeping in mind that each node can contain an interval of `t-1` up until
--  `2t` keys.
let min_height (n : i64) : i64 =
  let max_height = worst_case_height n
  let range      = degree - 1 ... degree * 2 -- the min and max number of elems of a node
  -- Create a list of
  in map (
    -- Iterate over the range given a height
    \h -> map (\i -> ((min_tree_size h) * i > n,h)) range
  ) (0 ... worst_case_height n)
  |> flatten
  |> filter (\(p,_)->p) -- filter out heights+sizes that are too small
  |> head               -- get the smallest height that is able to contain `n` keys
  |> (.1)

let node_new (nil: datatype) : node =
  let keys = replicate k nil |> zip <| iota k
  in { is_leaf  = true
     , parent   = (-1)
     , size     = 0i64
     , keys     = keys
     , children = replicate c (-1)
     }


let tree_height [n] (root : [n]node) : i64 =
  (.0) <| loop (i, r) = (0, head root) while !r.is_leaf
    do (i + 1, root[head r.children])


-- let min_tree_size (n: i64) : i64


-- Assume [](keys,vals) are already sorted by key
def construct_tree_from_keyvals [n] (nil: datatype) (keys: [n]i64) (vals: [n]datatype) : []node =
  let root = node_new nil in
  if n <= k then
    -- Insert all elements into the new root node
    [root with keys = scatter (copy root.keys) (iota n) (zip keys vals)]
  else
    -- We want to find a good number of nodes to evenly distribute the values
    -- s.t. the b-tree properties are still valid
    let (n_nodes, remainder, n_items) =
      loop (nn, rr, i) = (n / k, n % k, k)
      -- Good candidates:
      --   remainder == 0                            -- nothing to redistribute
      --   remainder < |nodes| && |nodes| > (n / k)  -- ie. there's room for at
      --                                                least 1 more element in
      --                                                each node
      --
      -- Hopefully we won't reach the case of `i=degree-1`
      while (rr != 0
             && !(rr < nn && i < k - 1))
             && i >= degree - 1 do
        (n / (i-1), n % (i-1), i - 1)


    in tabulate n_nodes (\i ->
      let (partsize,dropsize) =
        if i < remainder then -- the first nodes with index smaller than `remainder` should
                              -- have an extra item
          (n_items + 1, i * (n_items+1))
        else
          (n_items, remainder * (n_items+1) + (i-remainder) * n_items)

      in {
        is_leaf = true,
        parent  = (-1),
        size    = partsize,
        keys    = scatter (replicate k (-1,nil)) (iota partsize) (zip keys vals |> drop dropsize |> take partsize),
        children = replicate c (-1)
      }
    )


def node_from_tuple (n: node) : (bool, i64, i64, [k]i64, [k]datatype, [c]i64) =
  let (keys, vals) = unzip n.keys
  in (n.is_leaf, n.parent, n.size, keys, vals, n.children)


entry main [n] (keys: [n]i64) (vals: [n]datatype) : [](bool, i64, i64, [k]i64, [k]datatype, [c]i64) =
   let (sorted_k, sorted_v) =
     zip keys vals
     |> radix_sort_by_key (.0) (i64.num_bits) (i64.get_bit)
     |> unzip
   in construct_tree_from_keyvals nilval sorted_k sorted_v |> map node_from_tuple
