open import "../lib/github.com/diku-dk/sorts/radix_sort"
open import "types"
open import "btree-ops"

def logt (t : i64) : (i64->f64) = (\x -> (/) (f64.i64 x |> f64.log) (f64.i64 t |> f64.log))

-- The minimum number of nodes in a tree is (2*t)^h - 1
def min_tree_size (height: i64) = 2 * degree**height |> (+) (-1)

-- Upper bound on height of a tree with `n` keys
-- TODO: Maybe use `ceil` ?
def worst_case_height n = i64.f64 <-< f64.round <| logt degree <| (n + 1) / 2

-- Upper bound on nodes of a tree with `n` keys
def worst_case_size (n : i64) = worst_case_height n |> min_tree_size

-- Returns a "somewhat optimal" height of a tree that must be able to contain a
-- minimum of `n` keys
--  The goal is to get an idea of what hight is optimal in order to contain `n`
--  keys. Keeping in mind that each node can contain an interval of `t-1` up until
--  `2t` keys.
def min_height (n : i64) : i64 =
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




-- let min_tree_size (n: i64) : i64


-- Assume [](keys,vals) are already sorted by key
entry node_list_from_keyvalues [n] (nil: datatype) (keys: [n]i64) (vals: [n]datatype) : []node =
  let root = node_new nil in
  if n <= k then
    -- Insert all elements into the new root node
    [root with keys = scatter (copy root.keys) (iota n) (zip keys vals)]
  else
    -- We want to find a good number of nodes to evenly distribute the values
    -- s.t. the b-tree properties are still valid
    let t = degree - 1 in -- start from minimum number of keys to maximize number of nodes
    let (n_nodes, remainder, n_items) =
      loop (nn, rem, i) = (n / t, n % t, t)
      -- Good candidates:
      --   remainder == 0                            -- nothing to redistribute
      --   remainder < |nodes| && |nodes| > (n / k)  -- ie. there's room for at
      --                                                least 1 more element in
      --                                                each node
      --
      -- Hopefully we won't reach the case of `i=k`
      while (rem != 0
             && !(rem < nn && i < k))
             && i <= k do
        (n / (i + 1), n % (i + 1), i + 1)


    in let keyvals = zip keys vals
    in tabulate n_nodes (\i ->
      let (partsize,dropsize) =
        if i < remainder then -- the first nodes with index smaller than `remainder` should
                              -- have an extra item
          (n_items + 1, i * (n_items+1))
        else
          (n_items, remainder * (n_items+1) + (i-remainder) * n_items)

      in node_new nil with size = partsize
                      with keys = scatter (replicate k (-1,nil))
                                          (iota partsize)
                                          (drop dropsize keyvals |> take partsize)
    )

def node_from_tuple (n: node) : (bool, i64, i64, [k]i64, [k]datatype, [c]i64) =
  let (keys, vals) = unzip n.keys
  in (n.is_leaf, ptrval n.parent, n.size, keys, vals, map ptrval n.children)


entry main [n] (keys: [n]i64) (vals: [n]datatype) : [](bool, i64, i64, [k]i64, [k]datatype, [c]i64) =
   let (sorted_k, sorted_v) =
     zip keys vals
     |> radix_sort_by_key (.0) (i64.num_bits) (i64.get_bit)
     |> unzip
   in node_list_from_keyvalues nilval sorted_k sorted_v |> map node_from_tuple

def testtree (n : i64) : []node =
  let kk = iota n
  let vv = map (*2) kk
  in node_list_from_keyvalues (-1) kk vv
