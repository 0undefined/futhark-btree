open import "../lib/github.com/diku-dk/sorts/radix_sort"
open import "btree-ops.fut"

-- constant parameters
type datatype = i64

local def nilval : i64 = 0i64
local def degree : i64 = 32   -- aka. `t` in CLRS

-- All nodes (except the root node) must contain a number of k keys
--   such that  t - 1 ≤ k ≤ 2t - 1.
-- Let the number of child nodes be c, then c = k + 1 given the node is
--   not a leaf.
local def k : i64 = degree * 2 - 1
local def c : i64 = degree * 2

-- is_leaf: Indicates wether or not there's children to this node.
-- size: Number of keys in the node.
-- keys: List of tuple of key and data.
-- children: List of i64 indices to child nodes. A value of (-1) means no child.
-- parent: Index of parent node, -1 if root
--
-- Notes: The number of children (if any) are always equal to (size + 1)
type node = {
  is_leaf:  bool,
  size:     i64,
  keys:     [k](i64, datatype),
  children: [c]i64,
  parent:   i64
}

let logt (t : i64) : f64 = (\x -> (f64.log <| f64.i64 x) / (f64.log <| f64.i64 t))

-- The minimum number of nodes in a tree is (2*t)^h - 1
let min_tree_size (height: i64) = 2 * degree**height |> (+) (-1)

-- Upper bound on height of a tree with `n` keys
-- TODO: Maybe use `ceil` ?
let worst_case_height n = i64.f64 <-< f64.round <| logt degree <| (n + 1) / 2

-- Upper bound on nodes of a tree with `n` keys
let worst_case_size (n : i64) = worst_case_height n |> tree_size

-- Returns a "somewhat optimal" height of a tree that must be able to contain a
-- minimum of `n` keys
--  The goal is to get an idea of what hight is optimal in order to contain `n`
--  keys. Keeping in mind that each node can contain an interval of `t-1` up until
--  `2t` keys.
let min_height (n : i64) : i64 =
  let max_height = worst_case_height n
  let range      = degree - 1 ... degree * 2 -- the min and max number of elems of a node
  -- Create a list of
  (replicate (wch n) range)
  in map (
    -- Iterate over the range given a height
    \h -> map (\i -> ((min_tree_size h) * i > n,h)) range
  ) (0 ... wch n)
  |> flatten
  |> filter (\(p,_)->p) -- filter out heights+sizes that are too small
  |> head               -- get the smallest height that is able to contain `n` keys
  |> (.1)

let node_new (nil: datatype) : node =
  let keys = replicate k nil |> zip <| iota k
  in { is_leaf  = true
     , size     = 0i64
     , keys     = keys
     , children = replicate c (-1)
     }


let tree_height [n] (root : [n]node) : i64 =
  (.0) <| loop (i, r) = (0, head root) while !r.is_leaf
    do (i + 1, root[head r.children])


let min_tree_size (n: i64) : i64


def construct_tree_from_keyvals [n] (nil: datatype) (keys: [n]i64) (vals: [n]datatype) : []node =
  let root = node_new nil in
  if n <= k then
    -- Insert all elements into the new root node
    [root with keys = scatter (copy root.keys) (iota n) (zip keys vals)]
  else
    [root]


entry main [n] (keys: [n]i64) (vals: [n]datatype) : []datatype =
  let (sorted_k, sorted_v) =
    zip keys vals
    |> radix_sort_by_key (.0) (i64.num_bits) (i64.get_bit)
    |> unzip
  in head (construct_tree_from_keyvals nilval sorted_k sorted_v) |> (.keys) |> take n |> unzip |> (.1)
