open import "../lib/github.com/diku-dk/sorts/radix_sort"
open import "types"
open import "btree-ops"


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


-- returns:
--   number of required nodes to contain `n` elements
--   remainder of elements that are to be distributed ontop of the nodesize
--   nodesize
def node_distribution (n : i64) : (i64, i64, i64) =
  let k = degree - 1
  in (n / k, n % k, k)


-- Assume [](keys,vals) are already sorted by key
entry node_list_from_keyvalues [n] (keys: [n]i64) (vals: [n]datatype) : []node =
  if n <= k then
    -- Insert all elements into the new root node
    [ node_new ()
        with size = n
        with keys = scatter (newkeyarr ()) (indices keys) (zip keys vals)
    ]
  else
    let (n_nodes, remainder, n_items) = node_distribution n

    in let keyvals = zip keys vals
    in tabulate n_nodes (\i ->
      let (partsize,dropsize) =
        if i < remainder then -- the first nodes with index smaller than `remainder` should
                              -- have an extra item
          (n_items + 1, i * (n_items+1))
        else
          (n_items, remainder * (n_items+1) + (i-remainder) * n_items)

      in node_new () with size = partsize
                     with keys = scatter (replicate k (-1,nil))
                                          (iota partsize)
                                          (drop dropsize keyvals |> take partsize)
    )


-- Downsweep method guarantees h ≤ max_height n
def tree_from_values_downsweep [n] (ks : [n]i64) (vs : [n]datatype) : [](i64,i64,i64,i64) = -- : []node =
  if n <= k then [ (0, 0, 1, n) ]
  else
    let max_height  = worst_case_height n
    let max_node_sz = max_nodes max_height
    let max_n_keys  = max_keys  max_height
    let keyvals     = zip ks vs
    --let root = (scatter_node_keys (node_new ()) (newkeyarr () with [0] = keyvals[length keyvals / 2]))
    --  with size = 1
    --let result = replicate max_node_sz (node_new ())
    --  with [0] = root

    -- Create parameters for each layer in the tree
    let tree_param_init = replicate (max_height+1) (-1,-1,-1,-1) with [0] = (0, n - 1, 1, 1)
    let (_,tree_params) = loop
      -- Take:
      --   previous depth,
      --   remaining items,
      --   previous layer nodes and
      --   previous layer number of items
      -- as parameter, and the resulting array as "auxiliary"
      ((depth, rem, prev_nodes, prev_items), params) = (head tree_param_init, tree_param_init)
    while
      depth <= max_height && rem > 0
    do
      -- the number of nodes in current layer = |prev_layer_items| + |prev_layer_nodes|
      let nodes = prev_items + prev_nodes in
      -- determine nodesize for current layer
      let (node_sz, rr) = -- rr is the `real remainder` in this layer
        if rem / nodes < k then
          (rem / nodes, rem % nodes)
        else
          -- Fallback to the minimum number of keys a node can have
          (degree - 1, 0)
      in
      let items = node_sz * nodes in
      let localres =
      ( depth + 1         -- next depth
      , rem - items - rr  -- remaining items
      , nodes             -- number of nodes in this layer
      , items + rr        -- number of items in this layer
      ) in

      (localres, params with [depth+1] = localres)

    in let h = map (.0) tree_params |> reduce i64.max i64.lowest
    -- Cut off trailing unused layer params
    in take (h+1) tree_params
    -- TODO: Scatter keyvals to corresponding nodes
    -- (Done in upsweep)

def merge_nodelist [n] [m] (dst : *[m]node) (nodes : [n][1]node) : [m]node =
  let max_h = 0 in
  copy dst

def tree_from_nodelist [n] (nodes : [n]node) : []node =
  if n <= 1 then nodes
  else
    merge_nodelist (replicate n (node_new ())) (unflatten (length nodes) 1 nodes)


--entry tree_from_nodelist [n] (nodes : [n]node) : []node =
--  -- First, determine wether or not `nodes` is even or odd
--  -- Add an empty node if odd
--  let nodes_sgmt = unflatten (length nodes) 1 nodes in
--  let layer1 = map (\idx ->
--    let fused = fuse_leaf nodes_sgmt[idx*2] nodes_sgmt[idx*2 + 1]
--    let fused_len = length fused
--    in if fused_len > k then
--        -- Split into a tree, this case is very likely
--        let sk = fused[fused_len / 2]
--        let keys_l = take
--        in
--        [ node_new () with keys[0] = sk
--                       with children[0] = #ptr 1
--                       with children[1] = #ptr 2
--        , node_new () with keys = scatter (newkeyarr ()) (iota (fuse_len / 2)) (take (fuse_len / 2) fuse)
--        , ]
--      else
--        -- Just create a new node
--        let nn = node_new () with keys = scatter (newkeyarr ()) (indices fused) fused
--        in [ nn
--           , node_new () with size = -1
--           , node_new () with size = -1 ]
--  ) (iota <| length nodes_sgmt / 2)
--
--  in reduce (btree_join_parallel)


def node_from_tuple (n: node) : (i64, i64, [k]i64, [k]datatype, [c]i64) =
  let (keys, vals) = unzip n.keys
  in (ptrval n.parent, n.size, keys, vals, map ptrval n.children)


---- returns the indices of nodes containing keys
--def btree_search_nodes [n] [m] (t : [n]node) (keys : [m]i64) : []i64 =
--  -- start from the root node
--  let (result,_) = loop (_, aux) = ([], [(0,t[0])])
--  while (!all ((.1)>->(.leaf)) aux) && !null aux do
--    -- is any of the keys in aux?
--    let idx = map (
--      \(i,xx) -> (i, any (\x -> any ((==)x) keys) xx)
--    ) (map (\(i,n) ->
--        let keys = unzip n.keys |> (.0)
--        in (i, keys) :> (i64, [k]i64)
--      ) aux) |> filter (.1) |> unzip |> (.0)
--    -- If any k ϵ aux.keys
--    in (idx, [])
--  in result

entry main [n] (keys: [n]i64) (vals: [n]datatype) : [](i64, i64, [k]i64, [k]datatype, [c]i64) =
   let (sorted_k, sorted_v) =
     zip keys vals
     |> radix_sort_by_key (.0) (i64.num_bits) (i64.get_bit)
     |> unzip
   in node_list_from_keyvalues sorted_k sorted_v |> map node_from_tuple
