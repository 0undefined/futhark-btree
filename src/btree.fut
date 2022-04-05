open import "../lib/github.com/diku-dk/sorts/radix_sort"
open import "types"
open import "btree-misc"
open import "btree-ops"
open import "btree-construct"


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


def merge_nodelist [n] [m] (dst : *[m]node) (_nodes : [n][1]node) : [m]node =
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
