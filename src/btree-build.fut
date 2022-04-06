open import "../lib/github.com/diku-dk/sorts/radix_sort"

open import "types"
open import "btree-misc"


-- returns:
--   number of required nodes to contain `n` elements
--   remainder of elements that are to be distributed ontop of the nodesize
--   nodesize
local def node_distribution (n : i64) : (i64, i64, i64) =
  let k = degree - 1
  in (n / k, n % k, k)


-- Assume [](keys,vals) are already sorted by key
def node_list_from_keyvalues [n] (keys: [n]i64) (vals: [n]datatype) : []node =
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

      in let keys = keyvals[dropsize:dropsize + partsize] :> [partsize]key

      in node_new () with size = partsize
                     with keys = scatter (newkeyarr()) (indices keys) keys
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
