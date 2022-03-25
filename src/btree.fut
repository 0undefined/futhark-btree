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
  let _max_height = worst_case_height n
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
        let ii = i + 1 in
        (n / ii, n % ii, ii)


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


--entry tree_from_nodelist [n] (nil : datatype) (nodes : [n]node) : []node =
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
--        [ node_new nil with keys[0] = sk
--                       with children[0] = #ptr 1
--                       with children[1] = #ptr 2
--        , node_new nil with keys = scatter (newkeyarr nil) (iota (fuse_len / 2)) (take (fuse_len / 2) fuse)
--        , ]
--      else
--        -- Just create a new node
--        let nn = node_new nil with keys = scatter (newkeyarr nil) (indices fused) fused
--        in [ nn
--           , node_new nil with size = -1
--           , node_new nil with size = -1 ]
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
--  while (!all ((.1)>->(.is_leaf)) aux) && !null aux do
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
   in node_list_from_keyvalues nilval sorted_k sorted_v |> map node_from_tuple
