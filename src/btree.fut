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


--type layer_param = {depth: i64, remaining: i64, nodes: i64, items: i64}
def relax [h] (params : [h](i64, i64, i64, i64)) : [](i64, i64, i64, i64) =
  let leaf_layer_param = last params
  -- if |items| / |nodes| < degree in the last layer, we must "relax" the params a bit
  in if leaf_layer_param.3 / leaf_layer_param.2 < degree then
    [] -- TODO: Find out how to "relax"
  else
    params

type layer_param = {depth: i64, remaining: i64, nodes: i64, keys: i64}
def layer_param_nil : layer_param =
  {depth = -1i64, remaining = -1i64, nodes = -1i64, keys = -1i64}

-- analyze phase guarantees h ≤ max_height n
-- might require relaxation if
--     (last result).keys / (last result).nodes < degree - 1
def analyze (n : i64) : []layer_param =
  let rootlayer : layer_param = {depth = 0, remaining = n - 1, nodes = 1, keys = 1}
  in if n <= k then [rootlayer with keys = n]
  else
    let max_height : i64 = worst_case_height n
    in let dst = replicate (max_height+1) layer_param_nil
      with [0] = rootlayer

    in let (_,result) = loop (layer,tmp) = (head dst, dst)
    while layer.remaining > 0 && layer.depth < max_height do

      let nodes = layer.nodes + layer.keys in
      let (node_sz, remainder) = --if layer.remaining / nodes < k then
        if (i64.f64 <-< f64.ceil) (f64.i64 layer.remaining / f64.i64 nodes) <= k then
        (layer.remaining / nodes, layer.remaining % nodes)
      else
        (degree - 1, 0)

      in let num_keys = node_sz * nodes
      in let next_layer = {
        depth     = layer.depth + 1,
        remaining = layer.remaining - num_keys - remainder,
        nodes     = nodes,
        keys      = num_keys + remainder
      } in
      (next_layer, tmp with [layer.depth + 1] = next_layer)

    in let h = map (.depth) result |> reduce i64.max i64.lowest
    -- Cut off trailing unused layer params
    in take (h+1) result


-- start: 4^{x}*t
-- end:   2*t^{x+1}-2
-- t=5: Invalid ranges: 20-48,100-248,500-1248
-- t=4: Invalid ranges: 16-30,64-126,256-510
def test_valid_btree_params [h] (p : [h]layer_param) : bool =
  all (\q ->
    q.keys / q.nodes >= degree - 1 && q.keys / q.nodes <= k
  ) (tail p)


def invalid_range (n : i64) : bool =
  if n < degree then false else
  let max = (i64.f64 <-< f64.ceil) (logt n) in
  any (\x -> (4**x)*degree <= n && n <= 2 * degree**(x+1) - 2) (1...max)


def mk_depth_idx [m] (shape : [m]i64) =
  let shape_rot = rotate (-1) (copy shape) with [0] = 0
  let shape_scn = scan (+) 0 shape_rot
  let flags     = map (\_->1i64) (iota m) with [0] = 0
  let len       = last shape_scn + last shape
  in scatter (replicate len 0) shape_scn flags |> scan (+) 0



def tree_from_values_upsweep [n] [h] (ks : [n]i64) (vs : [n]datatype) (params : [h]layer_param) =
  let kv = zip ks vs
  in if h <= 1 then
    [node_new() with keys = kv
                with size = n]
  else
    let sizes  = map (.nodes) params
    let dst_sz = i64.sum sizes
    let dst    = replicate dst_sz (node_new ())
    --let layeridx = mk_depth_idx sizes
    let layeridx = scan (+) 0 sizes |> rotate (-1) with [0] = 0
    -- manually do the leaf nodes, loop over the rest
    in let (tree, _,_) = loop (res, aux, layer) = (dst, kv, (last params).depth)
    while layer >= 0 do
    -------------------------------------
      let p = params[layer] in
      let pn  = p.nodes
      in let nsz = p.keys / pn
      in let rem = p.keys % pn
      in let szs = reduce_by_index (replicate pn nsz) (+) 0 (iota rem) (replicate rem 1)
      --in let tmp = indices szs |> map (+1)
      in let src_indices = map2 (+)
                          (indices szs |> map (+1))
                          (scan (+) 0 szs)
                      |> rotate (-1)
                         with [0] = 0

      in let tmp' = replicate pn (node_new())

      in let newnodes = map3 (\i s (nn:node) ->
        let kk = drop i aux |> take s
        let parent = if layer == 0 then #null else #ptr 0
        in nn
          with keys   = scatter (copy nn.keys) (indices kk) kk
          with size   = s
          with parent = parent
      ) src_indices szs tmp'

      in let dst_idx = iota pn |> map (+(layeridx[layer]))
      in let next_kv_idx = (map2 (+) src_indices szs |> init)
      in let next_kvs = map (\i -> aux[i]) next_kv_idx

      in ( scatter (res) dst_idx newnodes
         , next_kvs
         , layer - 1)

    in tree


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
