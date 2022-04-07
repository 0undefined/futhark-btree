open import "../lib/github.com/diku-dk/sorts/radix_sort"

open import "types"
open import "btree-misc"

local type layer_param = {depth: i64, remaining: i64, nodes: i64, keys: i64}

-- start: 4^{x}*t
-- end:   2*t^{x+1}-2
-- t=5: Invalid ranges: 20-48,100-248,500-1248
-- t=4: Invalid ranges: 16-30,64-126,256-510
def invalid_range (n : i64) : bool =
  if n < degree then false else
  let max = (i64.f64 <-< f64.ceil) (logt n) in
  any (\x -> (4**x)*degree <= n && n <= 2 * degree**(x+1) - 2) (1...max)


local def test_valid_btree_params [h] (p : [h]layer_param) : bool =
  all (\q ->
    q.keys / q.nodes >= degree - 1 && q.keys / q.nodes <= k
  ) (tail p)


local def layer_param_nil : layer_param =
  {depth = -1i64, remaining = -1i64, nodes = -1i64, keys = -1i64}


local def relax [h] (params : [h](i64, i64, i64, i64)) : [](i64, i64, i64, i64) =
  let leaf_layer_param = last params
  -- if |items| / |nodes| < degree in the last layer, we must "relax" the params a bit
  in if leaf_layer_param.3 / leaf_layer_param.2 < degree then
    [] -- TODO: Find out how to "relax"
  else
    params


-- adds +1 to the first `rem` node sizes
local def add_remainder (num_nodes : i64) (node_sz : i64) (remainder : i64) =
  reduce_by_index (replicate num_nodes node_sz) (+) 0 (iota remainder) (replicate remainder 1)


local def mk_depth_idx [m] (shape : [m]i64) =
  let flags     = replicate m 1i64 with [0] = 0
  let shape_rot = rotate (-1) (copy shape) with [0] = 0
  let shape_scn = scan (+) 0 shape_rot
  let len       = last shape_scn + last shape
  in scatter (replicate len 0) shape_scn flags |> scan (+) 0


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


-- constructs a btree from a list of keys, values and b-tree parameters that
-- describe the shape of the given B-Tree.
def construct [n] [h] (ks : [n]i64) (vs : [n]datatype) (params : [h]layer_param) : []node =
  let kv = zip ks vs
  in if h <= 1 then
    let root = node_new() in
    [ root with keys = scatter (copy root.keys) (indices kv) kv
           with size = n ]
  else
    let sizes         = map (.nodes) params
    let dst_sz        = i64.sum sizes
    let dst           = replicate dst_sz (node_new ())
    let layeridx      = scan (+) 0 sizes |> rotate (-1) with [0] = 0

    -------------------------------------
    --TODO: GENERATE `szs` for all layers
    --
    -- (map ∘ replicate) (map .nodes params)
    -------------------------------------

    in let (tree, _,_) = loop (res, aux, layer) = (dst, kv, (last params).depth)
    while layer >= 0 do
      -- Preliminary info about the current layer
      let p = params[layer]
      in let pn  = p.nodes     -- number of nodes
      in let nsz = p.keys / pn -- node size / number of keys in each node
      in let rem = p.keys % pn -- remaining keys that need to be distributed

      in let szs = add_remainder pn nsz rem

      in let child_szs  = map (+1) szs
      in let sum_childs = i64.sum child_szs

      in let child_indices = ((rotate (-1) child_szs) with [0] = 0)
                           |> scan (+) 0

      in let src_indices = map2 (+)
                          (indices szs |> map (+1))
                          (scan (+) 0 szs)
                      |> rotate (-1)
                         with [0] = 0

      -- TODO: This function makes incredibly redundant calculations, move this!
      in let parent_idx (l: i64) : [pn]ptr =
        if l == 0 then replicate pn #null else
        let p = params[l-1]
        in let p2n = p.nodes
        in let nsz = p.keys / p2n
        in let rem = p.keys % p2n
        in let szs = add_remainder p2n nsz rem |> map (+1) :> [p2n]i64

        -- if Σ szs2 != pn then we messed up

        in let pidx = map (+layeridx[layer-1]) (iota sizes[layer-1]) |> map ptr_from_i64
        in let sidx = mk_depth_idx szs :> [pn]i64
        in map (\i -> pidx[i]) sidx


      in let children_idx (l: i64) : [sum_childs]ptr =
        if l == h - 1 then replicate sum_childs #null else
        map (((+) layeridx[l+1]) >-> ptr_from_i64) (iota sum_childs)



      in let parent_ptrs : [pn]ptr = parent_idx layer
      in let child_ptrs  : [sum_childs]ptr = children_idx layer

      in let newnodes = map4 (\i s p ci ->
        let kk = aux[i:i+s] in
        let cc = child_ptrs[ci:ci+s+1] in
          { leaf     = layer == h-1
          , parent   = p
          , size     = s
          , keys     = scatter (newkeyarr())   (indices kk) kk
          , children = scatter (newchildarr()) (indices cc) cc
          }
      ) src_indices szs parent_ptrs child_indices

      in let dst_idx = iota pn |> map (+(layeridx[layer]))
      in let next_kv_idx = (map2 (+) src_indices szs |> init)
      in let next_kvs = map (\i -> aux[i]) next_kv_idx

      in ( scatter (res) dst_idx newnodes
         , next_kvs
         , layer - 1)

    in tree
