open import "types"


-- Assumption: r(n0) == r(n1)
-- sk ≥ all keys in n0 and sk ≤ all keys in n1
-- n0.size + n1.size <= k
def fuse (n0 : node) (n1 : node) (sk : (i64,datatype)) : node =
  let offset = (+) (1 + n0.size) -- DRY
  in let newkeys_t = filter ((.0) >-> (!=)(-1)) n1.keys
  in let newkeys   = scatter (copy n0.keys with [n0.size] = sk) (indices newkeys_t |> map offset) newkeys_t

  -- If they're both leafs, we will end up scattering empty arrays
  in let newchilds_t = filter ((!=) #null) n1.children
  in let newchilds   = scatter (copy n0.children) (indices newchilds_t |> map offset) newchilds_t

  -- Merge with children
  in n0 with keys     = newkeys
        with children = newchilds
        with size     = offset n1.size


-- Split a node into two nodes and a splitter key
def node_split [kk] [cc] (nil: datatype) (keyvals : [kk](i64,datatype)) (children : [cc]ptr) : (node, node, (i64, datatype)) =
  let half = kk / 2
  let is_leaf = all ((==) #null) children
  let n0t = node_new nil
            with is_leaf = is_leaf
            with size = half
  let n1t = node_new nil
            with is_leaf = is_leaf
            with size = i64.f64 (f64.ceil (f64.i64 kk / 2f64))

  let (n0k,n1k_t) = split half keyvals
  in let (n0c,n1c) = split (half+1) children
  in let (n1k, sk) = (tail n1k_t, head n1k_t)

  in ( n0t with keys     = scatter (newkeyarr nil) (iota half)     n0k
           with children = scatter (newchildarr()) (iota (half+1)) n0c
     , n1t with keys     = scatter (newkeyarr nil) (indices n1k)   n1k
           with children = scatter (newchildarr()) (indices n1c)   n1c
     , keyvals[half])



-- "Fuse" two nodes and split the evenly
def fuse_split (nil : datatype) (n0 : node) (n1 : node) (sk: (i64,datatype)) : (node, node, (i64, datatype)) =
  let fuse_size = n0.size + n1.size + 1 -- +1 for splitter key `sk`

  -- Replicate is needed here, since fuse_size might be larger than `k`
  let fuse_keys_tmp = scatter (replicate fuse_size (-1i64, nil)) (iota k) n0.keys
  let fuse_keys     = scatter
                      (copy fuse_keys_tmp with [n0.size] = sk)
                      (map2 (\i (kid,_) -> if kid == -1 then -1 else 1+i+n0.size) (iota k) n1.keys)
                      n1.keys

  let fuse_c_tmp = scatter (replicate (fuse_size+1) #null) (iota c) n0.children
  let fuse_c     = scatter (copy fuse_c_tmp)
                           (map2 (\i p -> if ptrval p == -1 then -1 else 1+i+n0.size) (iota c) n1.children)
                           n1.children

  in node_split nil fuse_keys fuse_c
  --

-- split a tree using splitter key `x`
-- t: tree
-- i: index of the splitting node
-- x: index of the splitting key in t[i]
def btree_split [n] (t : *[n]node) (i: i64) (x : i64) : ([]node, []node) =
  -- 1. Loop through the layers of `t`:
  --    Split nodes into lhs and rhs, update nodes accordingly
  ([],[])

-- t: tree to decent into
-- j: the node to start from (left as a variable instead of using 0 for constructing a tree from a list)
-- r: the rank to stop at
-- returns: The index of the node in `t` that has rank `r`, following the left-most path
def decent_l [n] (t : [n]node) (j : i64) (r : i64) : i64 =
  let h = tree_rank t j
  in if h == 0 then j
  else
    let (i, _) = loop (i, nn) = (h, t[j]) while i > r && !nn.is_leaf do
      ( filter (ptrval >-> ((!=) (-1))) nn.children |> head |> ptrval
      , t[i])
    in i

-- t: tree to decent into
-- j: the node to start from (left as a variable instead of using 0 for constructing a tree from a list)
-- r: the rank to stop at
-- returns: The index of the node in `t` that has rank `r`, following the right-most path
def decent_r [n] (t : [n]node) (j : i64) (r : i64) : i64 =
  let h = tree_rank t j
  in if h == 0 then j
  else
    let (i, _) = loop (i, nn) = (h, t[j]) while i > r && !nn.is_leaf do
      ( filter (ptrval >-> ((!=) (-1))) nn.children |> last |> ptrval
      , t[i])
    in i

def min_key [n] (t : [n]node) (j : i64) =
  let idx = decent_l t j i64.lowest
  in t[idx].keys |> filter ((.0) >-> (!=) (-1i64)) |> head

def max_key [n] (t : [n]node) (j : i64) =
  let idx = decent_r t j i64.highest
  in t[idx].keys |> filter ((.0) >-> (!=) (-1i64)) |> last

def btree_join [n] [m] (t0 : *[n]node) (t1 : *[m]node) : []node =
  -- Join t0 and t1 into t
  -- 1. Decent right into t0 until r(t0') == r(t1)
  -- 2. Pick largest key in t0 as splitter key `sk`
  -- 3. if |t0| + |t1| < k then fuse
  --    else fuse_split t0 t1 sk (balances the keys/children)
  --         update sk
  --         t0.parent.keys ++ sk
  --         t0.parent.children ++ &t0
  -- 4. Traverse back to root, splitting any nodes that are overfull
  --
  []
