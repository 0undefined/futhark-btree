open import "types"

-- Filter-scatter functions for key and children arrays
-- They're the same thing, but with different predicates

def scatter_filter 'a [l] (dst : *[l]a) (p : a -> bool) (is : [l]i64) (as : [l]a) : *[l]a =
  let (iis, aas) = map2 (\i v -> if p v then (i,v) else (-1i64, v)) is as |> unzip
  in scatter dst iis aas

def scatter_keys (dst : *[k]key) (is : [k]i64) (keys : [k]key) : *[k]key =
  scatter_filter dst valid_key is keys

def scatter_children (dst : *[c]ptr) (is : [c]i64) (cs : [c]ptr) : *[c]ptr =
  scatter_filter dst valid_ptr is cs


-- splits an array at `i` and returns the element at the `i`th location and both
-- sides of the array.
def split_k [n] 't (i : i64) (xs : [n]t) : (t, [i]t, []t) =
  let (lhs, tmp) = split i xs in
  let (k,   rhs) = (head tmp, tail tmp)
  in (k, lhs, rhs)

def fuse_leaf (n0 : node) (n1 : node) : []key =
  filter ((.0)>->(!=)(-1)) (n0.keys ++ n1.keys)


-- Assumption: r(n0) == r(n1)
-- sk ≥ all keys in n0 and sk ≤ all keys in n1
-- n0.size + n1.size <= k
def fuse_internal (n0 : node) (n1 : node) (sk : key) : node =
  let offset = (+) (1 + n0.size) -- DRY
  in let idx = indices n1.children |> map offset :> [c]i64

  in let newkeys = scatter_keys
    (copy n0.keys with [n0.size] = sk)
    (init idx :> [k]i64) -- this is guaranteed since `k = c - 1` in def (see types.fut)
    n1.keys

  -- If they're both leafs, we will end up scattering empty arrays
  in let newchilds = scatter_children
    (copy n0.children)
    idx
    n1.children

  -- Merge with children
  in n0 with keys     = newkeys
        with children = newchilds
        with size     = offset n1.size


-- Split a node into two nodes and a splitter key
-- TODO: Fix sizes
def node_split [kk] [cc] (nil: datatype) (keyvals : [kk]key) (children : [cc]ptr) : (node, node, key) =
  let half = kk / 2
  let is_leaf = all ((==) #null) children
  let n0t = node_new nil
            with is_leaf = is_leaf
            with size = half
  let n1t = node_new nil
            with is_leaf = is_leaf
            with size = i64.f64 (f64.ceil (f64.i64 kk / 2f64))

  in let (n0c,n1c) = split (half+1) children
  in let (sk, n0k, n1k) = split_k half keyvals

  in ( n0t with keys     = scatter (newkeyarr nil) (iota half)     n0k
           with children = scatter (newchildarr()) (iota (half+1)) n0c
     , n1t with keys     = scatter (newkeyarr nil) (indices n1k)   n1k
           with children = scatter (newchildarr()) (indices n1c)   n1c
     , sk)


-- "Fuse" two nodes and split them evenly
def fuse_split (nil : datatype) (n0 : node) (n1 : node) (sk: key) : (node, node, key) =
  let fuse_size = n0.size + n1.size + 1 -- +1 for splitter key `sk`

  -- Replicate is needed here, since fuse_size might be larger than `k`
  let fuse_keys_tmp = scatter (replicate fuse_size (-1i64, nil)) (iota k) n0.keys
  let fuse_keys     = scatter
                      (copy fuse_keys_tmp with [n0.size] = sk)
                      (map2 (\i (kid,_) -> if kid == -1 then -1 else 1+i+n0.size) (iota k) n1.keys)
                      n1.keys

  let fuse_c_tmp = scatter (replicate (fuse_size+1) #null) (iota c) n0.children
  let fuse_c     = scatter (copy fuse_c_tmp)
                           (map2 (\i p -> if p == #null then -1 else 1+i+n0.size) (iota c) n1.children)
                           n1.children

  in node_split nil fuse_keys fuse_c


-- t: tree to decent into
-- j: the node to start from (left as a variable instead of using 0 for constructing a tree from a list)
-- r: the rank to stop at
-- returns: The index of the node in `t` that has rank `r`, following the left-most path
def decent_l [n] (t : [n]node) (j : i64) (r : i64) : i64 =
  let h = tree_rank t j
  in if h == 0 then j
  else
    let (i, _) = loop (i, nn) = (h, t[j]) while i > r && !nn.is_leaf do
      ( filter ((!=) #null) nn.children |> head |> ptrval
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
      ( filter ((!=) #null) nn.children |> last |> ptrval
      , t[i])
    in i


def min_key [n] (t : [n]node) (j : i64) =
  let idx = decent_l t j i64.lowest
  in t[idx].keys |> filter ((.0) >-> (!=) (-1i64)) |> head


def max_key [n] (t : [n]node) (j : i64) =
  let idx = decent_r t j i64.highest
  in t[idx].keys |> filter ((.0) >-> (!=) (-1i64)) |> last


def btree_join [n] [m] (_t0 : [n]node) (_t1 : [m]node) : []node =
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

-- Parallel join
-- Maintain a list of pointers to right spine of each tree
--   can be done in O(2h)
-- update pointers to spine scatter (U.spine) (indices + (|U.h| - |V.h|) (V.spine)


-- split a tree using splitter key `x`
-- t: tree
-- i: index of the splitting node
-- x: index of the splitting key in t[i]
def btree_split [n] (t : *[n]node) (i: i64) (_x : i64) : ([]node, []node) =
  -- 1. Loop through the layers of `t`:
  --    Split nodes into lhs and rhs, update nodes accordingly
  -- Find the node containing x
  --let (i,_) = loop (0while
  --  filter ((<) x) t
  --do
  let startnode = t[i]
  in let (ll, rr) = node_split startnode
  in loop (lhs, rhs, aux) = ([], [], startnode) while
    aux.parent != (-1)
  do
    --
  ([],[])
