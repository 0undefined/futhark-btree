open import "types"
def fuse (nil: datatype) (n0 : node) (n1 : node) (sk : (i64,datatype)) : node =
  --   let splitter_key ≤ any key ϵ n0
  --                    ≥ any key ϵ n1
  let keyvals = (n0.keys ++ [sk] ++ n1.keys) |> filter ((.1) >-> (!=) nil) in
  let children = n0.children ++ n1.children |> filter (ptrval >-> (!=) (-1)) in
  {
    is_leaf = n0.is_leaf || n1.is_leaf,
    parent   = match (n0.parent, n1.parent)
              case (#null,p1) -> p1
              case (p0,_)     -> p0,
    size     = n0.size + n1.size + 1,
    keys     = scatter (replicate k (-1i64,nil)) (iota <| length keyvals) keyvals,
    children = children
  }

-- split a tree using splitter key `x`
-- t: tree
-- i: index of the splitting node
-- x: index of the splitting key in t[i]
def btree_split [n] (t : *[n]node) (i: i64) (x : i64) : ([]node, []node) =
  -- 1. Loop through the layers of `t`:
  --    Split nodes into lhs and rhs, update nodes accordingly
  ([],[])

def btree_join [n] [m] (t0 : *[n]node) (t1 : *[m]node) : []node =
  -- Join t0 and t1 into t
  []
