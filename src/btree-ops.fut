open import "types"
-- def fuse (n0 : node) (n1 : node) : node =
--   let splitter_key ≤ any key ϵ n0
--                    ≥ any key ϵ n1
--   in n0 ++ n1

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
