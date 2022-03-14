-- constant parameters
type datatype = i64

def nilval : i64 = 0i64
-- def degree : i64 = 32   -- aka. `t` in CLRS
def degree : i64 = 32   -- aka. `t` in CLRS

-- All nodes (except the root node) must contain a number of k keys
--   such that  t - 1 ≤ k ≤ 2t - 1.
-- Let the number of child nodes be c, then c = k + 1 given the node is
--   not a leaf.
def k : i64 = degree * 2 - 1
def c : i64 = degree * 2

type ptr = #null | #ptr i64
-- is_leaf: Indicates wether or not there's children to this node.
-- size: Number of keys in the node.
-- keys: List of tuple of key and data.
-- children: List of i64 indices to child nodes. A value of (-1) means no child.
-- parent: Index of parent node, -1 if root
--
-- Notes: The number of children (if any) are always equal to (size + 1)
type node = {
  is_leaf:  bool,
  parent:   ptr,
  size:     i64,
  keys:     [k](i64, datatype),
  children: [c]ptr
}

def ptrval (p : ptr) : i64 =
  match p
  case #null  -> (-1)
  case #ptr q -> q
