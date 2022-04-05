-- constant parameters
type datatype = i64
def nil : datatype = (-1)

def nilkey : i64 = -1
def degree : i64 = 4   -- aka. `t` in CLRS

-- All nodes (except the root node) must contain a number of k keys
--   such that  t - 1 ≤ k ≤ 2t - 1.
-- Let the number of child nodes be c, then c = k + 1 given the node is
--   not a leaf.
def k : i64 = degree * 2 - 1
def c : i64 = degree * 2

type ptr = #null | #ptr i64
type key = (i64, datatype)

-- leaf: Indicates wether or not there's children to this node.
-- size: Number of keys in the node.
-- keys: List of tuple of key and data.
-- children: List of i64 indices to child nodes. A value of (-1) means no child.
-- parent: Index of parent node, -1 if root
--
-- Notes: The number of children (if any) are always equal to (size + 1)
type node = {
  leaf:     bool,
  parent:   ptr,
  size:     i64,
  keys:     [k]key,
  children: [c]ptr
}

-- Common predicates
def valid_node : (node -> bool) = (.size) >-> (<=) (0)
def valid_key  : (key  -> bool) = (.0)    >-> (!=) (-1)
def valid_ptr  : (ptr  -> bool) = (!=) #null

-- some constructors
def newchildarr () : *[c]ptr = replicate c #null
def newkeyarr   () : *[k]key = replicate k nil |> zip <| replicate k nilkey

def ptr_from_i64 (i: i64) : ptr =
  if i < 0 then #null else #ptr i


def node_new () : node =
  { leaf     = true
  , parent   = #null
  , size     = 0i64
  , keys     = newkeyarr ()
  , children = newchildarr ()
  }

-- Returns the rank (height) from a given node in the tree
-- with i=0 returns the rank of the whole tree.
def tree_rank [n] (tree : [n]node) (j : i64) : i64 =
  (.0) <| loop (i, r) = (0, tree[j]) while !r.leaf
    do let child = head r.children
       in match child
       case #ptr p -> (i + 1, tree[p])
       case #null  -> (i + 1, tree[0]) -- this case should never be reached
                                       -- also, this case results in an infinite
                                       -- loop on purpose.

-- Opaque to primitive type convertions
def ptrval (p : ptr) : i64 =
  match p
  case #null  -> (-1)
  case #ptr q -> q
