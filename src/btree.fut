let degree : i64 = 128
let nvals  : i64 = degree * 2 - 1
let nkeys  : i64 = degree * 2

type node [k] [c] 'v =
   #node {size: i64, keys: [k]i64, vals: [k]v, children: [c]i64}
 | #leaf {size: i64, keys: [k]i64, vals: [k]v}


-- The number of elements containable in a tree is (2*t)^h - 1
let tree_size (degree: i64) (height: i64) =
  degree |> (*2) |> (**height) |> (+) (-1)


let node_new (degree: i64) =
  let numkeys = degree * 2 - 1
  let keys    = iota numkeys
  in #leaf {size=0i64, keys=keys, vals=replicate numkeys 0i64} -- :> node [numkeys] [numkeys] i64


let construct_tree_from_keyvals 'a [n]
  (keys: [n]i64)
  (vals: [n]a) : [](node [0] [0] a) =
    let root : node = node_new degree
  in root
