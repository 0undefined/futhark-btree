open import "../lib/github.com/diku-dk/sorts/radix_sort"
let degree : i64 = 128
let v : i64 = degree * 2 - 1
let k : i64 = degree * 2

type node 't = {size: i64, keys: [v]i64, vals: [v]t, children: [k]i64}


-- The number of elements containable in a tree is (2*t)^h - 1
let tree_size (degree: i64) (height: i64) = (degree * 2)**height |> (+) (-1)


let node_new 'a (nil: a) : node a =
  let keys    = iota v
  in {size=0i64, keys=keys, vals=replicate v nil, children=replicate k (-1)}


def construct_tree_from_keyvals 'a [n] (nil: a) (keys: [n]i64) (vals: [n]a) : [](node a) =
    let root = node_new nil
  in [root]


entry main [n] (keys: [n]i64) (vals: [n]i64) : []i64 =
  []
