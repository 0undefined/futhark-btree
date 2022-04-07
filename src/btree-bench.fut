open import "btree"


-- Generates a simple (sorted) list of key-values
entry generic_kvpair (n: i64) : ([n]i64, [n]i64) =
  let ks = iota n
  in (ks, map (+10) ks)


entry generic_tree (n: i64) (k: i64) : ([]node, [k]i64) =
  let (keys,vals) = generic_kvpair n
  let ks = map (*4) (iota k)
  in
  (construct_tree_from_sorted_keyvals keys vals, ks)


-- The input size was generated with
--   tabulate 11 (\i -> 4**(2 + i))
-- ==
-- entry: bench_val_to_tree
-- script input { generic_kvpair           16i64 }
-- script input { generic_kvpair           64i64 }
-- script input { generic_kvpair          256i64 }
-- script input { generic_kvpair         1024i64 }
-- script input { generic_kvpair         4096i64 }
-- script input { generic_kvpair        16384i64 }
-- script input { generic_kvpair        65536i64 }
-- script input { generic_kvpair       262144i64 }
-- script input { generic_kvpair      1048576i64 }
-- script input { generic_kvpair      4194304i64 }
-- script input { generic_kvpair     16777216i64 }
entry bench_val_to_tree [n] (keys: [n]i64) (vals: [n]i64) : i64 =
  construct_tree_from_sorted_keyvals keys vals |> length

-- ==
-- entry: bench_search_naive2 bench_search_naive
-- script input { generic_tree 1024i64 2i64 }
-- script input { generic_tree 1024i64 16i64 }
-- script input { generic_tree 1024i64 256i64 }
-- script input { generic_tree 1024i64 512i64 }
-- script input { generic_tree 2048i64 256i64 }
-- script input { generic_tree 4096i64 256i64 }
-- script input { generic_tree 8192i64 256i64 }

entry bench_search_naive (t: []node) (k: []i64) =
  btree_search_naive t k

entry bench_search_naive2 (t: []node) (k: []i64) =
  btree_search_naive2 t k
