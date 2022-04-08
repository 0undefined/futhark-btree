open import "btree"


-- Generates a simple (sorted) list of key-values
entry generic_kvpair (n: i64) : ([n]i64, [n]i64) =
  let ks = iota n
  in (ks, map (+10) ks)


entry generic_tree (n: i64) (k: i64) : ([]node, [k]i64) =
  let (keys,vals) = generic_kvpair n
  let ks = map (*4) (iota k) |> map (+((n/2)-(k/2)))
  in
  (construct_tree_from_sorted_keyvals keys vals, ks)

entry generic_tree_with_keyspace_outside (n: i64) (k: i64) : ([]node, [k]i64) =
  let (tt,ks) = generic_tree n k
  in (tt, map (+n) ks)

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

-- Compare speed with different search-key sizes
-- ==
-- entry: bench_search_basic bench_search_opt bench_search_naive
-- script input { generic_tree     8192i64    2i64 }
-- script input { generic_tree     8192i64   16i64 }
-- script input { generic_tree     8192i64   64i64 }
-- script input { generic_tree     8192i64  128i64 }
-- script input { generic_tree     8192i64  256i64 }
-- script input { generic_tree     8192i64  512i64 }
-- script input { generic_tree     8192i64 1024i64 }
-- script input { generic_tree     8192i64 1536i64 }
-- script input { generic_tree     8192i64 2048i64 }

-- Compare speed with different tree sizes
-- ==
-- entry: bench_search_basic bench_search_opt bench_search_naive
-- script input { generic_tree     1024i64  512i64 }
-- script input { generic_tree     4096i64  512i64 }
-- script input { generic_tree    16384i64  512i64 }
-- script input { generic_tree    65536i64  512i64 }
-- script input { generic_tree   262144i64  512i64 }
-- script input { generic_tree  1048576i64  512i64 }
-- script input { generic_tree  4194304i64  512i64 }
-- script input { generic_tree 16777216i64  512i64 }

entry bench_search_basic (t: []node) (k: []i64) =
  btree_search_idx_simple t k

entry bench_search_opt (t: []node) (k: []i64) =
  btree_search_idx t k

entry bench_search_naive (t: []node) (k: []i64) =
  btree_search_naive t k


entry bench_search_outside_opt (t: []node) (k: []i64) =
  btree_search_idx t k

entry bench_search_outside_basic (t: []node) (k: []i64) =
  btree_search_idx_simple t k
