open import "btree"

entry generic_kvpair (n: i64) : ([n]i64, [n]i64) =
  let ks = iota n
  in (ks, map (+10) ks)

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
