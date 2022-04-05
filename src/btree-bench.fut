-- ==
-- entry: bench_val_to_nodelist
-- random input {[100]i64      [100]i64}      auto output
-- random input {[1000]i64     [1000]i64}     auto output
-- random input {[10000]i64    [10000]i64}    auto output
-- random input {[100000]i64   [100000]i64}   auto output
-- random input {[1000000]i64  [1000000]i64}  auto output
-- random input {[10000000]i64 [10000000]i64} auto output
open import "btree"

entry bench_val_to_nodelist [n] (keys: [n]i64) (vals: [n]datatype) : i64 =
  node_list_from_keyvalues keys vals |> length

--open import "src/btree"
--let nn = 87i64
--let kk = iota nn
--let vv = map (+10) kk
--let tt = construct_tree_from_keyvals (-1) kk vv

entry generic_kvpair (n: i64) : ([n]i64, [n]i64) =
  let ks = iota n
  in (ks, map (+10) ks)

-- ==
-- entry: bench_val_to_tree
-- script input { generic_kvpair           10i64 }
-- script input { generic_kvpair          100i64 }
-- script input { generic_kvpair         1000i64 }
-- script input { generic_kvpair        10000i64 }
-- script input { generic_kvpair       100000i64 }
-- script input { generic_kvpair      1000000i64 }
-- script input { generic_kvpair     10000000i64 }
entry bench_val_to_tree [n] (keys: [n]i64) (vals: [n]i64) : i64 =
  construct_tree_from_sorted_keyvals keys vals |> length
