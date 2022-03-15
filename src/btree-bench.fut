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
  node_list_from_keyvalues (-1) keys vals |> length

--open import "src/btree"
--let nn = 87i64
--let kk = iota nn
--let vv = map (+10) kk
--let tt = construct_tree_from_keyvals (-1) kk vv
