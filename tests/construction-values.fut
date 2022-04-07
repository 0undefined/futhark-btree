open import "../src/btree"

-- Test that all keys are still retrieveable after construction
-- ==
-- input {1i64}   output {true}
-- input {10i64}  output {true}
-- input {50i64}  output {true}
-- input {600i64} output {true}
entry main (n : i64) : bool =
  -- make the indexing a little more interesting than a plain `iota`, but still
  -- make them sorted
  let ks = iota n |> map (*3)
  let vs = map (+2) ks
  let tt = construct_tree_from_sorted_keyvals ks vs
  let sr : [n]i64 = btree_search_naive tt ks |> map searchres_to_id
  -- TODO: Make an interesting case
  in map2 (==) sr ks |> all id
