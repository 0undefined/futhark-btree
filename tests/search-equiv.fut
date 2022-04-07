open import "../src/btree"

-- All search functions should return the same
-- ==
-- tags {slow}
-- input {1i64}    output {true}
-- input {10i64}   output {true}
-- input {50i64}   output {true}
-- input {600i64}  output {true}
-- input {2048i64} output {true}
-- input {4000i64} output {true}
entry main (n : i64) : bool =
  -- make the indexing a little more interesting than a plain `iota`, but still
  -- make them sorted
  let ks = iota n |> map (*2)
  let vs = map (+2) ks
  let tt = construct_tree_from_sorted_keyvals ks vs
  let sr_naive  : [n]i64 = btree_search_naive      tt ks |> map searchres_to_id
  let sr_naive2 : [n]i64 = btree_search_naive2     tt ks |> map searchres_to_id
  let sr_simple : [n]i64 = btree_search_idx_simple tt ks |> map searchres_to_id
  let sr        : [n]i64 = btree_search_idx        tt ks |> map searchres_to_id
  -- TODO: Make an interesting case
  in map4 (\a b c d -> a == b && b == c && c == d) sr_naive sr_naive2 sr_simple sr
     |> all id
