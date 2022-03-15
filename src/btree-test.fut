open import "btree"


def testing_nodelist (n : i64) : []node =
  let kk = iota n
  let vv = map (*2) kk
  in node_list_from_keyvalues (-1) kk vv


-- Test node-list creation from array of values, size preservation
-- ==
-- entry:node_list_size_preservation
-- input {0i64}    output {true}
-- input {1i64}    output {true}
-- input {10i64}   output {true}
-- input {50i64}   output {true}
-- input {100i64}  output {true}
-- input {1024i64} output {true}
entry node_list_size_preservation (i : i64) : bool =
  map (.keys) (testing_nodelist i) |> flatten |> map (.0) |> filter ((!=)(-1)) |> length |> (==) i


-- Test node-list creation from array of values, value preservation
-- ==
-- entry:node_list_value_preservation
-- input {0i64}    output {true}
-- input {1i64}    output {true}
-- input {10i64}   output {true}
-- input {50i64}   output {true}
-- input {100i64}  output {true}
-- input {1024i64} output {true}
entry node_list_value_preservation (i : i64) : bool =
  -- Rely on the fact that testing_nodelist produces a value list following 2*i
  let res = tabulate i (*2i64) in
  let test = map (.keys) (testing_nodelist i) |>
    flatten |>
    map (.1) |>
    filter ((!=)(-1)) :> ([i]i64) -- required as the compiler doesn't know that our implementation works correctly
                                  -- do _we_ even know it works correctly?
  in map2 (==) res test |> reduce_comm (&&) true
