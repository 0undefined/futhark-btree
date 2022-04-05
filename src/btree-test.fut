open import "btree"


def testing_nodelist (n : i64) : []node =
  let ks = iota n
  let vs = map (*2) ks
  in node_list_from_keyvalues ks vs


def testing_tree (n: i64) : []node =
  let ks = iota n
  let vs = map (*2) ks
  in construct_tree_from_sorted_keyvals ks vs



-- Test node-list creation from array of values, size preservation
-- ==
-- entry:node_list_size_preservation
-- input {0i64}    output {0i64}
-- input {1i64}    output {1i64}
-- input {10i64}   output {10i64}
-- input {50i64}   output {50i64}
-- input {100i64}  output {100i64}
-- input {1024i64} output {1024i64}
entry node_list_size_preservation (i : i64) : i64 =
  map (.keys) (testing_nodelist i) |> flatten |> map (.0) |> filter ((!=)nilkey) |> length


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
    filter ((!=)nil) :> ([i]i64) -- required as the compiler doesn't know that
                                 -- our implementation works correctly
                                 -- do _we_ even know it works correctly?
  in all (\(r,t) -> r==t) (zip res test)


-- Test that a tree preserves the properties of a B-Tree
def valid_nodes [n] (t : [n]node) : bool =
  -- test wether sizes are representative of the actual number of keys
  let test_attributes (m : node) : bool =
    let res =
      (map ((.0) >-> (!=)(-1) >-> i64.bool) m.keys |> i64.sum) == m.size

    in if m.leaf then res else
      -- Check if number of children is actually equal to .size + 1
      res && (map ((==)#null >-> i64.bool) m.children |> i64.sum) == (m.size + 1)

  -- Test wether sizes actual respects the constraints
  let test_constraints (m : node) : bool = m.size >= degree - 1 && m.size <= k

  in if n == 0 then true else

  let root_valid = test_attributes (head t) in
  if n == 1 then
    root_valid
  else
    let scratchlayer = replicate (n-1) #null in
    let firstlayer = filter ((!=)#null) (head t).children in
    (.0) <| loop (valid, layer, layer_sz) = (
      root_valid,
      scatter scratchlayer (indices firstlayer) firstlayer,
      length firstlayer)
    while (valid && layer_sz > 0) do
      -- we should be guaranteed to never have an OOB index by how we construct the layer
      let layer_nodes = map (\p -> let i = ptrval p in t[i]) (take layer_sz layer)
      let children    = map (.children) layer_nodes |> flatten |> filter ((!=)#null)
      let next_layer  = scatter (copy layer) (indices children) children

      in
      ( all (\m -> test_attributes m && test_constraints m) layer_nodes
      , next_layer
      , length next_layer)


-- Test node-list creation from array of values, B-Tree property preservation
-- ==
-- entry:btree_construction_property_preservation
-- input {0i64}    output {true}
-- input {1i64}    output {true}
-- input {10i64}   output {true}
-- input {50i64}   output {true}
-- input {100i64}  output {true}
-- input {1024i64} output {true}
entry btree_construction_property_preservation (i : i64) : bool =
  valid_nodes (testing_nodelist i)
