open import "btree"


local def testing_tree (n: i64) : []node =
  let ks = iota n
  let vs = map (*2) ks
  in construct_tree_from_sorted_keyvals ks vs


-- Test that a tree preserves the properties of a B-Tree
local def valid_btree [n] (t : [n]node) : bool =
  -- test wether sizes are representative of the actual number of keys
  let test_attributes (m : node) : bool =
    -- Do `size` represent the actual number of keys?
    let sizetest  = map ((.0) >-> (!=)(-1) >-> i64.bool) m.keys |> i64.sum |> (==) m.size

    -- Check if number of children is actually equal to .size + 1
    let numchilds = filter valid_ptr m.children |> length
    let childtest = if m.leaf then numchilds == 0 else numchilds == m.size + 1

    in childtest && sizetest


  -- Test wether sizes actual respects the constraints of a B-Tree
  let test_properties (m : node) : bool = m.size >= degree - 1 && m.size <= k

  in if n == 0 then true else

  let root_valid = test_attributes (head t) in
  if n == 1 then
    root_valid
  else
    let firstlayer   = filter valid_ptr (head t).children in
    let (test_result,_) =
      loop (valid, layer) =
        ( root_valid
        , firstlayer
        )
      while (valid && length layer > 0) do
        -- we should be guaranteed to never have an OOB index by how we construct the layer
        let layer_nodes = map (\p -> let i = ptrval p in t[i]) layer
        let next_layer  = map (.children) layer_nodes |> flatten |> filter valid_ptr

        in
        ( valid && all (\m -> test_attributes m && test_properties m) layer_nodes
        , next_layer
        )
    in test_result


-- Test B-Tree construction from list of values, B-Tree property preservation.
-- ==
-- entry:btree_construction_property_preservation
-- input {0i64}    output {true}
-- input {1i64}    output {true}
-- input {10i64}   output {true}
-- input {50i64}   output {true}
-- input {2048i64} output {true}

-- Be careful on the inputs! sizes of `n` that produces invalid btrees from the
-- analyze step causes invalid b-trees to be produced.
entry btree_construction_property_preservation (i : i64) : bool =
  valid_btree (testing_tree i)

-- Test that all keys are still retrieveable after construction
-- ==
-- entry:btree_construction_value_preservation
-- input {1i64}    output {true}
-- input {10i64}   output {true}
-- input {50i64}   output {true}
-- input {2048i64} output {true}
entry btree_construction_value_preservation (n : i64) : bool =
  -- make the indexing a little more interesting than a plain `iota`, but still
  -- make them sorted
  let ks = iota n |> map (*3)
  let vs = map (+2) ks
  let tt = construct_tree_from_sorted_keyvals ks vs
  let sr : [n]i64 = btree_search_idx tt ks |> map (\r ->
    match r
    case #not_found -> nilkey
    case #result r  -> r.0
  )
  -- TODO: Make an interesting case
  in map2 (==) sr ks |> all id
  --
