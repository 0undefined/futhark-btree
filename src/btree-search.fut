open import "types"


type search_result = #not_found | #result key

local def prime_pred (p: key -> bool) : (key -> bool) = (\k -> valid_key k && p k)

-- returns indices of `vals` in `set`
def get_idxs_of [n] [m] (vals: [m]i64) (set: [n]i64) : [n]i64 =
  let foo k : bool = any ((==)k) vals
  in map2 (\i k ->
    if foo k then i else -1
  ) (indices set) set


-- Filter-style searching using a predicate
def btree_filter [n] (p: key -> bool) (t : [n]node) : []key =

  if n == 0 then [] else
  -- start from the root node
  let (result,_) =
    loop (res, aux) = ([], [head t])
    while !null aux do
      -- TODO: Actually use get_idxs_of to test wether or not we have found one
      let lr = map (.keys) aux |> flatten |> filter (prime_pred p)
      in let nl = map (.children) aux
              |> flatten
              |> filter valid_ptr
              |> map ptrval
              |> map (\i -> t[i])
      in (res ++ lr,nl)
  in result

def btree_filter_naive (p: key -> bool) (t: []node) : []key =
  map (.keys) t
  |> flatten
  |> filter (prime_pred p)




-- returns a list of search results, each element corresponds to a search key in `ks`
--def btree_search_idx [n] [m] (t: [n]node) (ks: [m]i64) : [m]search_result =
--  let (result,_) = loop (result, layer) = (replicate m #not_found, [head t])
--  while length layer > 0 do
--    -- lookup ks in layer
--    -- fetch next layer
--    -- repeat
--    let lk = map (.keys) layer |> flatten
--    in let (imm_idx,imm) =
--      if !any (\k -> any ((.0) >-> (==)k) lk) ks then
--        ([],[])
--      else
--        let layeridx = map () lk : []i64 -- -1 or idx
--        let keyres = map () ks : [m]search_result
--        --let keysinlayer = map2 (\i k ->
--        --    if any ((==)(k.0)) ks then #result k else #not_found
--        --  ) (indices lk) lk -- -> idx|-1,res
--        --in map2 (\i k ->
--        --  map (\kk ->
--        --    match kk
--        --  ) keysinlayer
--        --) (indices ks) ks
--        in ([],[])
--        -- ([],[])
--    in let nl = map (.children) layer
--        |> flatten
--        |> filter valid_ptr
--        |> map ptrval
--        |> map (\i -> t[i])
--
--    in (scatter result imm_idx imm, nl)
--
--  in result
  --let tk = map (.keys) t |> flatten
  --let tkidx = (indices tk, tk)
  --in
  --  map (\t -> if any ((==)t) (map (.0) tk) then #result t else #not_found) ks



--
-- let n = 32i64
-- let n = 600i64
-- let kk = iota n
-- let vv = map (+2) kk
-- let tt = construct_tree_from_sorted_keyvals kk vv
-- let sk : []i64 = kk
-- let m = length sk
-- btree_search_idx_simple tt sk

-- keys: keys to search in
-- sk: search keys to find
--def search_node_keys (keys: [k]key) (sk: [m]i64) : [m]i64 =
--  map (\k)

def merge_immediate_results [n] (lhs: [n](i64,key)) (rhs: [n](i64,key)) : [n](i64,key) =
  -- idea, use scatter instead?
  map2 (\l r -> if l.0 != -1 then l else r) lhs rhs


def key_to_search_result (k: key) : search_result =
  if valid_key k then #result k else #not_found


def btree_search_idx [n] [m] (t: [n]node) (ks: [m]i64) : [m]search_result =
  -- Just use the smallest and largest key
  let min = head ks
  let max = last ks
  let (result,_) =
  loop ((imm:[m]search_result), (layer:[]node)) = (replicate m (#not_found), [head t])
  while length layer > 0 do
    -- "Layer-Length"
    let ll = length layer in
    -- "Flat-Length"
    let fl = ll * k in
    -- "Layer-Keys"
    let lk = map (.keys) layer |> flatten :> [fl]key --- [ll*k]key

    -- heavily type annotated for perservation of my own sanity
    in let (idx,res) : ([m]i64, [m]key) =
      -- incredible
      map (\(l: key) ->
          -- incredibly inefficient that is
          map2 (\(i:i64) (k:i64) ->
               if l.0 == k then (i, l) else (-1,(nilkey,0))
          ) (indices ks) ks
      ) lk -- : [fl][m](i64,key)
      |> reduce_comm merge_immediate_results (replicate m (-1, (nilkey,0)))
      |> unzip

    in let next_imm = scatter (copy imm) idx (map key_to_search_result res)
    -- TODO: use `next_imm` to map which children are desirable to traverse into

    -- is it worth it to have a condition here?
    in let nl = if (head layer).leaf then [] else map (.children) layer
        |> flatten
        |> filter valid_ptr
        |> map ptrval
        |> map (\i -> t[i])

    in (next_imm, nl)

  in result


def btree_search_idx_simple [n] [m] (t: [n]node) (ks: [m]i64) : [m]search_result =
  let (result,_) =
  loop ((imm:[m]search_result), (layer:[]node)) = (replicate m (#not_found), [head t])
  while length layer > 0 do
    -- "Layer-Length"
    let ll = length layer in
    -- "Flat-Length"
    let fl = ll * k in
    -- "Layer-Keys"
    let lk = map (.keys) layer |> flatten :> [fl]key --- [ll*k]key

    -- heavily type annotated for perservation of my own sanity
    in let (idx,res) : ([m]i64, [m]key) =
      map (\(l: key) ->
          map2 (\(i:i64) (k:i64) ->
               if l.0 == k then (i, l) else (-1,(nilkey,0))
          ) (indices ks) ks
      ) lk -- : [fl][m](i64,key)
      |> reduce_comm merge_immediate_results (replicate m (-1, (nilkey,0)))
      |> unzip

    in let resres = map key_to_search_result res

    -- TODO: Update this nonsense
    in let nl = map (.children) layer
        |> flatten
        |> filter valid_ptr
        |> map ptrval
        |> map (\i -> t[i])

    in (scatter (copy imm) idx resres, nl)

  in result


-- Naive implementations
-----------------------
-- Both works on all keys of the tree simultanously, with the difference being
-- btree_search_naive uses a filter whereas btree_search_naive2 uses a
-- combination of map and scatter

entry btree_search_naive [n] [m] (t: [n]node) (ks: [m]i64) : [m]search_result =
  let kk = map (.keys) t |> flatten
  in map (\k ->
    let r = filter ((.0) >-> (==)k) kk
    in if length r > 0 then #result (head r)
    else #not_found
  ) ks

entry btree_search_naive2 [n] [m] (t: [n]node) (ks: [m]i64) : [m]search_result =
  let result : [m]search_result = replicate m (#not_found)
  let layer = map (.keys) t |> flatten --- [d*k]

  let (idx,res) = map (\l -> map2 (\i k -> if l.0 == k then (i, l) else (-1,(-1,0))) (indices ks) ks) layer
                |> reduce_comm merge_immediate_results (replicate m (-1, (-1,0)))
                |> unzip
  let resres = map (\r -> if r.0 == -1 then #not_found else #result r) res
  in scatter (copy result) idx resres
