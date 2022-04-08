open import "types"


type search_result = #not_found | #result key

local def prime_pred (p: key -> bool) : (key -> bool) = (\k -> valid_key k && p k)

def searchres_to_id (sr: search_result) : i64 =
  match sr
  case #not_found -> nilkey
  case #result r  -> r.0

-- returns indices of `vals` in `set`
def get_idxs_of [n] [m] (vals: [m]i64) (set: [n]i64) : [n]i64 =
  let foo k : bool = any ((==)k) vals
  in map2 (\i k ->
    if foo k then i else -1
  ) (indices set) set


-- Filter-style searching using a predicate
def btree_filter_dumb [n] (p: key -> bool) (t : [n]node) : []key =
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


local def merge_immediate_results [n] (lhs: [n](i64,key)) (rhs: [n](i64,key)) : [n](i64,key) =
  -- idea, use scatter instead?
  map2 (\l r -> if l.0 != -1 then l else r) lhs rhs


local def key_to_search_result (k: key) : search_result =
  if valid_key k then #result k else #not_found


def btree_search_idx [n] [m] (t: [n]node) (ks: [m]i64) : [m]search_result =
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

    -- "Filtered-Layer-Keys"
    let flk = filter ((.1) >-> valid_key) (zip (indices lk) lk)
    -- "Filtered-Layer-Keys-Ids"
    let lki = map (\(i,k) -> (i,k.0))  flk
    -- "Numberof-Filtered-Layer-Keys"
    let nflk = length flk

    -- Assume keys in the tree are sorted
    -- Get the largest key smaller than `min`
    let min' = (map ((.1) >-> (<min) >-> i64.bool) lki |> i64.sum |> (+) (-1))
    -- Get the smallest key larger than `max`
    let max' = (map ((.1) >-> (>max) >-> i64.bool) lki |> i64.sum |> (-) nflk |> (+) 1)



    let contained = (min' < 0) && (nflk < max')
    let localmin = (if contained then 0  else if min' <= 0   then (head lki).0 else lki[min'].0)
    let localmax = (
      if contained then
        fl else
      if max' >= nflk then
        (last lki).0
      else lki[max'].0
    )

    let subslice = lk[localmin:localmax]

    -- heavily type annotated for perservation of my own sanity
    in let (idx,res) : ([m]i64, [m]key) =
      -- incredible
      map (\(l: key) ->
          -- incredibly inefficient that is
          map2 (\(i:i64) (k:i64) ->
               if l.0 == k then (i, l) else (-1,(nilkey,0))
          ) (indices ks) ks
      ) subslice -- : [fl][m](i64,key)
      |> reduce_comm merge_immediate_results (replicate m (-1, (nilkey,0)))
      |> unzip

    in let next_imm = scatter (copy imm) idx (map key_to_search_result res)
    -- TODO: use `next_imm` to map which children are desirable to traverse into

    let children = map (.children) layer |> flatten
    let childmin = if contained then 0      else ((localmin / k) * c + (localmin % k)) -- TODO: Fancy calculation
    let childmax = if contained then (ll*c) else ((localmax / k) * c + (localmax % k))

    in let nl = filter valid_ptr children[childmin:childmax]
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
