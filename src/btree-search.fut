open import "types"


local def prime_pred (p: key -> bool) : (key -> bool) = (\k -> valid_key k && p k)


local def merge_immediate_results [n] (lhs: [n](i64,key)) (rhs: [n](i64,key)) : [n](i64,key) =
  -- idea, use scatter instead?
  map2 (\l r -> if l.0 != -1 then l else r) lhs rhs


local def key_to_search_result (k: key) : search_result =
  if valid_key k then #result k else #not_found


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

    -- Pick l if the value is larger, or the index is smaller, otherwise return r
    -- Consider l and right of (index,value)
    let cmp (l: (i64,i64)) (r:(i64,i64)) : (i64,i64) =
      if l.1 > r.1 then
        l
      else
        if l.0 < r.0
        then l
        else r

    -- just the keys, since they are the ones we're comparing
    let lkk = map (.0) lk
    let lkid = indices lkk

    -- map out which keys are <min and >max and get
    -- min' = the largest key-index of the keys that are <min
    -- max' = the smallest key-index of the keys that are >max
    let min' = map ((<min) >-> i64.bool) lkk |> zip lkid |> reduce_comm cmp (-1,i64.lowest) |> (.0)
    let max' = map ((>max) >-> i64.bool) lkk |> zip lkid |> reverse |> reduce_comm cmp (-1,i64.lowest) |> (.0) |> (-) fl |> (+) 1

    let contained = (min' < 0) && (fl < max')
    let localmin = (if contained then 0  else if min' <= 0  then (head lkid) else lkid[min'])
    let localmax = (if contained then fl else if max' >= fl then (last lkid) else lkid[max'])

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

    -- TODO: Optimization oportunity:
    -- * iteratively update `min` and `max` from the keys that are already found
    -- * use `next_imm` to map which children are desirable to traverse into
    -- The latter should be more desirable but much harder to implement

    let (localfrac_min,localrem_min) = (localmin / k, localmin % k)
    let (localfrac_max,localrem_max) = (localmax / k, localmax % k)
    let childmin = if contained then 0      else (localfrac_min * c + localrem_min)
    let childmax = if contained then (ll*c) else (localfrac_max * c + localrem_max)

    let children = (map (.children) layer |> flatten)[childmin:childmax]

    -- Filter the children
    let nl_flgs     = map (valid_ptr >-> i64.bool) children
    let nl_flgs_scn = scan (+) 0 nl_flgs |> map ((+)(-1))
    let nl_ptr_idx  = map2 (\f i -> if f == 1 then i else -1) nl_flgs nl_flgs_scn

    let nl_len = i64.sum nl_flgs
    --                                                    -- using -1, ballsy, i know, but i dont make mistakes, like any regular C-Programmer
    let nl_ptr : [nl_len]i64 = scatter (replicate nl_len (-1)) nl_ptr_idx (map ptrval children)
    in let nl = map (\i -> t[i]) nl_ptr

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

def btree_search_naive [n] [m] (t: [n]node) (ks: [m]i64) : [m]search_result =
  let kk = map (.keys) t |> flatten
  in map (\k ->
    let r = filter ((.0) >-> (==)k) kk
    in if length r > 0 then #result (head r)
    else #not_found
  ) ks

def btree_search_naive2 [n] [m] (t: [n]node) (ks: [m]i64) : [m]search_result =
  let result : [m]search_result = replicate m (#not_found)
  let layer = map (.keys) t |> flatten --- [d*k]

  let (idx,res) = map (\l -> map2 (\i k -> if l.0 == k then (i, l) else (-1,(-1,0))) (indices ks) ks) layer
                |> reduce_comm merge_immediate_results (replicate m (-1, (-1,0)))
                |> unzip
  let resres = map (\r -> if r.0 == -1 then #not_found else #result r) res
  in scatter (copy result) idx resres
