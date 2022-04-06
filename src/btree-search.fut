open import "types"


-- returns indices of `vals` in `set`
def get_idxs_of [n] [m] (vals: [m]i64) (set: [n]i64) : [n]i64 =
  let foo k : bool = any ((==)k) vals
  in map2 (\i k ->
    if foo k then i else -1
  ) (indices set) set


-- returns (node_index,key_index) of nodes containing keys, -1 in both indices
-- if not found
entry btree_search_nodes [n] [m] (t : [n]node) (keys : [m]i64) : ([m]i64,[m]i64) =
  let node_indices = replicate m (-1)
  let key_indices  = replicate m (-1)

  in if n == 0 then (node_indices, key_indices) else
  -- start from the root node
  let (result,_) =
    loop (res, aux) = ([], [head t])
    while !null aux do
      -- TODO: Actually use get_idxs_of to test wether or not we have found one
      let nl = map (.children) aux
              |> flatten
              |> filter valid_ptr
              |> map ptrval
              |> map (\i -> t[i])
       in ([],nl)
  in (node_indices,key_indices)
