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
