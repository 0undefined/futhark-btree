open import "../lib/github.com/diku-dk/sorts/radix_sort"
open import "types"
open import "btree-misc"
open import "btree-ops"
open import "btree-construct"
open import "btree-build"     --- TODO: delete this, likely
open import "btree-search"


entry construct_tree_from_sorted_keyvals [n] (ks : [n]i64) (vs: [n]datatype) : []node =
  let tree_params = analyze n
  in construct ks vs tree_params


entry construct_tree_from_keyvals [n] (ks : [n]i64) (vs: [n]datatype) : []node =
   let (ks', vs') =
     zip ks vs
     |> radix_sort_by_key (.0) (i64.num_bits) (i64.get_bit)
     |> unzip
  in construct_tree_from_sorted_keyvals ks' vs'


entry main [n] (keys: [n]i64) (vals: [n]datatype)
             : ([]bool, []i64, []i64, [n]i64, [n]datatype, []i64) =

   let tree = construct_tree_from_keyvals keys vals
   in tree_to_flat_representation tree
