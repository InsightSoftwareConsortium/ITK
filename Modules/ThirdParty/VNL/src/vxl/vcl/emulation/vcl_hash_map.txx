// -*- c++ -*-
#ifndef vcl_emulation_hash_map_txx_
#define vcl_emulation_hash_map_txx_

#include <vcl_compiler.h>
#include "vcl_hash_map.h"
#include "vcl_hash.txx"
#include "vcl_pair.txx"
#include "vcl_functional.txx"

/////////////////////////////////////////////////////////////////////////////

// --- Hash Map ---

// * You can't call VCL_HASH_MAP_INSTANTIATE twice from within the same macro
// as the __LINE__ will be the same.

// This macro ensures that the type is usable as a key for a hash map.
#define VCL_HASHKEY_INSTANTIATE(Key) \
template struct vcl_equal_to<Key >

// Then this macro may be used to instantiate the specific hash_maps.
#undef VCL_HASH_MAP_INSTANTIATE
#define VCL_HASH_MAP_INSTANTIATE(Key, Value, Hash, Comp) \
template class vcl_hash_map<Key, Value, Hash, Comp VCL_DFL_TMPL_ARG(vcl_alloc) >; \
template class vcl_hash_multimap<Key, Value, Hash, Comp VCL_DFL_TMPL_ARG(vcl_alloc) >; \
VCL_HASHTABLE_MAP_PAIR_INSTANTIATE(Key, Value, Hash, Comp, __LINE__)

// This macro uses the TAG to generate unique (within this compilation
// unit) typedef names.
#define VCL_HASH_MAP_TAG_INSTANTIATE(Key, Value, Hash, Comp, TAG) \
template class vcl_hash_map<Key, Value, Hash, Comp VCL_DFL_TMPL_ARG(vcl_alloc) >; \
template class vcl_hash_multimap<Key, Value, Hash, Comp VCL_DFL_TMPL_ARG(vcl_alloc) >; \
VCL_HASHTABLE_MAP_PAIR_INSTANTIATE(Key, Value, Hash, Comp, TAG)

// This "identity" passthru gets __LINE__ expanded
#define VCL_HASHTABLE_MAP_PAIR_INSTANTIATE(Key, Value, Hash, Comp, TAG) \
VCL_HASHTABLE_MAP_PAIR_INSTANTIATEx(Key, Value, Hash, Comp, TAG)

// Here is where the pair for the hash_map is instantiated.
#define VCL_HASHTABLE_MAP_PAIR_INSTANTIATEx(Key, Value, Hash, Comp, TAG) \
typedef vcl_pair<const Key, Value > HTPairc ## TAG; \
VCL_HASHTABLE_PAIR_INSTANTIATE(Key, HTPairc ## TAG, Hash, Comp, TAG)

// And here is where the hashtable of key/value gets instantiated.
#define VCL_HASHTABLE_PAIR_INSTANTIATE(T, HTPair, Hash, Comp, TAG) \
VCL_HASHTABLE_INSTANTIATE(T, HTPair, vcl_select1st<HTPair >, Hash, Comp, TAG)

/////////////////////////////////////////////////////////////////////////////


// --- HASH TABLE ---
#define VCL_HASHTABLE_INSTANTIATE(Key, Value, GetKey, Hash, Compare, TAG) \
template class vcl_hashtable<Value, Key, Hash, GetKey, Compare, vcl_alloc >; \
template class vcl_hashtable_base< Value, vcl_alloc >; \
template struct vcl_hashtable_iterator<Value, Key, Hash, GetKey, Compare, vcl_alloc >; \
typedef vcl_hashtable_iterator<Value, Key, Hash, GetKey, Compare, vcl_alloc > HTIter ## TAG; \
VCL_UNARY_INSTANTIATE(Hash); \
VCL_UNARY_INSTANTIATE(Compare); \
VCL_UNARY_INSTANTIATE(GetKey); \
VCL_ITER_FWD_INSTANTIATE(HTIter ## TAG); \
VCL_PAIR_INSTANTIATE(HTIter ## TAG, bool); \
VCL_PAIR_INSTANTIATE(HTIter ## TAG, HTIter ## TAG); \
template struct vcl_hashtable_const_iterator<Value, Key, Hash, GetKey, Compare, vcl_alloc >; \
typedef vcl_hashtable_const_iterator<Value, Key, Hash, GetKey, Compare, vcl_alloc > HTIterc ## TAG; \
VCL_ITER_FWD_INSTANTIATE(HTIterc ## TAG); \
VCL_INSTANTIATE_INLINE(void vcl_distance(HTIterc ## TAG, HTIterc ## TAG, vcl_size_t&)); \
VCL_INSTANTIATE_INLINE(void __distance(HTIterc ## TAG, HTIterc ## TAG const &, vcl_size_t &, vcl_forward_iterator_tag)); \
VCL_PAIR_INSTANTIATE(HTIterc ## TAG, bool); \
VCL_PAIR_INSTANTIATE(HTIterc ## TAG, HTIterc ## TAG); \
template struct vcl_hashtable_node<Value >; \
typedef vcl_hashtable_node<Value > HTNode ## TAG; \
typedef vcl_hashtable_node<Value >* HTNodeP ## TAG; \
VCL_UNARY_INSTANTIATE(HTNodeP ## TAG); \
VCL_UNARY_INSTANTIATE(HTNodeP ## TAG *); \
VCL_CONTAINABLE_INSTANTIATE(HTNodeP ## TAG); \
VCL_VECTOR_INSTANTIATE(HTNodeP ## TAG)

#endif // vcl_emulation_hash_map_txx_
