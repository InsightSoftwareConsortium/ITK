#include <vcl_compiler.h>
#include "vcl_algorithm.txx"
#include "vcl_iterator.txx"
#include "vcl_vector.txx"

// --- Feature testing ---
#ifdef __STL_LOOP_INLINE_PROBLEMS
#define VCL_INSTANTIATE_INLINE_LOOP(f) template f ;
#else
#define VCL_INSTANTIATE_INLINE_LOOP(f) VCL_INSTANTIATE_INLINE(f)
#endif
