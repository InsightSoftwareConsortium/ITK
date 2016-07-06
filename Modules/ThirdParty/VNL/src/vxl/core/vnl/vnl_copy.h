// This is core/vnl/vnl_copy.h
#ifndef vnl_copy_h_
#define vnl_copy_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
#include "vnl/vnl_export.h"
//:
//   \file
//   \brief Easy conversion between vectors and matrices templated over different types.
//   \author fsm
//
// \verbatim
// Modifications
//   LSB (Manchester) 26/3/01 Tidied documentation
// \endverbatim

//: Easy conversion between vectors and matrices templated over different types.
// \relatesalso vnl_matrix
// \relatesalso vnl_vector
template <class S, class T> VNL_TEMPLATE_EXPORT
void vnl_copy(S const * const src, T *const dst, const unsigned n);


//: Easy conversion between vectors and matrices templated over different types.
// \relatesalso vnl_matrix
// \relatesalso vnl_vector
template <class S, class T> VNL_TEMPLATE_EXPORT
void vnl_copy(S const &, T &);

#endif // vnl_copy_h_
