// This is core/vnl/vnl_identity.h
#ifndef vnl_identity_h_
#define vnl_identity_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Contains class vnl_identity
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   07 Dec 98
//
// \verbatim
//  Modifications
//   LSB (Manchester) 23/1/01 Tidied documentation
// \endverbatim
//-----------------------------------------------------------------------------

#include <vnl/vnl_unary_function.h>

template <class T>
class vnl_identity : public vnl_unary_function<T,T>
{
 public:
  vnl_unary_function<T,T>* Copy() const {
    vnl_identity<T>* copy = new vnl_identity<T>;
    *copy = *this;
    return copy;
  }

  T f(T const& x) {
    return x;
  }
};

#endif // vnl_identity_h_
