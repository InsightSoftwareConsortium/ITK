// This is core/vnl/vnl_identity.h
#ifndef vnl_identity_h_
#define vnl_identity_h_
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

#include "vnl_unary_function.h"
#include "vnl/vnl_export.h"

template <class T>
class VNL_EXPORT vnl_identity : public vnl_unary_function<T,T>
{
 public:
  vnl_unary_function<T,T>* Copy() const override {
    vnl_identity<T>* copy = new vnl_identity<T>;
    *copy = *this;
    return copy;
  }

  T f(T const& x) override {
    return x;
  }
};

#endif // vnl_identity_h_
