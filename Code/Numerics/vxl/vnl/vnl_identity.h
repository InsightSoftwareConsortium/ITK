#ifndef vnl_identity_h_
#define vnl_identity_h_
#ifdef __GNUC__
#pragma interface
#endif
//
// .NAME	vnl_identity
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_identity.h
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 07 Dec 98
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_unary_function.h>

template <class T>
class vnl_identity : public vnl_unary_function<T,T> {
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
