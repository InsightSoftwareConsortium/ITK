#ifndef vnl_vector_dereference_h_
#define vnl_vector_dereference_h_
#ifdef __GNUC__
#pragma interface
#endif
//
// .NAME	vnl_vector_dereference - Undocumented class FIXME
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_vector_dereference.h
// .FILE	vnl_vector_dereference.cxx
//
// .SECTION Description
//    vnl_vector_dereference is a class that awf hasn't documented properly. FIXME
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 28 Nov 98
//
//-----------------------------------------------------------------------------


#include <vcl_vector.h>

template <class T>
class vnl_vector_dereference : public vnl_unary_function<T, int> {
  vcl_vector<T> v_;
public:

  vnl_vector_dereference(const vcl_vector<T>& v):
    v_(v) {
  }

  T f(const int& i) {
    return v_[i];
  }
};

#endif // vnl_vector_dereference_h_
