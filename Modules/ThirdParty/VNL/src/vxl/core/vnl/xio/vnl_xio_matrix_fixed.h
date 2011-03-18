// This is core/vnl/xio/vnl_xio_matrix_fixed.h
#ifndef vnl_xio_matrix_fixed_h
#define vnl_xio_matrix_fixed_h
//:
// \file
// \author Gamze D. Tunali
// \date 22-Dec-2005

#include <vnl/vnl_matrix_fixed.h>
#include <vcl_string.h>
#include <vcl_iosfwd.h>

//: XML save vnl_matrix_fixed to stream.
// \relatesalso vnl_matrix_fixed
template <class T, unsigned m, unsigned n>
void x_write(vcl_ostream & os, const vnl_matrix_fixed<T,m,n> & v,
             vcl_string name="vnl_matrix_fixed");

#endif // vnl_xio_matrix_fixed_h
