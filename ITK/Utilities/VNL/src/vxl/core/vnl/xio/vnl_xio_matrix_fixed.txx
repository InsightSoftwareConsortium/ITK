// This is core/vnl/xio/vnl_xio_matrix_fixed.txx
#ifndef vnl_xio_matrix_fixed_txx_
#define vnl_xio_matrix_fixed_txx_
//:
// \file

#include "vnl_xio_matrix_fixed.h"
#include <vnl/vnl_matrix_fixed.h>
#include <vsl/vsl_basic_xml_element.h>

//=================================================================================
//: Binary save self to stream.
template<class T, unsigned m, unsigned n>
void x_write(vcl_ostream & os, const vnl_matrix_fixed<T,m,n> & p, vcl_string name)
{
  vsl_basic_xml_element element(name);
  element.add_attribute("rows", (int) p.rows());
  element.add_attribute("cols", (int) p.cols());
  for (unsigned int r=0; r<p.rows(); r++) {
    for (unsigned int c=0; c<p.rows(); c++) 
      element.append_cdata(p.get(r, c));
  } 
  element.x_write(os);
}

#define VNL_XIO_MATRIX_FIXED_INSTANTIATE(T,m,n) \
template void x_write(vcl_ostream &, const vnl_matrix_fixed<T,m,n > &, vcl_string name)

#endif // vnl_xio_matrix_fixed_txx_
