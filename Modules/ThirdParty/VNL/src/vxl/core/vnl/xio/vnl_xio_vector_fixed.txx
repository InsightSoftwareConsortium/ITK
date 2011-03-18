// This is core/vnl/xio/vnl_xio_vector_fixed.txx
#ifndef vnl_xio_vector_fixed_txx_
#define vnl_xio_vector_fixed_txx_
//:
// \file

#include "vnl_xio_vector_fixed.h"
#include <vsl/vsl_basic_xml_element.h>

//=================================================================================
//: XML save self to stream.
template<class T, unsigned int n>
void x_write(vcl_ostream & os, const vnl_vector_fixed<T,n> & p, vcl_string name)
{
  vsl_basic_xml_element element(name);
  element.add_attribute("size", (int) p.size());
  for (unsigned i=0; i<p.size(); i++)
    element.append_cdata(p.get(i));
  element.x_write(os);  
}

#define VNL_XIO_VECTOR_FIXED_INSTANTIATE(T,n) \
template void x_write(vcl_ostream &, const vnl_vector_fixed<T,n > &, vcl_string)

#endif // vnl_xio_vector_fixed_txx_
