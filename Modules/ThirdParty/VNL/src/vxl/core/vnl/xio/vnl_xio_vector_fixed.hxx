// This is core/vnl/xio/vnl_xio_vector_fixed.hxx
#ifndef vnl_xio_vector_fixed_hxx_
#define vnl_xio_vector_fixed_hxx_

#include "vnl_xio_vector_fixed.h"
#include <vsl/vsl_basic_xml_element.h>

//=================================================================================
template<class T, unsigned int n>
void x_write(vcl_ostream & os, vnl_vector_fixed<T,n> const& v, vcl_string name)
{
  vsl_basic_xml_element element(name);
  element.add_attribute("size", (int) n);
  for (unsigned i=0; i<n; i++)
    element.append_cdata(v.get(i));
  element.x_write(os);
}

//=================================================================================
template<class T, unsigned int n>
void x_write_tree(vcl_ostream & os, vnl_vector_fixed<T,n> const& v, vcl_string name)
{
  vsl_basic_xml_element element(name);
  element.add_attribute("size", (int) n);
  for (unsigned i=0; i<n; i++) {
    element.append_cdata("<element>");
    element.append_cdata(v.get(i));
    element.append_cdata("</element>");
  }
  element.x_write(os);
}

#undef VNL_XIO_VECTOR_FIXED_INSTANTIATE
#define VNL_XIO_VECTOR_FIXED_INSTANTIATE(T,n) \
template void x_write(vcl_ostream &, vnl_vector_fixed<T,n > const&, vcl_string); \
template void x_write_tree(vcl_ostream &, vnl_vector_fixed<T,n > const&, vcl_string)

#endif // vnl_xio_vector_fixed_hxx_
