// This is core/vnl/xio/vnl_xio_matrix_fixed.hxx
#ifndef vnl_xio_matrix_fixed_hxx_
#define vnl_xio_matrix_fixed_hxx_

#include "vnl_xio_matrix_fixed.h"
#include <vnl/vnl_matrix_fixed.h>
#include <vsl/vsl_basic_xml_element.h>

//=================================================================================
template<class T, unsigned m, unsigned n>
void x_write(std::ostream & os, vnl_matrix_fixed<T,m,n> const& M, std::string name)
{
  vsl_basic_xml_element element(name);
  element.add_attribute("rows", (int) m);
  element.add_attribute("cols", (int) n);
  for (unsigned int r=0; r<m; ++r) {
    for (unsigned int c=0; c<n; ++c)
      element.append_cdata(M.get(r,c));
  }
  element.x_write(os);
}

//=================================================================================
template<class T, unsigned m, unsigned n>
void x_write_tree(std::ostream & os, vnl_matrix_fixed<T,m,n> const& M, std::string name)
{
  vsl_basic_xml_element element(name);
  element.add_attribute("rows", (int) m);
  element.add_attribute("cols", (int) n);
  for (unsigned int r=0; r<m; ++r) {
    element.append_cdata("<row>");
    for (unsigned int c=0; c<n; ++c) {
      element.append_cdata("<cell>");
      element.append_cdata(M.get(r,c));
      element.append_cdata("</cell>");
    }
    element.append_cdata("</row>");
  }
  element.x_write(os);
}

#undef VNL_XIO_MATRIX_FIXED_INSTANTIATE
#define VNL_XIO_MATRIX_FIXED_INSTANTIATE(T,m,n) \
template void x_write(std::ostream &, vnl_matrix_fixed<T,m,n > const&, std::string); \
template void x_write_tree(std::ostream &, vnl_matrix_fixed<T,m,n > const&, std::string)

#endif // vnl_xio_matrix_fixed_hxx_
