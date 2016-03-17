// This is core/vnl/xio/vnl_xio_matrix.hxx
#ifndef vnl_xio_matrix_hxx_
#define vnl_xio_matrix_hxx_

#include "vnl_xio_matrix.h"
#include <vnl/vnl_matrix.h>
#include <vsl/vsl_basic_xml_element.h>

//=================================================================================
template<class T>
void x_write(vcl_ostream & os, vnl_matrix<T> const& M, vcl_string name)
{
  vsl_basic_xml_element element(name);
  element.add_attribute("rows", (int) M.rows());
  element.add_attribute("cols", (int) M.cols());
  for (unsigned int r=0; r<M.rows(); ++r) {
    for (unsigned int c=0; c<M.cols(); ++c)
      element.append_cdata(M.get(r,c));
  }
  element.x_write(os);
}

//=================================================================================
template<class T>
void x_write_tree(vcl_ostream & os, vnl_matrix<T> const& M, vcl_string name)
{
  vsl_basic_xml_element element(name);
  element.add_attribute("rows", (int) M.rows());
  element.add_attribute("cols", (int) M.cols());
  for (unsigned int r=0; r<M.rows(); ++r) {
    element.append_cdata("<row>");
    for (unsigned int c=0; c<M.cols(); ++c) {
      element.append_cdata("<cell>");
      element.append_cdata(M.get(r,c));
      element.append_cdata("</cell>");
    }
    element.append_cdata("</row>");
  }
  element.x_write(os);
}

#undef VNL_XIO_MATRIX_INSTANTIATE
#define VNL_XIO_MATRIX_INSTANTIATE(T) \
template void x_write(vcl_ostream &, vnl_matrix<T > const&, vcl_string); \
template void x_write_tree(vcl_ostream &, vnl_matrix<T > const&, vcl_string)

#endif // vnl_xio_matrix_hxx_
