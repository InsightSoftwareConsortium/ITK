// This is core/vnl/xio/vnl_xio_vector.hxx
#ifndef vnl_xio_vector_hxx_
#define vnl_xio_vector_hxx_

#include "vnl_xio_vector.h"
#include <vsl/vsl_basic_xml_element.h>

//=================================================================================
template<class T>
void x_write(std::ostream & os, vnl_vector<T> const& v, std::string name)
{
  vsl_basic_xml_element element(name);
  element.add_attribute("size", v.size());
  for (unsigned i=0; i<v.size(); ++i)
    element.append_cdata(v.get(i));
  element.x_write(os);
}

//=================================================================================
template<class T>
void x_write_tree(std::ostream & os, vnl_vector<T> const& v, std::string name)
{
  vsl_basic_xml_element element(name);
  element.add_attribute("size", v.size());
  for (unsigned i=0; i<v.size(); ++i) {
    element.append_cdata("<element>");
    element.append_cdata(v.get(i));
    element.append_cdata("</element>");
  }
  element.x_write(os);
}

#undef VNL_XIO_VECTOR_INSTANTIATE
#define VNL_XIO_VECTOR_INSTANTIATE(T) \
template VNL_TEMPLATE_EXPORT void x_write(std::ostream &, vnl_vector<T > const&, std::string); \
template VNL_TEMPLATE_EXPORT void x_write_tree(std::ostream &, vnl_vector<T > const&, std::string)

#endif // vnl_xio_vector_hxx_
