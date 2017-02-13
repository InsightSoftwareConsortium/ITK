// This is core/vnl/xio/vnl_xio_quaternion.hxx
#ifndef vnl_xio_quaternion_hxx_
#define vnl_xio_quaternion_hxx_

#include "vnl_xio_quaternion.h"
#include <vnl/vnl_quaternion.h>
#include <vsl/vsl_basic_xml_element.h>

//=================================================================================
template<class T>
void x_write(std::ostream & os, vnl_quaternion<T> const& q, std::string name)
{
  vsl_basic_xml_element element(name);
  element.add_attribute("x", q.x());
  element.add_attribute("y", q.y());
  element.add_attribute("z", q.z());
  element.add_attribute("r", q.r());
  element.x_write(os);
}

//=================================================================================
template<class T>
void x_write_tree(std::ostream & os, vnl_quaternion<T> const& q, std::string name)
{
  vsl_basic_xml_element element(name);
  element.append_cdata("<x>"); element.append_cdata(q.x()); element.append_cdata("</x>");
  element.append_cdata("<y>"); element.append_cdata(q.y()); element.append_cdata("</y>");
  element.append_cdata("<z>"); element.append_cdata(q.z()); element.append_cdata("</z>");
  element.append_cdata("<r>"); element.append_cdata(q.r()); element.append_cdata("</r>");
  element.x_write(os);
}

#undef VNL_XIO_QUATERNION_INSTANTIATE
#define VNL_XIO_QUATERNION_INSTANTIATE(T) \
template VNL_TEMPLATE_EXPORT void x_write(std::ostream &, vnl_quaternion<T > const&, std::string); \
template VNL_TEMPLATE_EXPORT void x_write_tree(std::ostream &, vnl_quaternion<T > const&, std::string)

#endif // vnl_xio_quaternion_hxx_
