// This is core/vnl/xio/vnl_xio_quaternion.txx
#ifndef vnl_xio_quaternion_txx_
#define vnl_xio_quaternion_txx_
//:
// \file

#include "vnl_xio_quaternion.h"
#include <vnl/vnl_quaternion.h>
#include <vsl/vsl_basic_xml_element.h>

//=================================================================================
//: Binary save self to stream.
template<class T>
void x_write(vcl_ostream & os, const vnl_quaternion<T> & p, vcl_string name)
{
  vsl_basic_xml_element element(name);
  element.add_attribute("x", p.x());
  element.add_attribute("y", p.y());
  element.add_attribute("z", p.z());
  element.add_attribute("r", p.r());
  element.x_write(os);
}

#define VNL_XIO_QUATERNION_INSTANTIATE(T) \
template void x_write(vcl_ostream &, const vnl_quaternion<T > &, vcl_string name)

#endif // vnl_xio_quaternion_txx_
