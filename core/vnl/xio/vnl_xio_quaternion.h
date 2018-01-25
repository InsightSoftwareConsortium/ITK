// This is core/vnl/xio/vnl_xio_quaternion.h
#ifndef vnl_xio_quaternion_h
#define vnl_xio_quaternion_h
//:
// \file
// \brief XML write a vnl_quaternion
//  Two functions which write a valid XML fragment to an ostream.
// - x_write() writes a single XML element node without content,
//   but with the four values as attributes.
// - x_write_tree() writes a 2-level XML fragment: one top element node,
//                  and one level below it for the four elements
//                  (element nodes x, y, z, and r with a single value each).
//  The name of the top node can be chosen; by default, it is "vnl_quaternion".
// \author Gamze D. Tunali
// \date 22-Dec-2005
// \verbatim
//  Modifications
//   29 July 2011 - Peter Vanroose - added documentation, tests, and x_write_tree()
// \endverbatim

#include <string>
#include <iosfwd>
#include <vnl/vnl_quaternion.h>
#include <vcl_compiler.h>

//: XML save vnl_quaternion to stream.
// \relatesalso vnl_quaternion
template <class T>
void x_write(std::ostream & os, vnl_quaternion<T> const& v,
             std::string name="vnl_quaternion");

//: XML save vnl_quaternion as a 2-level tree to stream.
// \relatesalso vnl_quaternion
template <class T>
void x_write_tree(std::ostream & os, vnl_quaternion<T> const& v,
                  std::string name="vnl_quaternion");

#define VNL_XIO_QUATERNION_INSTANTIATE(T) extern "Please #include <vnl/xio/vnl_xio_quaternion.hxx> first"

#endif // vnl_xio_quaternion_h
