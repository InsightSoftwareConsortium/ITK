// This is core/vnl/xio/vnl_xio_vector.h
#ifndef vnl_xio_vector_h
#define vnl_xio_vector_h
//:
// \file
// \brief XML write a vnl_vector
//  Two functions which write a valid XML fragment to an ostream.
//  The vector length is output as an attribute of the (top) node.
// - x_write() writes a single XML element node with text content only:
//             space-separated coordinates.
// - x_write_tree() writes a 2-level XML fragment: one top element node,
//                  and one level below it for the individual vector elements
//                  (element nodes named "element" with a single value each).
//  The name of the top node can be chosen; by default, it is "vnl_vector".
// \author Gamze Tunali
// \date 30-Dec-2005
// \verbatim
//  Modifications
//   29 July 2011 - Peter Vanroose - added documentation, tests, and x_write_tree()
// \endverbatim

#include <string>
#include <iosfwd>
#include <vnl/vnl_vector.h>
#include <vcl_compiler.h>

//: XML save vnl_vector to stream.
// \relatesalso vnl_vector
template <class T>
void x_write(std::ostream & os, vnl_vector<T> const& v,
             std::string name="vnl_vector");

//: XML save vnl_vector as a 2-level tree to stream.
// \relatesalso vnl_vector
template <class T>
void x_write_tree(std::ostream & os, vnl_vector<T> const& v,
                  std::string name="vnl_vector");

#define VNL_XIO_VECTOR_INSTANTIATE(T) extern "Please #include <vnl/xio/vnl_xio_vector.hxx> first"

#endif // vnl_xio_vector_h
