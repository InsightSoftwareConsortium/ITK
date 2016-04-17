// This is core/vnl/xio/vnl_xio_matrix_fixed.h
#ifndef vnl_xio_matrix_fixed_h
#define vnl_xio_matrix_fixed_h
//:
// \file
// \brief XML write a vnl_matrix_fixed
//  Two functions which write a valid XML fragment to an ostream.
//  The matrix dimensions are output as attributes of the (top) node.
// - x_write() writes a single XML element node with text content only:
//             space-separated coordinates.
// - x_write_tree() writes a 3-level XML fragment: one top element node,
//                  one level below it for the matrix rows, and one level
//                  below that with the individual matrix elements (element
//                  nodes with a single value each).
//  The name of the top node can be chosen; by default, it is "vnl_matrix_fixed".
//  The names of the intermediate nodes are "row" and "cell", respectively.
// \author Gamze D. Tunali
// \date 22-Dec-2005
// \verbatim
//  Modifications
//   29 July 2011 - Peter Vanroose - added documentation, tests, and x_write_tree()
// \endverbatim

#include <string>
#include <iosfwd>
#include <vnl/vnl_matrix_fixed.h>
#include <vcl_compiler.h>

//: XML save vnl_matrix_fixed to stream.
// \relatesalso vnl_matrix_fixed
template <class T, unsigned m, unsigned n>
void x_write(std::ostream & os, vnl_matrix_fixed<T,m,n> const& v,
             std::string name="vnl_matrix_fixed");

//: XML save vnl_matrix_fixed as a 3-level tree to stream.
// \relatesalso vnl_matrix_fixed
template <class T, unsigned m, unsigned n>
void x_write_tree(std::ostream & os, vnl_matrix_fixed<T,m,n> const& v,
                  std::string name="vnl_matrix_fixed");

#define VNL_XIO_MATRIX_FIXED_INSTANTIATE(T) extern "Please #include <vnl/xio/vnl_xio_matrix_fixed.hxx> first"

#endif // vnl_xio_matrix_fixed_h
