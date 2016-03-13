// This is core/vnl/xio/vnl_xio_matrix.h
#ifndef vnl_xio_matrix_h
#define vnl_xio_matrix_h
//:
// \file
// \brief XML write a vnl_matrix
//  Two functions which write a valid XML fragment to an ostream.
//  The matrix dimensions are output as attributes of the (top) node.
// - x_write() writes a single XML element node with text content only:
//             space-separated coordinates.
// - x_write_tree() writes a 3-level XML fragment: one top element node,
//                  one level below it for the matrix rows, and one level
//                  below that with the individual matrix elements (element
//                  nodes with a single value each).
//  The name of the top node can be chosen; by default, it is "vnl_matrix".
//  The names of the intermediate nodes are "row" and "cell", respectively.
// \author Peter Vanroose
// \date 29 July 2011
// \verbatim
//  Modifications
//   <none yet>
// \endverbatim

#include <string>
#include <iosfwd>
#include <vnl/vnl_matrix.h>
#include <vcl_compiler.h>

//: XML save vnl_matrix to stream.
// \relatesalso vnl_matrix
template <class T>
void x_write(std::ostream & os, vnl_matrix<T> const& v,
             std::string name="vnl_matrix");

//: XML save vnl_matrix as a 3-level tree to stream.
// \relatesalso vnl_matrix
template <class T>
void x_write_tree(std::ostream & os, vnl_matrix<T> const& v,
                  std::string name="vnl_matrix");

#define VNL_XIO_MATRIX_INSTANTIATE(T) extern "Please #include <vnl/xio/vnl_xio_matrix.hxx> first"

#endif // vnl_xio_matrix_h
