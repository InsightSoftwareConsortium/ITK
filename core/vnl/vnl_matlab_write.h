// This is core/vnl/vnl_matlab_write.h
#ifndef vnl_matlab_write_h_
#define vnl_matlab_write_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief Write to a MAT file
//
// Core code stolen from D. Capel's code. These functions are useful
// because they allow one to write, say, an image buffer to a MAT file.
//
// NB. with these functions, the variable name \e must be a non-null and
// point to a zero-terminated string. otherwise the code will segfault.
//
//  \author fsm
//
// \verbatim
//  Modifications
//   09 Mar 2000 fsm. changed order of arguments for consistency with \sa vnl_matlab_read.
// \endverbatim

#include <iosfwd>
#include <vcl_compiler.h>
#include "vnl/vnl_export.h"

template <class T> VNL_TEMPLATE_EXPORT // scalar
bool vnl_matlab_write(std::ostream &, T const &, char const *variable_name);

template <class T> VNL_TEMPLATE_EXPORT // 1D array
bool vnl_matlab_write(std::ostream &, T const *, unsigned size, char const *variable_name);

template <class T> VNL_TEMPLATE_EXPORT // 2D array
bool vnl_matlab_write(std::ostream &, T const * const *, unsigned rows, unsigned cols, char const *variable_name);

#endif // vnl_matlab_write_h_
