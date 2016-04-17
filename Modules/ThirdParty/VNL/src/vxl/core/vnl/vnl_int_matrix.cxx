// This is core/vnl/vnl_int_matrix.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
//
// vnl_int_matrix
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: 27 Dec 96
//
//-----------------------------------------------------------------------------

#include <fstream>
#include "vnl_int_matrix.h"
#include <vcl_compiler.h>

//: Construct from matrix of double.
//  The double-to-int conversion is simply the standard (int) cast.
vnl_int_matrix::vnl_int_matrix(const vnl_matrix<double>& d):
  Base(d.rows(), d.columns())
{
  unsigned m = d.rows();
  unsigned n = d.columns();

  for (unsigned i = 0; i < m; ++i)
    for (unsigned j = 0; j < n; ++j)
      (*this)(i,j) = (int)d(i,j);
}

//: Load from disk
vnl_int_matrix::vnl_int_matrix(char const* filename)
{
  std::ifstream s(filename);
  read_ascii(s);
}
