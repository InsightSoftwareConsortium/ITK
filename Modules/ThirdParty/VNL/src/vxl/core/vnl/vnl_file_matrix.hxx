// This is core/vnl/vnl_file_matrix.hxx
#ifndef vnl_file_matrix_hxx_
#define vnl_file_matrix_hxx_
//:
// \file
//
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   23 Dec 96
//
//-----------------------------------------------------------------------------

#include <fstream>
#include <iostream>
#include "vnl_file_matrix.h"
#include <vcl_compiler.h>

//: Load matrix from filename.
template <class T>
vnl_file_matrix<T>::vnl_file_matrix(char const* filename)
{
  if (filename && filename[0]=='-' && filename[1]=='\0')
    ok_ = this->read_ascii(std::cin);
  else {
    std::ifstream o(filename);
    ok_ = this->read_ascii(o);
  }

  if (!ok_)
    std::cerr << "vnl_file_matrix: ERROR loading " << filename << '\n';
}

//--------------------------------------------------------------------------------

#undef VNL_FILE_MATRIX_INSTANTIATE
#define VNL_FILE_MATRIX_INSTANTIATE(T) \
template class VNL_EXPORT vnl_file_matrix<T >

#endif // vnl_file_matrix_hxx_
