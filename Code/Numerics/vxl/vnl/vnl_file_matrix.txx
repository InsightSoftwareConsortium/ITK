
// This is vxl/vnl/vnl_file_matrix.txx

//
// vnl_file_matrix
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: 23 Dec 96
//
//-----------------------------------------------------------------------------

#include "vnl_file_matrix.h"
#include <vcl_fstream.h>
#include <vcl_cstring.h> // for strcmp()

//: Load matrix from filename.
template <class T>
vnl_file_matrix<T>::vnl_file_matrix(char const* filename)
{
  if (filename && strcmp(filename, "-")) {
    vcl_ifstream o(filename);
    ok_=read_ascii(o);
    if (!ok_)
      vcl_cerr << "vnl_file_matrix: ERROR loading " << filename << vcl_endl;
  }
  else {
    ok_=read_ascii(vcl_cin);
    if (!ok_)
      vcl_cerr << "vnl_file_matrix: ERROR loading from stdin " << vcl_endl;
  }
}

//--------------------------------------------------------------------------------

#undef VNL_FILE_MATRIX_INSTANTIATE
#define VNL_FILE_MATRIX_INSTANTIATE(T) template class vnl_file_matrix<T >
