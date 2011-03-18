// This is core/vnl/vnl_file_matrix.txx
#ifndef vnl_file_matrix_txx_
#define vnl_file_matrix_txx_
//:
// \file
//
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   23 Dec 96
//
//-----------------------------------------------------------------------------

#include "vnl_file_matrix.h"
#include <vcl_fstream.h>
#include <vcl_iostream.h>

//: Load matrix from filename.
template <class T>
vnl_file_matrix<T>::vnl_file_matrix(char const* filename)
{
  if (filename && filename[0]=='-' && filename[1]=='\0')
    ok_ = this->read_ascii(vcl_cin);
  else {
    vcl_ifstream o(filename);
    ok_ = this->read_ascii(o);
  }

  if (!ok_)
    vcl_cerr << "vnl_file_matrix: ERROR loading " << filename << '\n';
}

//--------------------------------------------------------------------------------

#undef VNL_FILE_MATRIX_INSTANTIATE
#define VNL_FILE_MATRIX_INSTANTIATE(T) template class vnl_file_matrix<T >

#endif // vnl_file_matrix_txx_
