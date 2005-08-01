// This is core/vnl/vnl_file_vector.txx
#ifndef vnl_file_vector_txx_
#define vnl_file_vector_txx_
//:
// \file

#include "vnl_file_vector.h"

#include <vcl_fstream.h>
#include <vcl_cstring.h> // for strcmp()

//: Load vector from filename.
template <class T>
vnl_file_vector<T>::vnl_file_vector(char const* filename)
  : vnl_vector<T>() // makes an empty vector.
{
#ifdef DEBUG
  vcl_cerr << "filename=" << filename << "\nsize=" << this->size() << '\n';
#endif
  if (filename && vcl_strcmp(filename, "-")) {
    vcl_ifstream o(filename);
    ok_ = this->read_ascii(o);
  }
  else
    ok_ = this->read_ascii(vcl_cin);
#ifdef DEBUG
    vcl_cerr << "size=" << this->size() << '\n';
#endif
  if (!ok_)
    vcl_cerr << "vnl_file_vector: ERROR loading from " << filename << '\n';
}

//--------------------------------------------------------------------------------

#undef VNL_FILE_VECTOR_INSTANTIATE
#define VNL_FILE_VECTOR_INSTANTIATE(T) template class vnl_file_vector<T >

#endif // vnl_file_vector_txx_
