// This is core/vnl/vnl_file_vector.hxx
#ifndef vnl_file_vector_hxx_
#define vnl_file_vector_hxx_
//:
// \file

#include <fstream>
#include <iostream>
#include <cstring>
#include "vnl_file_vector.h"

#include <vcl_compiler.h>

//: Load vector from filename.
template <class T>
vnl_file_vector<T>::vnl_file_vector(char const* filename)
  : vnl_vector<T>() // makes an empty vector.
{
#ifdef DEBUG
  std::cerr << "filename=" << filename << "\nsize=" << this->size() << '\n';
#endif
  if (filename && std::strcmp(filename, "-")) {
    std::ifstream o(filename);
    ok_ = this->read_ascii(o);
  }
  else
    ok_ = this->read_ascii(std::cin);
#ifdef DEBUG
    std::cerr << "size=" << this->size() << '\n';
#endif
  if (!ok_)
    std::cerr << "vnl_file_vector: ERROR loading from " << filename << '\n';
}

//--------------------------------------------------------------------------------

#undef VNL_FILE_VECTOR_INSTANTIATE
#define VNL_FILE_VECTOR_INSTANTIATE(T) \
template class VNL_EXPORT vnl_file_vector<T >

#endif // vnl_file_vector_hxx_
