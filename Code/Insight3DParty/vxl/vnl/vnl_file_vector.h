#ifndef vnl_file_vector_h_
#define vnl_file_vector_h_
#ifdef __GNUC__
#pragma interface
#endif
// .NAME	vnl_file_vector - Load vnl_vector<T> from file
// .LIBRARY	vnl
// .HEADER	vxl Package
// .INCLUDE	vnl/vnl_file_vector.h
// .FILE	vnl_file_vector.txx
//
// .SECTION Description
//    vnl_file_vector is a templated class to load a vector from a file.
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 23 Dec 96
//
// .SECTION Modifications:
//     fsm created by modifying class FileMatrix
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_vector.h>

template <class T>
class vnl_file_vector : public vnl_vector<T> {
public:
  vnl_file_vector(char const* filename);

  operator bool() const { return ok_; }

private:
  bool ok_;
};

#endif // vnl_file_vector_h_
