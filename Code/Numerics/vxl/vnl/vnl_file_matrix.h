#ifndef vnl_file_matrix_h_
#define vnl_file_matrix_h_
#ifdef __GNUC__
#pragma interface
#endif
// .NAME	vnl_file_matrix - Load vnl_matrix<double> from file
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_file_matrix.h
// .FILE	vnl_file_matrix.txx
//
// .SECTION Description
//    vnl_file_matrix is a class to load a matrix from a file.
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 23 Dec 96
//
// .SECTION Modifications:
//     <none yet>
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix.h>

template <class T>
class vnl_file_matrix : public vnl_matrix<T> {
public:
  vnl_file_matrix(char const* filename);

  operator bool() const { return ok_; }

private:
  bool ok_;
};

#endif // vnl_file_matrix_h_
