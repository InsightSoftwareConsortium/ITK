#ifndef vnl_file_matrix_h_
#define vnl_file_matrix_h_
#ifdef __GNUC__
#pragma interface
#endif
// This is vxl/vnl/vnl_file_matrix.h

//: \file
// \brief Load vnl_matrix<double> from file
//  \author Andrew W. Fitzgibbon, Oxford RRG, 23 Dec 96

// Modifications:
// LSB (Manchester) 23/3/01 Documenation tidied
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix.h>
//: Class to load a matrix from a file.
export template <class T>
class vnl_file_matrix : public vnl_matrix<T> {
public:
  vnl_file_matrix(char const* filename);

  operator bool() const { return ok_; }

private:
  bool ok_;
};

#endif // vnl_file_matrix_h_
