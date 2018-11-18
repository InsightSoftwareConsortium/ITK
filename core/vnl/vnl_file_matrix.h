// This is core/vnl/vnl_file_matrix.h
#ifndef vnl_file_matrix_h_
#define vnl_file_matrix_h_
//:
// \file
// \brief Load vnl_matrix<double> from file
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   23 Dec 96
//
// \verbatim
//  Modifications
//   LSB (Manchester) 23/3/01 Documentation tidied
// \endverbatim
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix.h>
#include "vnl/vnl_export.h"

//: Class to load a matrix from a file.
template <class T>
class VNL_EXPORT vnl_file_matrix : public vnl_matrix<T>
{
 private:

 public:
  vnl_file_matrix(char const* filename);

  explicit operator bool () const
    { return (ok_)? true : false; }
  bool operator!() const
    { return !ok_; }

 private:
  bool ok_;
};

#endif // vnl_file_matrix_h_
