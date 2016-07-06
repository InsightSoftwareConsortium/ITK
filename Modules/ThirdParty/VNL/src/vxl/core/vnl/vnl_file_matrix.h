// This is core/vnl/vnl_file_matrix.h
#ifndef vnl_file_matrix_h_
#define vnl_file_matrix_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
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
VCL_TEMPLATE_EXPORT template <class T>
class VNL_TEMPLATE_EXPORT vnl_file_matrix : public vnl_matrix<T>
{
 private:
  VCL_SAFE_BOOL_DEFINE;
 public:
  vnl_file_matrix(char const* filename);

  operator safe_bool () const
    { return (ok_)? VCL_SAFE_BOOL_TRUE : 0; }
  bool operator!() const
    { return !ok_; }

 private:
  bool ok_;
};

#endif // vnl_file_matrix_h_
