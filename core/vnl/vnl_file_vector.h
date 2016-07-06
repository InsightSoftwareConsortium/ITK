// This is core/vnl/vnl_file_vector.h
#ifndef vnl_file_vector_h_
#define vnl_file_vector_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Load vnl_vector<T> from file
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   23 Dec 96
//
// \verbatim
//  Modifications
//   fsm created by modifying class FileMatrix
//   LSB (Manchester) 23/3/01 Tidied documentation
// \endverbatim
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_vector.h>
#include "vnl/vnl_export.h"

//: Templated class to load a vector from a file.
template <class T>
class VNL_TEMPLATE_EXPORT vnl_file_vector : public vnl_vector<T>
{
 private:
  VCL_SAFE_BOOL_DEFINE;
 public:
  vnl_file_vector(char const* filename);

  operator safe_bool () const
    { return (ok_)? VCL_SAFE_BOOL_TRUE : 0; }
  bool operator!() const
    { return !ok_; }

 private:
  bool ok_;
};

#endif // vnl_file_vector_h_
