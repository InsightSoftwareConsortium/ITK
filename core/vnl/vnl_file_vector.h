// This is core/vnl/vnl_file_vector.h
#ifndef vnl_file_vector_h_
#define vnl_file_vector_h_
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
class VNL_EXPORT vnl_file_vector : public vnl_vector<T>
{
 private:

 public:
  vnl_file_vector(char const* filename);

  explicit operator bool () const
    { return (ok_)? true : false; }
  bool operator!() const
    { return !ok_; }

 private:
  bool ok_;
};

#endif // vnl_file_vector_h_
