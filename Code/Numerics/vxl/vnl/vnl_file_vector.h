#ifndef vnl_file_vector_h_
#define vnl_file_vector_h_
#ifdef __GNUC__
#pragma interface
#endif
// This is vxl/vnl/vnl_file_vector.h

//: \file
//  \brief Load vnl_vector<T> from file
//  \author Andrew W. Fitzgibbon, Oxford RRG, 23 Dec 96


//
//   Modifications:
//     fsm created by modifying class FileMatrix
//  LSB (Manchester) 23/3/01 Tidied documentation
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_vector.h>

//: Templated class to load a vector from a file.
template <class T>
class vnl_file_vector : public vnl_vector<T> {
public:
  vnl_file_vector(char const* filename);

  operator bool() const { return ok_; }

private:
  bool ok_;
};

#endif // vnl_file_vector_h_
