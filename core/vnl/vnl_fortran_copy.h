// This is core/vnl/vnl_fortran_copy.h
#ifndef vnl_fortran_copy_h_
#define vnl_fortran_copy_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Convert row-stored matrix to column-stored
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   29 Aug 96
//
// convert C format (row-stored) matrix to fortran format (column-stored) matrix
//
// \verbatim
//  Modifications
//   LSB (Manchester) 23/3/01 Tidied documentation
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
// \endverbatim
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix.h>
//:  Convert row-stored matrix to column-stored.
//  Convert C format (row-stored) matrix to fortran format (column-stored) matrix.
template <class T>
class vnl_fortran_copy
{
 public:
  // Constructors/Destructors--------------------------------------------------

  vnl_fortran_copy(vnl_matrix<T> const & M);

  ~vnl_fortran_copy();

  // Operations----------------------------------------------------------------
  operator T*() { return data; }

 protected:
  // Data Members--------------------------------------------------------------
  int sz;
  T *data;

 private:
  // Helpers-------------------------------------------------------------------
};

#endif // vnl_fortran_copy_h_
