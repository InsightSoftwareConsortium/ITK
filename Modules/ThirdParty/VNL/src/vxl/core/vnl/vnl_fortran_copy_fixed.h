// This is core/vnl/vnl_fortran_copy_fixed.h
#ifndef vnl_fortran_copy_fixed_h_
#define vnl_fortran_copy_fixed_h_
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
//   Oct.2009 - Converted for a stack-storage fixed-size version
// \endverbatim
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>
#include "vnl/vnl_export.h"

//:  Convert row-stored matrix to column-stored.
//  Convert C format (row-stored) matrix to fortran format (column-stored) matrix.
template <class T, unsigned R, unsigned C>
class VNL_TEMPLATE_EXPORT vnl_fortran_copy_fixed
{
 public:
  // Constructors/Destructors--------------------------------------------------

  vnl_fortran_copy_fixed(vnl_matrix_fixed<T, R, C> const & M);

  // Operations----------------------------------------------------------------
  operator T*() { return data; }

 protected:
  // Data Members--------------------------------------------------------------
  T data[R*C];

 private:
  // Helpers-------------------------------------------------------------------
};

#endif // vnl_fortran_copy_fixed_h_
