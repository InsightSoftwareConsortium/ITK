#ifndef vnl_fortran_copy_h_
#define vnl_fortran_copy_h_
#ifdef __GNUC__
#pragma interface
#endif
//
// .NAME	vnl_fortran_copy - convert row-stored matrix to column-stored
// .LIBRARY	vnl
// .HEADER	vxl Package
// .INCLUDE	vnl/vnl_fortran_copy.h
// .FILE	vnl_fortran_copy.txx
// .SECTION Description
//    convert C format (row-stored) matrix to fortran format (column-stored) matrix.
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 29 Aug 96
//
// .SECTION Modifications:
//     <none yet>
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix.h>

template <class T>
class vnl_fortran_copy {
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
