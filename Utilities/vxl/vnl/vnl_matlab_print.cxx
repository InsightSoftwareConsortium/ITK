// This is vxl/vnl/vnl_matlab_print.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author fsm
//
// Adapted from awf's MatOps class.

#include "vnl_matlab_print.h"
#include <vcl_iostream.h>
#include <vnl/vnl_matrix.h>

//: Can be used within debugger to print matrix
extern "C"
void vnl_dbprintmx(vnl_matrix<double> const& p)
{
  // why the cast? is it a const_cast?
  vnl_matlab_print(vcl_cerr, p, (char const*)"M", vnl_matlab_print_format_default);
}
