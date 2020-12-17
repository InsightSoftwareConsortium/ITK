#ifndef vnl_matlab_print_format_h_
#define vnl_matlab_print_format_h_

#include "vnl/vnl_export.h"
/*
  fsm
*/

//:
// \file

//: pretty-printing matlab formats
enum vnl_matlab_print_format
{
  vnl_matlab_print_format_default,
  vnl_matlab_print_format_short,
  vnl_matlab_print_format_long,
  vnl_matlab_print_format_short_e,
  vnl_matlab_print_format_long_e
};

// -------------------- Setting the default format.

//: get top of stack :
VNL_EXPORT vnl_matlab_print_format vnl_matlab_print_format_top();

//: set new, get old format at top of stack :
VNL_EXPORT vnl_matlab_print_format vnl_matlab_print_format_set(vnl_matlab_print_format);

//: push/pop the top of the stack :
VNL_EXPORT void vnl_matlab_print_format_push(vnl_matlab_print_format);
VNL_EXPORT void vnl_matlab_print_format_pop ();

#endif
