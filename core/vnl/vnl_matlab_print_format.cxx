// This is core/vnl/vnl_matlab_print_format.cxx
#include "vnl_matlab_print_format.h"
//:
// \file

#include <vcl_iostream.h>
#include <vcl_vector.h>

//: Choose precision in printouts.
//
// vnl_matlab_format(vnl_matops::fmt_long) selects 16-digit precision
//
// vnl_matlab_format(vnl_matops::fmt_short) selects 4-digit precision

//: this variable is the current top of the stack.
// moved here because gcc 2.7 choked
static vnl_matlab_print_format the_format = vnl_matlab_print_format_short;
//: the rest of the stack is stored in this vector.
static vcl_vector<int> *format_stack = 0;
//: call this to initialize the format stack.
static void vnl_matlab_print_format_init()
{ if (!format_stack) format_stack = new vcl_vector<int>; }

void vnl_matlab_print_format_push(vnl_matlab_print_format f)
{
  vnl_matlab_print_format_init();
  format_stack->push_back(the_format);
  the_format = f;
}

void vnl_matlab_print_format_pop()
{
  vnl_matlab_print_format_init();
  if (format_stack->empty())
    vcl_cerr << __FILE__ ": format stack empty\n";
  else {
    the_format = vnl_matlab_print_format(format_stack->back());
    format_stack->pop_back();
  }
}

vnl_matlab_print_format vnl_matlab_print_format_set(vnl_matlab_print_format f)
{
  vnl_matlab_print_format_init();
  vnl_matlab_print_format old = the_format;
  the_format = f;
  return old;
}

vnl_matlab_print_format vnl_matlab_print_format_top()
{
  return the_format;
}
