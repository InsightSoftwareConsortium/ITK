#ifndef testlib_root_dir_h_
#define testlib_root_dir_h_
//:
// \file
// \brief Function to return root directory (ie the one just below vcl).
// \author Tim Cootes
// \verbatim
//    Modifications
//     17-05-2001 I. Scott - Moved from vul to testlib
// \endverbatim
#include <vcl_string.h>

//: Return source root directory (ie the one just below vcl).
//  If the file vcl_where_root_dir.h has been automatically generated
//  during configuration (which will happen with cmake) then the
//  appropriate source directory will be returned.
//
//  If another build system is used in which this is not created,
//  the function will return the value of either of the environment
//  variables: VXLSRC, VCLSRC or VXL_SRC in that order.
vcl_string testlib_root_dir();

#endif // testlib_root_dir_h_
