#ifndef testlib_root_dir_h_
#define testlib_root_dir_h_
//:
// \file
// \brief Function to return root directory (i.e. the parent directory of both core and contrib).
// \author Tim Cootes
// \verbatim
//    Modifications
//     17-05-2001 I. Scott - Moved from vul to testlib
// \endverbatim
#include <string>
#include <vcl_compiler.h>

// macros to stringify compiler -D path value
#define TEST_STR(x) #x
#define TEST_PATH_DEFINE(x) TEST_STR(x)

//: Return source root directory (i.e. the parent directory of both core and contrib).
// *** Only use this directory tree for read-only operations! ***
//
//  If the file vcl_where_root_dir.h has been automatically generated
//  during configuration (which will happen with cmake) then the
//  appropriate source directory will be returned.
//
//  If another build system is used in which this is not created,
//  the function will return the value of either of the environment
//  variables: VXLSRC, VCLSRC or VXL_SRC in that order.
std::string testlib_root_dir();

#endif // testlib_root_dir_h_
