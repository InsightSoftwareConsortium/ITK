// This is core/testlib/testlib_root_dir.cxx
#include <cstdlib>
#include <iostream>
#include "testlib_root_dir.h"
//:
// \file

#include <vcl_compiler.h>

// The following should have been created automatically by the
// configuration scripts from vcl_where_root_dir.h.in
// We need to check for its existence and if it doesn't exist - do something else.
#include <vcl_where_root_dir.h>
//: Return source root directory (ie the one just below vcl and vxl).
std::string testlib_root_dir()
{
  char* ptr = std::getenv("VXLSRC");
  if (ptr)
    return std::string(ptr);

  ptr= std::getenv("VCLSRC");
  if (ptr)
    return std::string(ptr);

  ptr= std::getenv("VXL_SRC");
  if (ptr)
    return std::string(ptr);

  return std::string(VCL_SOURCE_ROOT_DIR);

  //std::cerr<<"ERROR: testlib_root_dir() Unable to retrieve directory from\n"
  //      <<"$VCLSRC or $VXLSRC or $VXL_SRC.  Sorry.\n";
  //return std::string("");
}
