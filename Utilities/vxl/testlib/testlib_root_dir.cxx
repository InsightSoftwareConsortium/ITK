// This is vxl/testlib/testlib_root_dir.cxx
#include "testlib_root_dir.h"
//:
// \file

#include <vcl_cstdlib.h>
#include <vcl_iostream.h>

// The following should have been created automatically by the
// configuration scripts from vcl_where_root_dir.h.in
// We need to check for its existence and if it doesn't exist - do something else.
#ifdef VCL_WHERE_ROOT_DIR_H_EXISTS
#include <vcl_where_root_dir.h>
//: Return source root directory (ie the one just below vcl).
vcl_string testlib_root_dir()
{
  return vcl_string(VCL_SOURCE_ROOT_DIR);
}
#else
//: Return source root directory (ie the one just below vcl and vxl).
vcl_string testlib_root_dir()
{
  char* ptr;

  ptr= vcl_getenv("VXLSRC");
  if (ptr)
    return vcl_string(ptr);

  ptr= vcl_getenv("VCLSRC");
  if (ptr)
    return vcl_string(ptr);

  ptr= vcl_getenv("VXL_SRC");
  if (ptr)
    return vcl_string(ptr);

  vcl_cerr<<"ERROR: testlib_root_dir() Unable to retrieve directory from\n"
          <<"$VCLSRC or $VXLSRC or $VXL_SRC.  Sorry.\n";
  return vcl_string("");
}

#endif
