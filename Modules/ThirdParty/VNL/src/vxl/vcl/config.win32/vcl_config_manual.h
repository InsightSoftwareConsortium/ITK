#ifndef vcl_config_manual_h_config_win32_
#define vcl_config_manual_h_config_win32_

// This file is *not* generated. It must be consistent
// with vcl_config_manual.h.in, though. See same file
// for explanation of the meaning of these macros.

#ifndef _MSC_VER
  ** error **
#else 
#  if _MSC_VER >=1300
#    include "vc70/vcl_config_manual.h"
#  else //_MSC_VER >=1200
#    include "vc60/vcl_config_manual.h"
#  endif
#endif

#endif // vcl_config_manual_h_config_win32_
