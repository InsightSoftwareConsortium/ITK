#ifndef vcl_config_compiler_h_config_win32_
#define vcl_config_compiler_h_config_win32_

#ifndef _MSC_VER
# ifndef __BORLANDC__
  ** error **
# else
# include "borland55/vcl_config_compiler.h"
# endif  
#else 
#  if _MSC_VER >=1300
#    include "vc70/vcl_config_compiler.h"
#  else //_MSC_VER >=1200
#    include "vc60/vcl_config_compiler.h"
#  endif
#endif


#endif // vcl_config_compiler_h_config_win32_
