#ifndef vcl_fstream_h_
#define vcl_fstream_h_
/*
  fsm
*/

// this is to get the vcl_ios_* macros.
#include "vcl_iostream.h"

#if defined(VCL_SGI_CC_720)
# include <fstream.h>

#else  // -------------------- ISO
# include "iso/vcl_fstream.h"
#endif

#endif // vcl_fstream_h_
