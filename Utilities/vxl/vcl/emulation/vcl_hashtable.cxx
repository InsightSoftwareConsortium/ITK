#include <vcl_compiler.h>
#if !VCL_USE_NATIVE_STL

#include "vcl_hashtable.h"

#if ( __STL_STATIC_TEMPLATE_DATA > 0 ) && ! defined (VCL_WIN32)

//template struct __stl_prime<false>;

const unsigned long VCL_prime<false>::list_[] =
{
  53,         97,         193,       389,       769,
  1543,       3079,       6151,      12289,     24593,
  49157,      98317,      196613,    393241,    786433,
  1572869,    3145739,    6291469,   12582917,  25165843,
  50331653,   100663319,  201326611, 402653189, 805306457, 
  1610612741, 3221225473U, 4294967291U
};

#endif
#endif // VCL_USE_NATIVE_STL
