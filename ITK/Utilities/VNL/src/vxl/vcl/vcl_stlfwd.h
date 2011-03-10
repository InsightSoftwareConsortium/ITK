#ifndef vcl_stlfwd_h_
#define vcl_stlfwd_h_

#include "vcl_compiler.h"

#if !VCL_USE_NATIVE_STL
# include "emulation/vcl_stlfwd.h"
#else
# include "vcl_memory.h"
# include "vcl_vector.h"
# include "vcl_list.h"
# include "vcl_map.h"
# include "vcl_set.h"
# include "vcl_queue.h"
# include "vcl_deque.h"
#endif

#endif // vcl_stlfwd_h_
