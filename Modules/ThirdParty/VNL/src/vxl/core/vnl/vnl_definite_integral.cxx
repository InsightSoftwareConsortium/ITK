#include <vcl_compiler.h>
#include "vnl_definite_integral.h"
#include <vnl/vnl_integrant_fnct.h>

// initialize the static member
vnl_integrant_fnct* vnl_definite_integral::pfnct_ = VXL_NULLPTR;
