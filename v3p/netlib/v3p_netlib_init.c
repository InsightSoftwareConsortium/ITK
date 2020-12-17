#include "v3p_netlib.h"

/* Initialization flag.  Purposely left with default compiler
   initialization to zero since it may be referenced before this
   translation unit is initialized.  */
static int v3p_netlib_initialized;

/* Library-wide initialization function.  Calling this is not
   necessary for single-threaded applications.  For multi-threaded
   applications it should be called in the main thread before other
   threads are created.  */
void v3p_netlib_initialize()
{
  if(!v3p_netlib_initialized)
    {
    v3p_netlib_initialized = 1;

    /* Call the per-routine initialization functions.  */
    v3p_netlib_slamch_init();
    v3p_netlib_dlamch_init();
    v3p_netlib_slartg_init();
    v3p_netlib_dlartg_init();
    }
}
