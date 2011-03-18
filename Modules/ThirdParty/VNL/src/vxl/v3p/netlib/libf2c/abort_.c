#include "stdio.h"
#include "v3p_f2c.h"
#ifdef __cplusplus
extern "C" {
#endif

#ifdef KR_headers
extern VOID sig_die();

int abort_()
#else
extern void sig_die(char*,int);

int abort_(void)
#endif
{
sig_die("Fortran abort routine called", 1);
return 0;       /* not reached */
}
#ifdef __cplusplus
}
#endif
