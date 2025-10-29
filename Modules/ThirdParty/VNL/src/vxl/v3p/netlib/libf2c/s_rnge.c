#include "stdio.h"
#include "v3p_f2c.h"
#ifdef __cplusplus
extern "C" {
#endif

/* called when a subscript is out of range */

#ifdef KR_headers
extern VOID sig_die();
integer s_rnge(varn, offset, procn, line) char *varn, *procn; ftnint offset, line;
#else
extern VOID sig_die(const char*,int);
integer s_rnge(char *varn, ftnint offset, char *procn, ftnint line)
#endif
{
register int i;

fprintf(stderr, "Subscript out of range on file line %ld, procedure ",
        (long)line);
while((i = *procn) && i != '_' && i != ' ')
        putc(*procn++, stderr);
fprintf(stderr, ".\nAttempt to access the %ld-th element of variable ",
        (long)offset+1);
while((i = *varn) && i != ' ')
        putc(*varn++, stderr);
sig_die(".", 1);
return 0;       /* not reached */
}
#ifdef __cplusplus
}
#endif
