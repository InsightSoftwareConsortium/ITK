#include "sysdep1.h"    /* here to get stat64 on some badly designed Linux systems */
#include "v3p_f2c.h"
#include "fio.h"
#ifdef __cplusplus
extern "C" {
#endif

 VOID
#ifdef KR_headers
#define Const /*nothing*/
g_char(a,alen,b) char *a,*b; ftnlen alen;
#else
#define Const const
g_char(const char *a, ftnlen alen, char *b)
#endif
{
        Const char *x = a + alen;
        char *y = b + alen;

        for(;; y--) {
                if (x <= a) {
                        *b = 0;
                        return;
                        }
                if (*--x != ' ')
                        break;
                }
        *y-- = 0;
        do *y-- = *x;
                while(x-- > a);
        }

 VOID
#ifdef KR_headers
b_char(a,b,blen) char *a,*b; ftnlen blen;
#else
b_char(const char *a, char *b, ftnlen blen)
#endif
{       int i;
        for(i=0;i<blen && *a!=0;i++) *b++= *a++;
        for(;i<blen;i++) *b++=' ';
}
#ifndef NON_UNIX_STDIO
#ifdef KR_headers
long f__inode(a, dev) char *a; int *dev;
#else
long f__inode(char *a, int *dev)
#endif
{       struct STAT_ST x;
        if(STAT(a,&x)<0) return(-1);
        *dev = x.st_dev;
        return(x.st_ino);
}
#endif
#ifdef __cplusplus
}
#endif
