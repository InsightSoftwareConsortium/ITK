/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


/* A simple test program to see if a function "works" */
#define SIMPLE_TEST(x) int main(void){ x; return 0; }


#ifdef HAVE_ATTRIBUTE

int
main ()
{
    int __attribute__((unused)) x;

    return 0;
}

#endif /* HAVE_ATTRIBUTE */

#ifdef HAVE_TIMEZONE

#ifdef HAVE_SYS_TIME_H
#   include <sys/time.h>
#endif
#include <time.h>
SIMPLE_TEST(timezone = 0);

#endif /* HAVE_TIMEZONE */

#ifdef SYSTEM_SCOPE_THREADS
#include <stdlib.h>
#include <pthread.h>

int main(void)
{
    pthread_attr_t attribute;
    int ret;

    pthread_attr_init(&attribute);
    ret = pthread_attr_setscope(&attribute, PTHREAD_SCOPE_SYSTEM);
    if (ret == 0)
        return 0;
    return 1;
}

#endif /* SYSTEM_SCOPE_THREADS */

#ifdef HAVE_SOCKLEN_T

#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#   include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#   include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#   include <sys/socket.h>
#endif

SIMPLE_TEST(socklen_t foo);

#endif /* HAVE_SOCKLEN_T */

#ifdef DEV_T_IS_SCALAR

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

int main ()
{
    dev_t d1, d2;
    if (d1 == d2)
        return 0;
    return 1;
}

#endif /* DEV_T_IS_SCALAR */

#ifdef HAVE_OFF64_T

#include <sys/types.h>

int main(void)
{
    off64_t n = 0;
    return (int)n;
}
#endif

#ifdef TEST_DIRECT_VFD_WORKS

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

int main(void)
{
    int fid;

    if ((fid = open("tst_file", O_CREAT | O_TRUNC | O_DIRECT, 0755)) < 0)
        return 1;
    close(fid);
    remove("tst_file");

    return 0;
}
#endif

#ifdef HAVE_DIRECT
       SIMPLE_TEST(posix_memalign());
#endif

#ifdef HAVE_DEFAULT_SOURCE
/* Check default source */
#include <features.h>

int
main(void)
{
#ifdef __GLIBC_PREREQ
    return __GLIBC_PREREQ(2,19);
#else
    return 0;
#endif
}
#endif

#ifdef TEST_LFS_WORKS

/* Return 0 when LFS is available and 1 otherwise.  */

#define _LARGEFILE_SOURCE
#define _LARGEFILE64_SOURCE
#define _LARGE_FILES
#define _FILE_OFFSET_BITS 64

#include <sys/types.h>
#include <sys/stat.h>
#include <assert.h>
#include <stdio.h>

#define OFF_T_64 (((off_t) 1 << 62) - 1 + ((off_t) 1 << 62))

int main(int argc, char **argv)
{

    /* Check that off_t can hold 2^63 - 1 and perform basic operations... */
    if (OFF_T_64 % 2147483647 != 1)
        return 1;

    /* stat breaks on SCO OpenServer */
    struct stat buf;
    stat(argv[0], &buf);
    if (!S_ISREG(buf.st_mode))
        return 2;

    FILE *file = fopen(argv[0], "r");
    off_t offset = ftello(file);
    fseek(file, offset, SEEK_CUR);
    fclose(file);
    return 0;
}
#endif

#ifdef GETTIMEOFDAY_GIVES_TZ
#include <time.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
int main(void)
{
    struct timeval tv;
    struct timezone tz;

    tz.tz_minuteswest = 7777;  /* Initialize to an unreasonable number */
    tz.tz_dsttime = 7;

    gettimeofday(&tv, &tz);

    /* Check whether the function returned any value at all */
    if (tz.tz_minuteswest == 7777 && tz.tz_dsttime == 7)
        return 1;
    else
        return 0;
}
#endif

#ifdef HAVE_IOEO

#include <windows.h>
typedef void (WINAPI *PGNSI)(LPSYSTEM_INFO);
int main ()
{
    PGNSI pGNSI;

    pGNSI = (PGNSI) GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")), "InitOnceExecuteOnce");

    if (NULL == pGNSI)
        return 1;
    else
        return 0;
}

#endif /* HAVE_IOEO */
