/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
#if defined (TRY_SYS_TIME_H)
#include <sys/time.h>
/* #include <time.h> */
#endif


#if defined (TRY_TIME_H)
#include <time.h>
#endif

int main(int argc, char **argv) {
	  struct timeval t1;
	  gettimeofday(&t1, 0x00);
	return 0;
}
