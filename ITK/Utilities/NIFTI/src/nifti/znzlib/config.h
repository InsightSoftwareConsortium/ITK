#ifndef _ZNZCONFIG_H_
#define _ZNZCONFIG_H_

/*
config.h - for znzlib


*****            This code is released to the public domain.            *****

*****  Author: Mark Jenkinson, FMRIB Centre, University of Oxford       *****
*****  Date:   September 2004                                           *****

*****  Neither the FMRIB Centre, the University of Oxford, nor any of   *****
*****  its employees imply any warranty of usefulness of this software  *****
*****  for any purpose, and do not assume any liability for damages,    *****
*****  incidental or otherwise, caused by any use of this document.     *****

*/

/*

This file contains all the configuration variables for znzlib

*/


/* comment out the following line if compression support is not wanted */
/* #define HAVE_ZLIB   -- this is now defined via -DHAVE_ZLIB in Makefile */

/* uncomment the following line is fdopen() exists for your compiler and compiler options */
/* #define HAVE_FDOPEN */

/* for _ZNZCONFIG_H */
#endif
