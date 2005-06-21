#ifndef _ZNZLIB_H_
#define _ZNZLIB_H_

/*
znzlib.h  (zipped or non-zipped library)

*****            This code is released to the public domain.            *****

*****  Author: Mark Jenkinson, FMRIB Centre, University of Oxford       *****
*****  Date:   September 2004                                           *****

*****  Neither the FMRIB Centre, the University of Oxford, nor any of   *****
*****  its employees imply any warranty of usefulness of this software  *****
*****  for any purpose, and do not assume any liability for damages,    *****
*****  incidental or otherwise, caused by any use of this document.     *****

*/

/*

This library provides an interface to both compressed (gzip/zlib) and
uncompressed (normal) file IO.  The functions are written to have the
same interface as the standard file IO functions.  

To use this library instead of normal file IO, the following changes
are required:
 - replace all instances of FILE* with znzFile
 - change the name of all function calls, replacing the initial character
   f with the znz  (e.g. fseek becomes znzseek)
 - add a third parameter to all calls to znzopen (previously fopen)
   that specifies whether to use compression (1) or not (0)
 - use znz_isnull rather than any (pointer == NULL) comparisons in the code
 
NB: seeks for writable files with compression are quite restricted

*/


/*=================*/
#ifdef  __cplusplus
extern "C" {
#endif
/*=================*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "config.h"

#ifdef HAVE_ZLIB 
#include "zlib.h"
#endif


struct znzptr {
  int withz;
  FILE* nzfptr;
#ifdef HAVE_ZLIB
  gzFile zfptr;
#endif
} ;

/* the type for all file pointers */
typedef struct znzptr * znzFile;
typedef struct znzptr * const znzFileConst;


/* int znz_isnull(znzFile f); */
/* int znzclose(znzFile f); */
#define znz_isnull(f) ((f) == NULL)
#define znzclose(f)   Xznzclose(&(f))

/* Note extra argument (use_compression) where 
   use_compression==0 is no compression
   use_compression!=0 uses zlib (gzip) compression
*/

znzFile znzopen(const char * const path, const char * const mode, const int use_compression);

znzFile znzdopen(const int fd, const char * const mode, const int use_compression);

int Xznzclose(znzFile * file);

size_t znzread(void* const buf, const size_t size, const size_t nmemb, znzFileConst file);

size_t znzwrite(void* const buf, const size_t size, const size_t nmemb, znzFileConst file);

long znzseek(znzFileConst file, const long offset, const int whence);

int znzrewind(znzFileConst stream);

long znztell(znzFileConst file);

int znzputs(char const * const str, znzFileConst file);

char * znzgets(char * const str, const int size, znzFileConst file);

int znzputc(const int c, znzFileConst file);

int znzgetc(znzFileConst file);

#if !defined(WIN32)
int znzprintf(znzFileConst stream, const char * const format, ...);
#endif

/*=================*/
#ifdef  __cplusplus
}
#endif
/*=================*/

#endif

