#ifndef ZNZLIB_H
#define ZNZLIB_H

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


/* include optional check for HAVE_FDOPEN here, from deleted config.h:

   uncomment the following line if fdopen() exists for your compiler and
   compiler options
*/
/* #define HAVE_FDOPEN */

#if defined(WIN32) || defined(WIN64) || defined(_WIN32) || defined(_WIN64) || defined(_MSVC) || defined(_MSC_VER)
#include <io.h>
#define fseek _fseeki64
#define ftell _ftelli64
#define znz_off_t long long
#elif defined(__APPLE__) || defined(__FreeBSD__)
#define znz_off_t off_t
#else
#include <unistd.h>
#include <sys/types.h>
#define znz_off_t off_t
#endif

#ifdef HAVE_ZLIB
#if defined(ITKZLIB) && !defined(ITK_USE_SYSTEM_ZLIB)
#include "itk_zlib.h"
#else
#include "zlib.h"
#endif
#endif

#ifndef ZNZ_API
  #if defined(_WIN32) || defined(__CYGWIN__)
    #if defined(ZNZ_BUILD_SHARED)
      #ifdef __GNUC__
        #define ZNZ_API __attribute__ ((dllexport))
      #else
        #define ZNZ_API __declspec( dllexport )
      #endif
    #elif defined(ZNZ_USE_SHARED)
      #ifdef __GNUC__
        #define ZNZ_API __attribute__ ((dllimport))
      #else
        #define ZNZ_API __declspec( dllimport )
      #endif
    #else
      #define ZNZ_API
    #endif
  #elif (defined(__GNUC__) && __GNUC__ >= 4) || defined(__clang__)
    #define ZNZ_API __attribute__ ((visibility ("default")))
  #else
    #define ZNZ_API
  #endif
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


/* int znz_isnull(znzFile f); */
/* int znzclose(znzFile f); */
#define znz_isnull(f) ((f) == NULL)
#define znzclose(f)   Xznzclose(&(f))

/* Note extra argument (use_compression) where
   use_compression==0 is no compression
   use_compression!=0 uses zlib (gzip) compression
*/

ZNZ_API znzFile znzopen(const char *path, const char *mode, int use_compression);

#ifdef COMPILE_NIFTIUNUSED_CODE
ZNZ_API znzFile znzdopen(int fd, const char *mode, int use_compression);
#endif

ZNZ_API int Xznzclose(znzFile * file);

ZNZ_API size_t znzread(void* buf, size_t size, size_t nmemb, znzFile file);

ZNZ_API size_t znzwrite(const void* buf, size_t size, size_t nmemb, znzFile file);

ZNZ_API znz_off_t znzseek(znzFile file, znz_off_t offset, int whence);

ZNZ_API int znzrewind(znzFile stream);

ZNZ_API znz_off_t znztell(znzFile file);

ZNZ_API int znzputs(const char *str, znzFile file);

#ifdef COMPILE_NIFTIUNUSED_CODE
ZNZ_API char * znzgets(char* str, int size, znzFile file);

ZNZ_API int znzputc(int c, znzFile file);

ZNZ_API int znzgetc(znzFile file);

#if !defined(WIN32)
ZNZ_API int znzprintf(znzFile stream, const char *format, ...);
#endif
#endif

/*=================*/
#ifdef  __cplusplus
}
#endif
/*=================*/

#endif
