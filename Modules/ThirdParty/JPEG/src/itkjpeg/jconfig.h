/* jconfig.h --- source file edited by configure script */
/* see jconfig.txt for explanations */

#define HAVE_PROTOTYPES
#define HAVE_UNSIGNED_CHAR
#define HAVE_UNSIGNED_SHORT
#undef CHAR_IS_UNSIGNED
#ifndef HAVE_STDDEF_H
#define HAVE_STDDEF_H
#endif
#ifndef HAVE_STDLIB_H
#define HAVE_STDLIB_H
#endif
#undef NEED_BSD_STRINGS
#undef NEED_SYS_TYPES_H
#undef NEED_FAR_POINTERS
#undef NEED_SHORT_EXTERNAL_NAMES
/* Define this if you get warnings about undefined structures. */
#undef INCOMPLETE_TYPES_BROKEN

/* Define "boolean" as unsigned char, not int, on Windows systems. */
#ifdef _WIN32
#ifndef __RPCNDR_H__		/* don't conflict if rpcndr.h already read */
typedef unsigned char boolean;
#endif
#define HAVE_BOOLEAN		/* prevent jmorecfg.h from redefining it */
#endif

#ifdef JPEG_INTERNALS

#undef RIGHT_SHIFT_IS_UNSIGNED
#define INLINE
/* These are for configuring the JPEG memory manager. */
#undef DEFAULT_MAX_MEM
#undef NO_MKTEMP

#endif /* JPEG_INTERNALS */

#ifdef JPEG_CJPEG_DJPEG

#define BMP_SUPPORTED           /* BMP image file format */
#define GIF_SUPPORTED           /* GIF image file format */
#define PPM_SUPPORTED           /* PBMPLUS PPM/PGM image file format */
#undef RLE_SUPPORTED            /* Utah RLE image file format */
#define TARGA_SUPPORTED         /* Targa image file format */

#undef TWO_FILE_COMMANDLINE
#undef NEED_SIGNAL_CATCHER
#undef DONT_USE_B_MODE

/* Define this if you want percent-done progress reports from cjpeg/djpeg. */
#undef PROGRESS_REPORT

#endif /* JPEG_CJPEG_DJPEG */


/* names mangling */

#include "itk_jpeg_mangle.h"

#if defined(__BORLANDC__)
#pragma warn -8004 /* Disable "foo is assigned a value that is never used." */
#endif
