#ifndef VOL_IO_SYSTEM_DEPENDENT_H
#define VOL_IO_SYSTEM_DEPENDENT_H

/* ----------------------------------------------------------------------------
@COPYRIGHT  :
              Copyright 1993,1994,1995 David MacDonald,
              McConnell Brain Imaging Centre,
              Montreal Neurological Institute, McGill University.
              Permission to use, copy, modify, and distribute this
              software and its documentation for any purpose and without
              fee is hereby granted, provided that the above copyright
              notice appear in all copies.  The author and McGill University
              make no representations about the suitability of this
              software for any purpose.  It is provided "as is" without
              express or implied warranty.
---------------------------------------------------------------------------- */

#if HAVE_FLOAT_H
#include <float.h>

#else

#if HAVE_VALUES_H
#include <values.h>

#ifndef DBL_MAX
#define DBL_MAX  MAXDOUBLE
#endif  /* DBL_MAX not defined */

#endif /* HAVE_VALUES_H */
#endif /* HAVE_FLOAT_H */

#include <stdlib.h>

#ifndef  EXIT_FAILURE
#define  EXIT_FAILURE  1
#endif

#ifndef  EXIT_SUCCESS
#define  EXIT_SUCCESS  0
#endif

#ifdef __cplusplus
#define  ASSIGN_PTR( ptr )  (*((void **) &(ptr)))
#else
#define  ASSIGN_PTR( ptr )  (ptr)
#endif

/* To allow gcc and clang to warn upon mismatch of format string and parameters. */
#if defined(__GNUC__) && (__GNUC__ >= 4)
#define VIO_FORMAT_FUNCTION(func, p1, p2) __attribute__((format(func, p1, p2)))
#else
#define VIO_FORMAT_FUNCTION(func, p1, p2)
#endif

#endif
