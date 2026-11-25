#ifndef VOL_IO_INTERNAL_VOLUME_IO_H
#define VOL_IO_INTERNAL_VOLUME_IO_H

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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#ifndef  VIOAPI
#if defined(_MSC_VER)
#define  VIOAPI /*__declspec(dllexport)*/
#else
#define  VIOAPI
#endif /* _MSC_VER not defined */
#endif /* VIOAPI not defined */

#include  <volume_io.h>

#endif
