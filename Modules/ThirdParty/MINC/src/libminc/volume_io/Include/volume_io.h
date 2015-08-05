#ifndef VOL_IO_VOLUME_IO_H
#define VOL_IO_VOLUME_IO_H

#ifdef __cplusplus
extern "C" {
#endif

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
@VERSION    : $Header: /private-cvsroot/minc/volume_io/Include/volume_io.h,v 1.13 2005-05-19 21:19:27 bert Exp $
---------------------------------------------------------------------------- */

/* ----------------------------- MNI Header -----------------------------------
@NAME       : volume_io.h
@INPUT      : 
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Header for Volume IO API
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : July 15, 1991       David MacDonald
@MODIFIED   :  
---------------------------------------------------------------------------- */

#ifndef VIO_PREFIX_NAMES
#define VIO_PREFIX_NAMES 1      /* Don't allow old-fashioned namespace pollution */
#endif /* VIO_PREFIX_NAMES */

#include  <volume_io/basic.h>
#include  <volume_io/string_funcs.h>
#include  <volume_io/files.h>
#include  <volume_io/arrays.h>
#include  <volume_io/geom_structs.h>
#include  <volume_io/progress.h>
#include  <volume_io/geometry.h>
#include  <volume_io/volume.h>
#include  <volume_io/transforms.h>

#ifndef  VIOAPI
#define  VIOAPI /*TODO: Could be used for dll linking in Windows*/
#endif /* VIOAPI not defined */

#include  <volume_io/vol_io_prototypes.h>

#ifdef __cplusplus
}
#endif

#endif /* VOL_IO_VOLUME_IO_H */
