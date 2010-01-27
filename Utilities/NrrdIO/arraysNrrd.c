/*
  Teem: Tools to process and visualize scientific data and images              
  Copyright (C) 2008, 2007, 2006, 2005  Gordon Kindlmann
  Copyright (C) 2004, 2003, 2002, 2001, 2000, 1999, 1998  University of Utah

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  (LGPL) as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.
  The terms of redistributing and/or modifying this software also
  include exceptions to the LGPL that facilitate static linking.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this library; if not, write to Free Software Foundation, Inc.,
  51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "NrrdIO.h"
#include "privateNrrd.h"

/* learned: /usr/bin/c++ on mac (at least) won't actually put a 
const int blah[] array in an object file if it hasn't been declared
as "extern" */

const char
nrrdTypePrintfStr[NRRD_TYPE_MAX+1][AIR_STRLEN_SMALL] = {
  "%*d",  /* what else? sscanf: skip, printf: use "minimum precision" */
  "%d",
  "%u",
  "%hd",
  "%hu",
  "%d",
  "%u",
  AIR_LLONG_FMT,
  AIR_ULLONG_FMT,
  "%f",
  "%lf",
  "%*d"  /* what else? */
};

/*
** the setting of NRRD_TYPE_BIGGEST has to be in accordance with this
*/
const size_t
nrrdTypeSize[NRRD_TYPE_MAX+1] = {
  0,  /* unknown */
  1,  /* char */
  1,  /* unsigned char */
  2,  /* short */
  2,  /* unsigned short */
  4,  /* int */
  4,  /* unsigned int */
  8,  /* long long */
  8,  /* unsigned long long */
  4,  /* float */
  8,  /* double */
  0   /* effectively unknown; user has to set explicitly */
};

const int 
nrrdTypeIsIntegral[NRRD_TYPE_MAX+1] = {
  0,  /* unknown */
  1,  /* char */
  1,  /* unsigned char */
  1,  /* short */
  1,  /* unsigned short */
  1,  /* int */
  1,  /* unsigned int */
  1,  /* long long */
  1,  /* unsigned long long */
  0,  /* float */
  0,  /* double */
  1   /* for some reason we pretend that blocks are integers */
};

const int 
nrrdTypeIsUnsigned[NRRD_TYPE_MAX+1] = {
  0,  /* unknown */
  0,  /* char */
  1,  /* unsigned char */
  0,  /* short */
  1,  /* unsigned short */
  0,  /* int */
  1,  /* unsigned int */
  0,  /* long long */
  1,  /* unsigned long long */
  0,  /* float */
  0,  /* double */
  0   /* for some reason we pretend that blocks are signed */
};

/*
******** nrrdTypeMin[]
******** nrrdTypeMax[]
**
** only intended for small (<= 32 bits) integral types,
** so that we know how to "unquantize" integral values.
** A 64-bit double can correctly store the 32-bit integral
** mins and maxs, but gets the last few places wrong in the
** 64-bit mins and max.
*/
const double
nrrdTypeMin[NRRD_TYPE_MAX+1] = {
  0,                       /* unknown */
  SCHAR_MIN,               /* char */
  0,                       /* unsigned char */
  SHRT_MIN,                /* short */
  0,                       /* unsigned short */
  INT_MIN,                 /* int */
  0,                       /* unsigned int */
  (double)NRRD_LLONG_MIN,  /* long long */
  0,                       /* unsigned long long */
  0,                       /* float */
  0,                       /* double */
  0                        /* punt */
},
nrrdTypeMax[NRRD_TYPE_MAX+1] = {
  0,                       /* unknown */
  SCHAR_MAX,               /* char */
  UCHAR_MAX,               /* unsigned char */
  SHRT_MAX,                /* short */
  USHRT_MAX,               /* unsigned short */
  INT_MAX,                 /* int */
  UINT_MAX,                /* unsigned int */
  (double)NRRD_LLONG_MAX,  /* long long */
  (double)NRRD_ULLONG_MAX, /* unsigned long long */
  0,                       /* float */
  0,                       /* double */
  0                        /* punt */
};

/*
******** nrrdTypeNumberOfValues[]
**
** only meaningful for integral values, and only correct for
** 32-bit values; tells the number of different integral values that
** can be represented by the type
*/
const double
nrrdTypeNumberOfValues[NRRD_TYPE_MAX+1] = {
  0,                         /* unknown */
  UCHAR_MAX+1,               /* char */
  UCHAR_MAX+1,               /* unsigned char */
  USHRT_MAX+1,               /* short */
  USHRT_MAX+1,               /* unsigned short */
  (double)UINT_MAX+1,        /* int */
  (double)UINT_MAX+1,        /* unsigned int */
  (double)NRRD_ULLONG_MAX+1, /* long long */
  (double)NRRD_ULLONG_MAX+1, /* unsigned long long */
  0,                         /* float */
  0,                         /* double */
  0                          /* punt */
};

/*
** _nrrdFieldValidInImage[]
**
** these fields are valid embedded in PNM and PNG comments
** This does NOT include the fields who's values are constrained
** by the image format (and in the case of PNM, magic) itself.
*/
const int
_nrrdFieldValidInImage[NRRD_FIELD_MAX+1] = {
  0, /* nrrdField_unknown */
  1, /* nrrdField_comment */
  1, /* nrrdField_content */
  0, /* nrrdField_number */
  0, /* nrrdField_type */
  0, /* nrrdField_block_size */
  0, /* nrrdField_dimension */
  1, /* nrrdField_space */
  1, /* nrrdField_space_dimension */
  0, /* nrrdField_sizes */
  1, /* nrrdField_spacings */
  1, /* nrrdField_thicknesses */
  1, /* nrrdField_axis_mins */
  1, /* nrrdField_axis_maxs */
  1, /* nrrdField_space_directions */
  1, /* nrrdField_centers */
  1, /* nrrdField_kinds */
  1, /* nrrdField_labels */
  1, /* nrrdField_units */
  0, /* nrrdField_min */
  0, /* nrrdField_max */
  1, /* nrrdField_old_min */
  1, /* nrrdField_old_max */
  0, /* nrrdField_endian */
  0, /* nrrdField_encoding */
  0, /* nrrdField_line_skip */
  0, /* nrrdField_byte_skip */
  1, /* nrrdField_keyvalue */
  1, /* nrrdField_sample_units */
  1, /* nrrdField_space_units */
  1, /* nrrdField_space_origin */
  1, /* nrrdField_measurement_frame */
  0  /* nrrdField_data_file */
};

/*
** _nrrdFieldOnePerAxis
** 
** whether or not you need one value per axis, like labels and spacings
*/
const int
_nrrdFieldOnePerAxis[NRRD_FIELD_MAX+1] = {
  0, /* nrrdField_unknown */
  0, /* nrrdField_comment */
  0, /* nrrdField_content */
  0, /* nrrdField_number */
  0, /* nrrdField_type */
  0, /* nrrdField_block_size */
  0, /* nrrdField_dimension */
  0, /* nrrdField_space */
  0, /* nrrdField_space_dimension */
  1, /* nrrdField_sizes */
  1, /* nrrdField_spacings */
  1, /* nrrdField_thicknesses */
  1, /* nrrdField_axis_mins */
  1, /* nrrdField_axis_maxs */
  1, /* nrrdField_space_directions */
  1, /* nrrdField_centers */
  1, /* nrrdField_kinds */
  1, /* nrrdField_labels */
  1, /* nrrdField_units */
  0, /* nrrdField_min */
  0, /* nrrdField_max */
  0, /* nrrdField_old_min */
  0, /* nrrdField_old_max */
  0, /* nrrdField_endian */
  0, /* nrrdField_encoding */
  0, /* nrrdField_line_skip */
  0, /* nrrdField_byte_skip */
  0, /* nrrdField_keyvalue */
  0, /* nrrdField_sample_units */
  0, /* nrrdField_space_units */
  0, /* nrrdField_space_origin */
  0, /* nrrdField_measurement_frame */
  0  /* nrrdField_data_file */
};

/*
** _nrrdFieldValidInText[]
** 
** these fields are valid embedded in plain text comments
** This does NOT include the fields who's values are constrained
** the plain text format itself.
*/
const int
_nrrdFieldValidInText[NRRD_FIELD_MAX+1] = {
  0, /* nrrdField_unknown */
  1, /* nrrdField_comment */
  1, /* nrrdField_content */
  0, /* nrrdField_number */
  0, /* nrrdField_type: decided AGAINST plain text holding general type 
        (but I forget why ...) */
  0, /* nrrdField_block_size */
  1, /* nrrdField_dimension: but can only be 1 or 2 */
  0, /* nrrdField_space */
  0, /* nrrdField_space_dimension */
  0, /* nrrdField_sizes */
  1, /* nrrdField_spacings */
  1, /* nrrdField_thicknesses */
  1, /* nrrdField_axis_mins */
  1, /* nrrdField_axis_maxs */
  1, /* nrrdField_space_directions */
  1, /* nrrdField_centers */
  1, /* nrrdField_kinds */
  1, /* nrrdField_labels */
  1, /* nrrdField_units */
  0, /* nrrdField_min */
  0, /* nrrdField_max */
  1, /* nrrdField_old_min */
  1, /* nrrdField_old_max */
  0, /* nrrdField_endian */
  0, /* nrrdField_encoding */
  0, /* nrrdField_line_skip */
  0, /* nrrdField_byte_skip */
  0, /* nrrdField_keyvalue */
  0, /* nrrdField_sample_units */
  0, /* nrrdField_space_units */
  0, /* nrrdField_space_origin */
  0, /* nrrdField_measurement_frame */
  0  /* nrrdField_data_file */
};

/*
** _nrrdFieldRequired[]
**
** regardless of whether its a nrrd, PNM, or plain text, these things
** need to be conveyed, either explicity or implicitly
*/
const int
_nrrdFieldRequired[NRRD_FIELD_MAX+1] = {
  0, /* "Ernesto \"Che\" Guevara" */
  0, /* "#" */
  0, /* nrrdField_content */
  0, /* nrrdField_number */
  1, /* nrrdField_type */
  0, /* nrrdField_block size */
  1, /* nrrdField_dimension */
  0, /* nrrdField_space */
  0, /* nrrdField_space_dimension */
  1, /* nrrdField_sizes */
  0, /* nrrdField_spacings */
  0, /* nrrdField_thicknesses */
  0, /* nrrdField_axis mins */
  0, /* nrrdField_axis maxs */
  0, /* nrrdField_space_directions */
  0, /* nrrdField_centers */
  0, /* nrrdField_kinds */
  0, /* nrrdField_labels */
  0, /* nrrdField_units */
  0, /* nrrdField_min */
  0, /* nrrdField_max */
  0, /* nrrdField_old min */
  0, /* nrrdField_old max */
  0, /* nrrdField_endian */
  1, /* nrrdField_encoding */
  0, /* nrrdField_line_skip */
  0, /* nrrdField_byte_skip */
  0, /* nrrdField_keyvalue */
  0, /* nrrdField_sample_units */
  0, /* nrrdField_space_units */
  0, /* nrrdField_space_origin */
  0, /* nrrdField_measurement_frame */
  0  /* nrrdField_data file */
};

