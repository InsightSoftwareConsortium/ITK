/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2013, 2012, 2011, 2010, 2009  University of Chicago
  Copyright (C) 2008, 2007, 2006, 2005  Gordon Kindlmann
  Copyright (C) 2004, 2003, 2002, 2001, 2000, 1999, 1998  University of Utah

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any
  damages arising from the use of this software.

  Permission is granted to anyone to use this software for any
  purpose, including commercial applications, and to alter it and
  redistribute it freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must
     not claim that you wrote the original software. If you use this
     software in a product, an acknowledgment in the product
     documentation would be appreciated but is not required.

  2. Altered source versions must be plainly marked as such, and must
     not be misrepresented as being the original software.

  3. This notice may not be removed or altered from any source distribution.
*/

#include "NrrdIO.h"
#include "privateNrrd.h"

/* learned: /usr/bin/c++ on mac (at least) won't actually put a
const int blah[] array in an object file if it hasn't been declared
as "extern" */

const char
nrrdTypePrintfStr[NRRD_TYPE_MAX+1][AIR_STRLEN_SMALL] = {
  "%*d",          /* nrrdTypeUnknown: what else?  the effect will be
                    "skip" for sscanf, and "minimum precision" for printf */
  "%d",           /* nrrdTypeChar: char */
  "%u",           /* nrrdTypeUChar: unsigned char */
  "%hd",          /* nrrdTypeShort: short */
  "%hu",          /* nrrdTypeUShort: unsigned short */
  "%d",           /* nrrdTypeInt: int */
  "%u",           /* nrrdTypeUInt: unsigned int */
  AIR_LLONG_FMT,  /* nrrdTypeLLong: long long */
  AIR_ULLONG_FMT, /* nrrdTypeULLong: unsigned long long */
  "%f",           /* nrrdTypeFloat: float */
  "%lf",          /* nrrdTypeDouble: double */
  "%*d"           /* nrrdTypeBlock: what else? */
};

/*
** the setting of NRRD_TYPE_BIGGEST has to be in accordance with this
*/
const size_t
nrrdTypeSize[NRRD_TYPE_MAX+1] = {
  0,  /* nrrdTypeUnknown: unknown */
  1,  /* nrrdTypeChar: char */
  1,  /* nrrdTypeUChar: unsigned char */
  2,  /* nrrdTypeShort: short */
  2,  /* nrrdTypeUShort: unsigned short */
  4,  /* nrrdTypeInt: int */
  4,  /* nrrdTypeUInt: unsigned int */
  8,  /* nrrdTypeLLong: long long */
  8,  /* nrrdTypeULLong: unsigned long long */
  4,  /* nrrdTypeFloat: float */
  8,  /* nrrdTypeDouble: double */
  0   /* nrrdTypeBlock: effectively unknown; user has to set explicitly */
};

const int
nrrdTypeIsIntegral[NRRD_TYPE_MAX+1] = {
  0,  /* nrrdTypeUnknown: unknown */
  1,  /* nrrdTypeChar: char */
  1,  /* nrrdTypeUChar: unsigned char */
  1,  /* nrrdTypeShort: short */
  1,  /* nrrdTypeUShort: unsigned short */
  1,  /* nrrdTypeInt: int */
  1,  /* nrrdTypeUInt: unsigned int */
  1,  /* nrrdTypeLLong: long long */
  1,  /* nrrdTypeULLong: unsigned long long */
  0,  /* nrrdTypeFloat: float */
  0,  /* nrrdTypeDouble: double */
  1   /* nrrdTypeBlock: for some reason we pretend that blocks are integers */
};

const int
nrrdTypeIsUnsigned[NRRD_TYPE_MAX+1] = {
  0,  /* nrrdTypeUnknown: unknown */
  0,  /* nrrdTypeChar: char */
  1,  /* nrrdTypeUChar: unsigned char */
  0,  /* nrrdTypeShort: short */
  1,  /* nrrdTypeUShort: unsigned short */
  0,  /* nrrdTypeInt: int */
  1,  /* nrrdTypeUInt: unsigned int */
  0,  /* nrrdTypeLLong: long long */
  1,  /* nrrdTypeULLong: unsigned long long */
  0,  /* nrrdTypeFloat: float */
  0,  /* nrrdTypeDouble: double */
  0   /* nrrdTypeBlock: for some reason we pretend that blocks are signed */
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
  0,                       /* nrrdTypeUnknown: unknown */
  SCHAR_MIN,               /* nrrdTypeChar: char */
  0,                       /* nrrdTypeUChar: unsigned char */
  SHRT_MIN,                /* nrrdTypeShort: short */
  0,                       /* nrrdTypeUShort: unsigned short */
  INT_MIN,                 /* nrrdTypeInt: int */
  0,                       /* nrrdTypeUInt: unsigned int */
  (double)NRRD_LLONG_MIN,  /* nrrdTypeLLong: long long */
  0,                       /* nrrdTypeULLong: unsigned long long */
  0,                       /* nrrdTypeFloat: float */
  0,                       /* nrrdTypeDouble: double */
  0                        /* nrrdTypeBlock: punt */
},
nrrdTypeMax[NRRD_TYPE_MAX+1] = {
  0,                       /* nrrdTypeUnknown: unknown */
  SCHAR_MAX,               /* nrrdTypeChar: char */
  UCHAR_MAX,               /* nrrdTypeUChar: unsigned char */
  SHRT_MAX,                /* nrrdTypeShort: short */
  USHRT_MAX,               /* nrrdTypeUShort: unsigned short */
  INT_MAX,                 /* nrrdTypeInt: int */
  UINT_MAX,                /* nrrdTypeUInt: unsigned int */
  (double)NRRD_LLONG_MAX,  /* nrrdTypeLLong: long long */
  (double)NRRD_ULLONG_MAX, /* nrrdTypeULLong: unsigned long long */
  0,                       /* nrrdTypeFloat: float */
  0,                       /* nrrdTypeDouble: double */
  0                        /* nrrdTypeBlock: punt */
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
  1, /* nrrdField_keyvalue */
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

