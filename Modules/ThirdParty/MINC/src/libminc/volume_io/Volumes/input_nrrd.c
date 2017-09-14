/**
 * \file Reader for NRRD format files.
 * TODO/WARNING: Like the NIfTI and MGH readers, this reader is somewhat
 * hard-wired to assume the XYZTV dimension ordering used by volume_io
 * by default. Generalizing it to support other possible orderings is
 * an exercise left to future generations...
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include  <internal_volume_io.h>

#include <stdio.h>

#if HAVE_STRING_H
#include <string.h>
#endif

#if HAVE_STRINGS_H
#include <strings.h>
#endif

#include <unistd.h>
#include <ctype.h>
#include <stdint.h>             /* for int32_t, etc. */
#include <zlib.h>

#include "input_nrrd.h"

#define NUM_BYTE_VALUES (UCHAR_MAX + 1)

#define NRRD_MAX_DIMS VIO_MAX_DIMENSIONS
#define NRRD_MAX_LINE 2048
#define NRRD_MAX_PATH 2048
#define NRRD_MAX_SPACE 3

typedef float float32_t;
typedef double float64_t;

typedef enum nrrd_type {
  NRRD_TYPE_NONE,
  NRRD_TYPE_INT8,
  NRRD_TYPE_UINT8,
  NRRD_TYPE_INT16,
  NRRD_TYPE_UINT16,
  NRRD_TYPE_INT32,
  NRRD_TYPE_UINT32,
  NRRD_TYPE_INT64,
  NRRD_TYPE_UINT64,
  NRRD_TYPE_FLOAT32,
  NRRD_TYPE_FLOAT64
} nrrd_type_t;

typedef enum nrrd_encoding {
  NRRD_ENCODING_NONE,
  NRRD_ENCODING_RAW,
  NRRD_ENCODING_ASCII,
  NRRD_ENCODING_HEX,
  NRRD_ENCODING_GZIP,
  NRRD_ENCODING_BZIP2
} nrrd_encoding_t;

typedef enum nrrd_endian {
  NRRD_ENDIAN_NONE,
  NRRD_ENDIAN_LITTLE,
  NRRD_ENDIAN_BIG
} nrrd_endian_t;

typedef enum nrrd_space {
  NRRD_SPACE_NONE,
  NRRD_SPACE_RAS,
  NRRD_SPACE_LAS,
  NRRD_SPACE_LPS
} nrrd_space_t;

typedef enum nrrd_kind {
  NRRD_KIND_NONE,
  NRRD_KIND_DOMAIN,             /* generic domain */
  NRRD_KIND_SPACE,              /* spatial domain */
  NRRD_KIND_TIME,               /* temporal domain */
  NRRD_KIND_LIST,               /* non-scalar */
  NRRD_KIND_POINT,              /* point (list) */
  NRRD_KIND_VECTOR,             /* vector (list) */
  NRRD_KIND_COVARIANT_VECTOR,   /* covariant vector (list)*/
  NRRD_KIND_NORMAL,             /* normal vector (list) */
  NRRD_KIND_STUB,               /* ??? (1) */
  NRRD_KIND_SCALAR,             /* explicit scalar (1) */
  NRRD_KIND_COMPLEX,            /* axis of length 2, real/imag (2) */
  NRRD_KIND_2_VECTOR,           /* Any 2-vector (2) */
  NRRD_KIND_3_COLOR,            /* generic 3-color (3) */
  NRRD_KIND_RGB_COLOR,          /* RGB 3-color (3) */
  NRRD_KIND_HSV_COLOR,          /* HSV 3-color (3) */
  NRRD_KIND_XYZ_COLOR,          /* XYZ 3-color (3) */
  NRRD_KIND_4_COLOR,            /* generic 4-color (4) */
  NRRD_KIND_RGBA_COLOR,         /* RGBA 4-color (4) */
  NRRD_KIND_3_VECTOR,           /* Any 3-vector (3) */
  NRRD_KIND_3_GRADIENT,         /* Covariant 3-vector (3) */
  NRRD_KIND_3_NORMAL,           /* Unit 3-vector (3) */
  NRRD_KIND_4_VECTOR,           /* Any 4-vector (4) */
  NRRD_KIND_QUATERNION,         /* Quaternion 4-vector WXYZ (4) */
  NRRD_KIND_2D_SYMMETRIC_MATRIX,        /*  (3) */
  NRRD_KIND_2D_MASKED_SYMMETRIC_MATRIX, /*  (4) */
  NRRD_KIND_2D_MATRIX,                  /*  (4) */
  NRRD_KIND_2D_MASKED_MATRIX,           /*  (4) */
  NRRD_KIND_3D_SYMMETRIC_MATRIX,        /*  (6) */
  NRRD_KIND_3D_MASKED_SYMMETRIC_MATRIX, /*  (7) */
  NRRD_KIND_3D_MATRIX,                  /*  (9) */
  NRRD_KIND_3D_MASKED_MATRIX            /*  (10) */
} nrrd_kind_t;

typedef enum nrrd_units {       /* space units */
  NRRD_UNITS_NONE,
  NRRD_UNITS_MM
} nrrd_units_t;

typedef enum nrrd_centers {     /* voxel centers */
  NRRD_CENTER_NONE,
  NRRD_CENTER_CELL,
  NRRD_CENTER_NODE
} nrrd_centers_t;

typedef struct nrrd_header {
  int dimension;
  nrrd_type_t type;
  nrrd_encoding_t encoding;
  nrrd_endian_t endian;
  nrrd_space_t space;
  char data_file[NRRD_MAX_PATH+1];
  float space_origin[NRRD_MAX_SPACE];

  int sizes[NRRD_MAX_DIMS];
  nrrd_kind_t kinds[NRRD_MAX_DIMS];
  char *labels[NRRD_MAX_DIMS];
  float space_directions[NRRD_MAX_DIMS][NRRD_MAX_SPACE];
  float spacings[NRRD_MAX_DIMS];
  nrrd_units_t units[NRRD_MAX_SPACE];
  nrrd_centers_t centers[NRRD_MAX_DIMS];

  /* Stuff for reading/writing data */
  gzFile gzfp;
} *nrrd_header_t;

/**
 * Remove trailing whitespace from a string. Modifies the string
 * directly!! Any whitespace at the end of the string will be
 * replaced with ASCII NUL ('\0') characters.
 * \param str_ptr The string to modify.
 */
void
rstrip(char *str_ptr)
{
  char *p = str_ptr;
  while (*p != 0)
    p++;                        /* advance to end. */
  --p;                          /* back up one character. */
  while (p > str_ptr && isspace(*p))
  {
    *p-- = 0;                   /* replace spaces with null. */
  }
}

/**
 * Given a NRRD type, return the size in bytes of the type.
 * \param nrrd_type A NRRD type code.
 * \returns The number of bytes used to represent the type.
 */
static int
nrrd_type_to_size(nrrd_type_t nrrd_type)
{
  switch (nrrd_type)
  {
  case NRRD_TYPE_INT8:
  case NRRD_TYPE_UINT8:
    return 1;
  case NRRD_TYPE_INT16:
  case NRRD_TYPE_UINT16:
    return 2;
  case NRRD_TYPE_INT32:
  case NRRD_TYPE_UINT32:
  case NRRD_TYPE_FLOAT32:
    return 4;
  case NRRD_TYPE_INT64:
  case NRRD_TYPE_UINT64:
  case NRRD_TYPE_FLOAT64:
    return 8;
  default:
    return -1;
  }
}

/**
 * Given a NRRD type, return the canonical name of the type.
 * \param nrrd_type A NRRD type code.
 * \returns A string representation of the type, suitable for use in
 * a NRRD header.
 */
static const char *
nrrd_type_to_str(nrrd_type_t nrrd_type)
{
  switch (nrrd_type)
  {
  case NRRD_TYPE_NONE:
    break;
  case NRRD_TYPE_INT8:
    return "int8";
  case NRRD_TYPE_UINT8:
    return "uint8";
  case NRRD_TYPE_INT16:
    return "int16";
  case NRRD_TYPE_UINT16:
    return "uint16";
  case NRRD_TYPE_INT32:
    return "int32";
  case NRRD_TYPE_UINT32:
    return "uint32";
  case NRRD_TYPE_INT64:
    return "int64";
  case NRRD_TYPE_UINT64:
    return "uint64";
  case NRRD_TYPE_FLOAT32:
    return "float";
  case NRRD_TYPE_FLOAT64:
    return "double";
  }
  return "???";
}

/**
 * Given a string representation of a NRRD type, return the internal
 * integer code we use to represent the type.
 * \param str_ptr A NRRD type name.
 * \returns An integer representing the NRRD type.
 */
static nrrd_type_t
nrrd_str_to_type(const char *str_ptr)
{
  if (!strcasecmp(str_ptr, "signed char") ||
      !strcasecmp(str_ptr, "int8") ||
      !strcasecmp(str_ptr, "int8_t"))
  {
    return NRRD_TYPE_INT8;
  }
  else if (!strcasecmp(str_ptr, "unsigned char") ||
           !strcasecmp(str_ptr, "uchar") ||
           !strcasecmp(str_ptr, "uint8") ||
           !strcasecmp(str_ptr, "uint8_t"))
  {
    return NRRD_TYPE_UINT8;
  }
  else if (!strcasecmp(str_ptr, "short") ||
           !strcasecmp(str_ptr, "short int") ||
           !strcasecmp(str_ptr, "signed short") ||
           !strcasecmp(str_ptr, "signed short int") ||
           !strcasecmp(str_ptr, "int16") ||
           !strcasecmp(str_ptr, "int16_t"))
  {
    return NRRD_TYPE_INT16;
  }
  else if (!strcasecmp(str_ptr, "ushort") ||
           !strcasecmp(str_ptr, "unsigned short") ||
           !strcasecmp(str_ptr, "unsigned short int") ||
           !strcasecmp(str_ptr, "uint16") ||
           !strcasecmp(str_ptr, "uint16_t"))
  {
    return NRRD_TYPE_UINT16;
  }
  else if (!strcasecmp(str_ptr, "int") ||
           !strcasecmp(str_ptr, "signed int") ||
           !strcasecmp(str_ptr, "int32") ||
           !strcasecmp(str_ptr, "int32_t"))
  {
    return NRRD_TYPE_INT32;
  }
  else if (!strcasecmp(str_ptr, "uint") ||
           !strcasecmp(str_ptr, "unsigned int") ||
           !strcasecmp(str_ptr, "uint32") ||
           !strcasecmp(str_ptr, "uint32_t"))
  {
    return NRRD_TYPE_UINT32;
  }
  else if (!strcasecmp(str_ptr, "longlong") ||
           !strcasecmp(str_ptr, "long long") ||
           !strcasecmp(str_ptr, "long long int") ||
           !strcasecmp(str_ptr, "signed long long int") ||
           !strcasecmp(str_ptr, "int64") ||
           !strcasecmp(str_ptr, "int64_t"))
  {
    return NRRD_TYPE_INT64;
  }
  else if (!strcasecmp(str_ptr, "ulonglong") ||
           !strcasecmp(str_ptr, "unsigned long long") ||
           !strcasecmp(str_ptr, "unsigned long long int") ||
           !strcasecmp(str_ptr, "uint64") ||
           !strcasecmp(str_ptr, "uint64_t"))
  {
    return NRRD_TYPE_UINT64;
  }
  else if (!strcasecmp(str_ptr, "float"))
  {
    return NRRD_TYPE_FLOAT32;
  }
  else if (!strcasecmp(str_ptr, "double"))
  {
    return NRRD_TYPE_FLOAT64;
  }
  else
  {
    return -1;
  }
}

/**
 * Given an integer representing a NRRD encoding, return a string
 * suitable for use in the NRRD header.
 * \param nrrd_encoding An integer encoding type.
 * \returns An string representing the NRRD encoding.
 */
static const char *
nrrd_encoding_to_str(nrrd_encoding_t nrrd_encoding)
{
  switch (nrrd_encoding)
  {
  case NRRD_ENCODING_NONE:
    break;
  case NRRD_ENCODING_RAW:
    return "raw";
  case NRRD_ENCODING_ASCII:
    return "ascii";
  case NRRD_ENCODING_HEX:
    return "hex";
  case NRRD_ENCODING_GZIP:
    return "gzip";
  case NRRD_ENCODING_BZIP2:
    return "bzip2";
  }
  return "???";
}

/**
 * Given a string representation of a NRRD encoding, return the internal
 * integer code we use to represent the encoding.
 * \param str_ptr A NRRD encoding name.
 * \returns An integer representing the NRRD encoding.
 */
static nrrd_encoding_t
nrrd_str_to_encoding(const char *str_ptr)
{
  if (!strcasecmp(str_ptr, "raw"))
  {
    return NRRD_ENCODING_RAW;
  }
  else if (!strcasecmp(str_ptr, "txt") ||
           !strcasecmp(str_ptr, "text") ||
           !strcasecmp(str_ptr, "ascii"))
  {
    return NRRD_ENCODING_ASCII;
  }
  else if (!strcasecmp(str_ptr, "hex"))
  {
    return NRRD_ENCODING_HEX;
  }
  else if (!strcasecmp(str_ptr, "gz") || !strcasecmp(str_ptr, "gzip"))
  {
    return NRRD_ENCODING_GZIP;
  }
  else if (!strcasecmp(str_ptr, "bz2") || !strcasecmp(str_ptr, "bzip2"))
  {
    return NRRD_ENCODING_BZIP2;
  }
  else
  {
    return -1;
  }
}

/**
 * Given an integer representing a NRRD endian type, return a string
 * suitable for use in the NRRD header.
 * \param nrrd_endian An integer endian type.
 * \returns An string representing the NRRD endianness.
 */
static const char *
nrrd_endian_to_str(nrrd_endian_t nrrd_endian)
{
  switch (nrrd_endian)
  {
  case NRRD_ENDIAN_NONE:
    break;
  case NRRD_ENDIAN_LITTLE:
    return "little";
  case NRRD_ENDIAN_BIG:
    return "big";
  }
  return "???";
}

/**
 * Given an integer representing a NRRD space, return a string
 * suitable for use in the NRRD header.
 * \param nrrd_space Internal code for the space.
 * \returns A string representation of the NRRD space.
 */
static const char *
nrrd_space_to_str(nrrd_space_t nrrd_space)
{
  switch (nrrd_space)
  {
  case NRRD_SPACE_NONE:
    break;
  case NRRD_SPACE_RAS:
    return "RAS";
  case NRRD_SPACE_LAS:
    return "LAS";
  case NRRD_SPACE_LPS:
    return "LPS";
  }
  return "???";
}

/**
 * Given an integer representing a NRRD dimension 'kind', return
 * a boolean indicating whether this kind is associated with spatial axes.
 * \param nrrd_kind The NRRD kind code for this dimension.
 * \returns TRUE if this kind is spatial.
 */
static VIO_BOOL
nrrd_is_spatial_kind(nrrd_kind_t nrrd_kind)
{
  return (nrrd_kind == NRRD_KIND_DOMAIN || nrrd_kind == NRRD_KIND_SPACE);
}

/**
 * Given an integer representing a NRRD dimension 'kind', return
 * a string representing this kind, suitable for use in a NRRD header.
 * \param nrrd_kind The NRRD kind code for this dimension.
 * \returns A string representation of the kind.
 */
static const char *
nrrd_kind_to_str(nrrd_kind_t nrrd_kind)
{
  switch (nrrd_kind)
  {
  case NRRD_KIND_NONE:
    return "none";
  case NRRD_KIND_DOMAIN:
    return "domain";
  case NRRD_KIND_SPACE:
    return "space";
  case NRRD_KIND_TIME:
    return "time";
  case NRRD_KIND_LIST:
    return "list";
  case NRRD_KIND_POINT:
    return "point";
  case NRRD_KIND_VECTOR:
    return "vector";
  case NRRD_KIND_COVARIANT_VECTOR:
    return "covariant-vector";
  case NRRD_KIND_NORMAL:
    return "normal";
  case NRRD_KIND_STUB:
    return "stub";
  case NRRD_KIND_SCALAR:
    return "scalar";
  case NRRD_KIND_COMPLEX:
    return "complex";
  case NRRD_KIND_2_VECTOR:
    return "2-vector";
  case NRRD_KIND_3_COLOR:
    return "3-color";
  case NRRD_KIND_RGB_COLOR:
    return "RGB-color";
  case NRRD_KIND_HSV_COLOR:
    return "HSV-color";
  case NRRD_KIND_XYZ_COLOR:
    return "XYZ-color";
  case NRRD_KIND_4_COLOR:
    return "4-color";
  case NRRD_KIND_RGBA_COLOR:
    return "RGBA-color";
  case NRRD_KIND_3_VECTOR:
    return "3-vector";
  case NRRD_KIND_3_GRADIENT:
    return "3-gradient";
  case NRRD_KIND_3_NORMAL:
    return "3-normal";
  case NRRD_KIND_4_VECTOR:
    return "4-vector";
  case NRRD_KIND_QUATERNION:
    return "quaternion";
  case NRRD_KIND_2D_SYMMETRIC_MATRIX:
    return "2D-symmetric-matrix";
  case NRRD_KIND_2D_MASKED_SYMMETRIC_MATRIX:
    return "2D-masked-symmetric-matrix";
  case NRRD_KIND_2D_MATRIX:
    return "2D-matrix";
  case NRRD_KIND_2D_MASKED_MATRIX:
    return "2D-masked-matrix";
  case NRRD_KIND_3D_SYMMETRIC_MATRIX:
    return "3D-symmetric-matrix";;
  case NRRD_KIND_3D_MASKED_SYMMETRIC_MATRIX:
    return "3D-masked-symmetric-matrix";
  case NRRD_KIND_3D_MATRIX:
    return "3D-matrix";
  case NRRD_KIND_3D_MASKED_MATRIX:
    return "3D-masked-matrix";
  }
  return "???";
}

/**
 * Given an string representing a NRRD dimension 'kind', return
 * an internal integer representing this kind.
 * \param str_ptr The NRRD kind string for this dimension.
 * \returns An integer representation of the kind.
 */
static int
nrrd_str_to_kind(const char *str_ptr)
{
  if (!strcasecmp(str_ptr, "domain"))
  {
    return NRRD_KIND_DOMAIN;
  }
  else if (!strcasecmp(str_ptr, "space"))
  {
    return NRRD_KIND_SPACE;
  }
  else if (!strcasecmp(str_ptr, "time"))
  {
    return NRRD_KIND_TIME;
  }
  else if (!strcasecmp(str_ptr, "list"))
  {
    return NRRD_KIND_LIST;
  }
  else if (!strcasecmp(str_ptr, "point"))
  {
    return NRRD_KIND_POINT;
  }
  else if (!strcasecmp(str_ptr, "vector"))
  {
    return NRRD_KIND_VECTOR;
  }
  else if (!strcasecmp(str_ptr, "covariant-vector"))
  {
    return NRRD_KIND_COVARIANT_VECTOR;
  }
  else if (!strcasecmp(str_ptr, "normal"))
  {
    return NRRD_KIND_NORMAL;
  }
  else if (!strcasecmp(str_ptr, "stub"))
  {
    return NRRD_KIND_STUB;
  }
  else if (!strcasecmp(str_ptr, "scalar"))
  {
    return NRRD_KIND_SCALAR;
  }
  else if (!strcasecmp(str_ptr, "complex"))
  {
    return NRRD_KIND_COMPLEX;
  }
  else if (!strcasecmp(str_ptr, "2-vector"))
  {
    return NRRD_KIND_2_VECTOR;
  }
  else if (!strcasecmp(str_ptr, "3-color"))
  {
    return NRRD_KIND_3_COLOR;
  }
  else if (!strcasecmp(str_ptr, "RGB-color") || !strcasecmp(str_ptr, "RGB"))
  {
    return NRRD_KIND_RGB_COLOR;
  }
  else if (!strcasecmp(str_ptr, "HSV-color"))
  {
    return NRRD_KIND_HSV_COLOR;
  }
  else if (!strcasecmp(str_ptr, "XYZ-color"))
  {
    return NRRD_KIND_XYZ_COLOR;
  }
  else if (!strcasecmp(str_ptr, "4-color"))
  {
    return NRRD_KIND_4_COLOR;
  }
  else if (!strcasecmp(str_ptr, "RGBA-color") || !strcasecmp(str_ptr, "RGBA"))
  {
    return NRRD_KIND_RGBA_COLOR;
  }
  else if (!strcasecmp(str_ptr, "3-vector"))
  {
    return NRRD_KIND_3_VECTOR;
  }
  else if (!strcasecmp(str_ptr, "3-gradient"))
  {
    return NRRD_KIND_3_GRADIENT;
  }
  else if (!strcasecmp(str_ptr, "3-normal"))
  {
    return NRRD_KIND_3_NORMAL;
  }
  else if (!strcasecmp(str_ptr, "4-vector"))
  {
    return NRRD_KIND_4_VECTOR;
  }
  else if (!strcasecmp(str_ptr, "quaternion"))
  {
    return NRRD_KIND_QUATERNION;
  }
  else if (!strcasecmp(str_ptr, "2D-symmetric-matrix"))
  {
    return NRRD_KIND_2D_SYMMETRIC_MATRIX;
  }
  else if (!strcasecmp(str_ptr, "2D-masked-symmetric-matrix"))
  {
    return NRRD_KIND_2D_MASKED_SYMMETRIC_MATRIX;
  }
  else if (!strcasecmp(str_ptr, "2D-matrix"))
  {
    return NRRD_KIND_2D_MATRIX;
  }
  else if (!strcasecmp(str_ptr, "2D-masked-matrix"))
  {
    return NRRD_KIND_2D_MASKED_MATRIX;
  }
  else if (!strcasecmp(str_ptr, "3D-symmetric-matrix"))
  {
    return NRRD_KIND_3D_SYMMETRIC_MATRIX;
  }
  else if (!strcasecmp(str_ptr, "3D-masked-symmetric-matrix"))
  {
    return NRRD_KIND_3D_MASKED_SYMMETRIC_MATRIX;
  }
  else if (!strcasecmp(str_ptr, "3D-matrix"))
  {
    return NRRD_KIND_3D_MATRIX;
  }
  else if (!strcasecmp(str_ptr, "3D-masked-matrix"))
  {
    return NRRD_KIND_3D_MASKED_MATRIX;
  }
  else if (!strcasecmp(str_ptr, "none") ||
           !strcasecmp(str_ptr, "???"))
  {
    return NRRD_KIND_NONE;
  }
  else
  {
    return -1;
  }
}

/**
 * Print a NRRD header.
 * \param fp The stream where we will write the header.
 * \param nrrd_ptr The internal representation of the header.
 */
static void
nrrd_print_header(FILE *fp, nrrd_header_t nrrd_ptr)
{
  int i;

  fprintf(fp, "NRRD0004\n");    /* Probably we are V4-compliant. */
  fprintf(fp, "# Complete NRRD file format specification at:\n");
  fprintf(fp, "# http://teem.sourceforge.net/nrrd/format.html\n");
  fprintf(fp, "# NRRD header written by volume_io.\n");
  if (nrrd_ptr->data_file[0] != 0)
    fprintf(fp, "data file: %s\n", nrrd_ptr->data_file);
  fprintf(fp, "dimension: %d\n", nrrd_ptr->dimension);
  fprintf(fp, "type: %s\n", nrrd_type_to_str(nrrd_ptr->type));
  fprintf(fp, "encoding: %s\n", nrrd_encoding_to_str(nrrd_ptr->encoding));
  if (nrrd_ptr->endian != 0)
    fprintf(fp, "endian: %s\n", nrrd_endian_to_str(nrrd_ptr->endian));
  if (nrrd_ptr->space != 0)
    fprintf(fp, "space: %s\n", nrrd_space_to_str(nrrd_ptr->space));
  fprintf(fp, "space origin: (%g,%g,%g)\n",
          nrrd_ptr->space_origin[0],
          nrrd_ptr->space_origin[1],
          nrrd_ptr->space_origin[2]);
  fprintf(fp, "sizes: ");
  for (i = 0; i < nrrd_ptr->dimension; i++)
  {
    fprintf(fp, "%d ", nrrd_ptr->sizes[i]);
  }
  fprintf(fp, "\n");
  fprintf(fp, "space directions: ");
  for (i = 0; i < nrrd_ptr->dimension; i++)
  {
    fprintf(fp, "(%g,%g,%g) ",
            nrrd_ptr->space_directions[i][0],
            nrrd_ptr->space_directions[i][1],
            nrrd_ptr->space_directions[i][2]);
  }
  fprintf(fp, "\n");
  fprintf(fp, "kinds: ");
  for (i = 0; i < nrrd_ptr->dimension; i++)
  {
    fprintf(fp, "%s ", nrrd_kind_to_str(nrrd_ptr->kinds[i]));
  }
  fprintf(fp, "\n");
  if (nrrd_ptr->labels[0] != NULL)
  {
    fprintf(fp, "labels: ");
    for (i = 0; i < nrrd_ptr->dimension; i++)
    {
      fprintf(fp, "\"%s\" ", nrrd_ptr->labels[i]);
    }
    fprintf(fp, "\n");
  }

  if (nrrd_ptr->spacings[0] != 0)
  {
    fprintf(fp, "spacings: ");
    for (i = 0; i < nrrd_ptr->dimension; i++)
    {
      fprintf(fp, "%g ", nrrd_ptr->spacings[i]);
    }
    fprintf(fp, "\n");
  }
  fprintf(fp, "\n");
}

/**
 * Compute the endian-ness of the current system.
 * \returns NRRD_ENDIAN_LITTLE on little-endian architectures,
 * NRRD_ENDIAN_BIG on big-endian architectures.
 */
static nrrd_endian_t
nrrd_get_system_endian()
{
  union
  {
    short s;
    unsigned char c[2];
  } v;
  v.s = 0x0201;
  return (v.c[1] == 2 && v.c[0] == 1) ? NRRD_ENDIAN_LITTLE : NRRD_ENDIAN_BIG;
}

/**
 * Read up to 'n_bytes' bytes of data from the NRRD file. Handles
 * differences in encodings and endianness.
 *
 * \param nrrd_ptr The internal representation of a NRRD header.
 * \param data_ptr Buffer where the data will be stored.
 * \param n_bytes The number of bytes requested.
 * \param fp A file pointer, open for read, associated with the
 * data of the NRRD.
 * \returns The number of bytes read, or zero on end of file.
 */
static int
nrrd_read_buffer(nrrd_header_t nrrd_ptr, unsigned char *data_ptr,
                 int n_bytes, FILE *fp)
{
  int n_bytes_read = 0;         /* our return value */
  int n_bytes_per_item;
  int n_items;
  int i;
  double dvalue;
  float fvalue;
  int ivalue;
  unsigned int uvalue;
  int n_converted;

  switch (nrrd_ptr->encoding)
  {
  case NRRD_ENCODING_RAW:
    n_bytes_read = fread(data_ptr, 1, n_bytes, fp);
    break;

  case NRRD_ENCODING_ASCII:
    /* This is crufty and slow, but then we don't expect to see any
     * large images with ASCII encoding, do we??
     */
    n_bytes_per_item = nrrd_type_to_size(nrrd_ptr->type);
    n_items = n_bytes / n_bytes_per_item;
    n_converted = 1;
    for (i = 0; i < n_items && n_converted > 0; i++)
    {
      switch (nrrd_ptr->type)
      {
      case NRRD_TYPE_UINT8:
        if ((n_converted = fscanf(fp, "%u", &uvalue)) == 1)
          ((unsigned char *) data_ptr)[i] = uvalue;
        break;
      case NRRD_TYPE_INT8:
        if ((n_converted = fscanf(fp, "%d", &ivalue)) == 1)
          ((signed char *) data_ptr)[i] = ivalue;
        break;
      case NRRD_TYPE_UINT16:
        if ((n_converted = fscanf(fp, "%u", &uvalue)) == 1)
          ((unsigned short *) data_ptr)[i] = uvalue;
        break;
      case NRRD_TYPE_INT16:
        if ((n_converted = fscanf(fp, "%d", &ivalue)) == 1)
          ((short *) data_ptr)[i] = ivalue;
        break;
      case NRRD_TYPE_UINT32:
        if ((n_converted = fscanf(fp, "%u", &uvalue)) == 1)
          ((uint32_t *) data_ptr)[i] = uvalue;
        break;
      case NRRD_TYPE_INT32:
        if ((n_converted = fscanf(fp, "%d", &ivalue)) == 1)
          ((int32_t *) data_ptr)[i] = ivalue;
        break;
      case NRRD_TYPE_FLOAT32:
        /* According to the NRRD specification, the values NAN and INF
         * are permitted for floating-point values. I've checked that 
         * at least the current version of fscanf() seems to handle
         * them correctly.
         */
        if ((n_converted = fscanf(fp, "%f", &fvalue)) == 1)
          ((float32_t *)data_ptr)[i] = fvalue;
        break;
      case NRRD_TYPE_FLOAT64:
        if ((n_converted = fscanf(fp, "%lf", &dvalue)) == 1)
          ((float64_t *)data_ptr)[i] = dvalue;
        break;
      default:
        break;
      }
      if (n_converted > 0)
        n_bytes_read += n_bytes_per_item * n_converted;
    }
    break;

  case NRRD_ENCODING_GZIP:
    if (nrrd_ptr->gzfp == NULL)
    {
      int fd = dup(fileno(fp));
      off_t off = ftell(fp);
      lseek(fd, off, SEEK_SET);
      nrrd_ptr->gzfp = gzdopen(fd, "r");
      if (nrrd_ptr->gzfp == Z_NULL)
      {
        return -1;
      }
    }
    n_bytes_read = gzread(nrrd_ptr->gzfp, data_ptr, n_bytes);
    break;

  default:
    print_error("ERROR: NRRD encoding '%s' is not implemented.\n",
                nrrd_encoding_to_str(nrrd_ptr->encoding));
    return -1;
  }

  /* We've read all of the data, now see if we have to swap bytes.
   */
  if ( nrrd_ptr->endian != nrrd_get_system_endian() )
  {
    int i;
    unsigned char temp;

    switch (nrrd_type_to_size( nrrd_ptr->type ))
    {
    case 1:
      /* Nothing to do. */
      break;

    case 2:
      for (i = 0; i < (int) n_bytes_read; i += 2)
      {
        temp = data_ptr[i + 0];
        data_ptr[i + 0] = data_ptr[i + 1];
        data_ptr[i + 1] = temp;
      }
      break;

    case 4:
      for (i = 0; i < (int) n_bytes_read; i += 4)
      {
        temp = data_ptr[i + 0];
        data_ptr[i + 0] = data_ptr[i + 3];
        data_ptr[i + 3] = temp;
        temp = data_ptr[i + 1];
        data_ptr[i + 1] = data_ptr[i + 2];
        data_ptr[i + 2] = temp;
      }
      break;

    case 8:
      for (i = 0; i < (int) n_bytes_read; i += 8)
      {
        temp = data_ptr[i + 0];
        data_ptr[i + 0] = data_ptr[i + 7];
        data_ptr[i + 7] = temp;
        temp = data_ptr[i + 1];
        data_ptr[i + 1] = data_ptr[i + 6];
        data_ptr[i + 6] = temp;
        temp = data_ptr[i + 2];
        data_ptr[i + 2] = data_ptr[i + 5];
        data_ptr[i + 5] = temp;
        temp = data_ptr[i + 3];
        data_ptr[i + 3] = data_ptr[i + 4];
        data_ptr[i + 4] = temp;
      }
      break;

    default:
      print_error("ERROR: Can't byte swap elements of type '%s'.\n",
                  nrrd_type_to_str(nrrd_ptr->type));
      break;
    }
  }
  return n_bytes_read;
}

/**
 * Scan through the NRRD data and determine the minimum and maximul voxel
 * values. 
 * \param nrrd_ptr The internal representation of the NRRD header.
 * \param fp A file pointer referencing the open data stream.
 * \param min_voxel A pointer to receive the minimum voxel value.
 * \param max_voxel A pointer to receive the maximum voxel value.
 */
static void
nrrd_find_data_range(nrrd_header_t nrrd_ptr, FILE *fp,
                     VIO_Real *min_voxel,
                     VIO_Real *max_voxel)
{
  unsigned char buffer[4096];
  off_t pos = ftell(fp);
  int i;
  int n_bytes;
  VIO_Real tmp;

  *min_voxel = DBL_MAX;
  *max_voxel = -DBL_MAX;

  while ((n_bytes = nrrd_read_buffer(nrrd_ptr, buffer, sizeof(buffer), fp)) > 0)
  {
    int n_items = n_bytes / nrrd_type_to_size(nrrd_ptr->type);

    for (i = 0; i < n_items; i++)
    {
      switch (nrrd_ptr->type)
      {
      case NRRD_TYPE_INT8:
        tmp = ((int8_t *)buffer)[i];
        break;
      case NRRD_TYPE_UINT8:
        tmp = ((uint8_t *)buffer)[i];
        break;
      case NRRD_TYPE_INT16:
        tmp = ((int16_t *)buffer)[i];
        break;
      case NRRD_TYPE_UINT16:
        tmp = ((uint16_t *)buffer)[i];
        break;
      case NRRD_TYPE_INT32:
        tmp = ((int32_t *)buffer)[i];
        break;
      case NRRD_TYPE_UINT32:
        tmp = ((uint32_t *)buffer)[i];
        break;
      case NRRD_TYPE_FLOAT32:
        tmp = ((float32_t *)buffer)[i];
        break;
      case NRRD_TYPE_FLOAT64:
        tmp = ((float64_t *)buffer)[i];
        break;
      default:
        print_error("Unsupported NRRD type %d\n", nrrd_ptr->type);
        break;
      }
      if (tmp < *min_voxel)
      {
        *min_voxel = tmp;
      }
      if (tmp > *max_voxel)
      {
        *max_voxel = tmp;
      }
    }
  }
  if (nrrd_ptr->encoding == NRRD_ENCODING_GZIP)
  {
    gzclose(nrrd_ptr->gzfp);
    nrrd_ptr->gzfp = NULL;
  }
  fseek(fp, pos, SEEK_SET);
}

/**
 * Read a NRRD header, converting it to an internal structure.
 *
 * \param fp A file pointer open for reading the header.
 * \param nrrd_ptr The structure to be filled in.
 * \returns VIO_OK if all goes well.
 */
static VIO_Status
nrrd_read_header(FILE *fp, nrrd_header_t nrrd_ptr)
{
  char line[NRRD_MAX_LINE + 1];
  char *str_ptr;
  int i;
  int space_found = 0;

  memset(nrrd_ptr, 0, sizeof(struct nrrd_header));

  if (fgets(line, NRRD_MAX_LINE, fp) == NULL ||
      strncmp(line, "NRRD000", 7) != 0)
  {
    return VIO_ERROR;
  }

  while (fgets(line, NRRD_MAX_LINE, fp) && !isspace(line[0]))
  {
    /* Ignore comments.
     */
    if (line[0] == '#')
    {
      continue;
    }

    /* Remove trailing white space.
     */
    rstrip(line);

    /* Find the colon separator. Ignore lines that don't have one.
     */
    if ((str_ptr = strchr(line, ':')) == NULL)
    {
      continue;
    }

    /* Now split the string at the colon, and advance str_ptr to 
     * the start of the arguments.
     */
    *str_ptr++ = '\0';
    while (isspace(*str_ptr) && *str_ptr != 0)
    {
      str_ptr++;
    }

    /* Now figure out which field we are dealing with.
     */
    if (!strcasecmp("dimension", line))
    {
      nrrd_ptr->dimension = strtol(str_ptr, NULL, 10);
      if (nrrd_ptr->dimension > NRRD_MAX_DIMS)
      {
        print_error("Too many NRRD dimensions: %d\n", nrrd_ptr->dimension);
        return VIO_ERROR;
      }
    }
    else if (!strcasecmp("content", line))
    {
      /* We just ignore content lines for now. The provide a comment
       * describing the data set.
       */
    }
    else if (!strcasecmp("measurement frame", line))
    {
      /* We just ignore content lines for now. The provide a comment
       * describing the data set.
       */
    }
    else if (!strcasecmp("data file", line) ||
             !strcasecmp("datafile", line))
    {
      strncpy(nrrd_ptr->data_file, str_ptr, NRRD_MAX_PATH);
      nrrd_ptr->data_file[NRRD_MAX_PATH] = 0;
    }
    else if (!strcasecmp("type", line))
    {
      nrrd_ptr->type = nrrd_str_to_type(str_ptr);
      if (nrrd_ptr->type <= 0)
      {
        print_error("Unknown NRRD type: '%s'\n", str_ptr);
        return VIO_ERROR;
      }
    }
    else if (!strcasecmp("encoding", line))
    {
      nrrd_ptr->encoding = nrrd_str_to_encoding(str_ptr);
      if (nrrd_ptr->encoding <= 0)
      {
        print_error("Unknown NRRD encoding: '%s'\n", str_ptr);
        return VIO_ERROR;
      }
    }
    else if (!strcasecmp("endian", line))
    {
      if (!strcasecmp(str_ptr, "little"))
      {
        nrrd_ptr->endian = NRRD_ENDIAN_LITTLE;
      }
      else if (!strcasecmp(str_ptr, "big"))
      {
        nrrd_ptr->endian = NRRD_ENDIAN_BIG;
      }
      else
      {
        print_error("Unknown NRRD endian: '%s'\n", str_ptr);
        return VIO_ERROR;
      }
    }
    else if (!strcasecmp("kinds", line))
    {
      char *tmp_ptr;
      i = 0;

      if ((tmp_ptr = strtok(str_ptr, " ")) != NULL)
      {
        do
        {
          nrrd_ptr->kinds[i] = nrrd_str_to_kind(tmp_ptr);
          if (nrrd_ptr->kinds[i] <= 0)
          {
            print_error("Unknown NRRD kind: '%s'\n", tmp_ptr);
            return VIO_ERROR;
          }
          if (i >= nrrd_ptr->dimension)
          {
            print_error("Too many kinds entries!\n");
            return VIO_ERROR;
          }
          i++;
        } while ((tmp_ptr = strtok(NULL, " ")) != NULL);
      }
      if (i < nrrd_ptr->dimension)
      {
        print_error("Too few kinds entries!\n");
        return VIO_ERROR;
      }
    }
    else if (!strcasecmp("centers", line) || !strcasecmp("centerings", line))
    {
      char *val_ptr;
      i = 0;

      space_found++;

      if ((val_ptr = strtok(str_ptr, " ")) != NULL)
      {
        do
        {
          if (i >= nrrd_ptr->dimension)
          {
            print_error("Too many center entries!\n");
            return VIO_ERROR;
          }
          if (!strcasecmp("cell", val_ptr) ||
              !strcasecmp("\"cell\"", val_ptr))
          {
            nrrd_ptr->centers[i] = NRRD_CENTER_CELL;
          }
          else if (!strcasecmp("node", val_ptr) ||
                   !strcasecmp("\"node\"", val_ptr))
          {
            nrrd_ptr->centers[i] = NRRD_CENTER_NODE;
          }
          else if (!strcasecmp("???", val_ptr) ||
                   !strcasecmp("\"???\"", val_ptr) ||
                   !strcasecmp("none", val_ptr) ||
                   !strcasecmp("\"none\"", val_ptr))
          {
            nrrd_ptr->centers[i] = NRRD_CENTER_NONE;
          }
          else
          {
            print_error("WARNING: Unknown space centers '%s'\n", val_ptr);
            nrrd_ptr->centers[i] = NRRD_CENTER_NONE;
          }
          i++;
        } while ((val_ptr = strtok(NULL, " ")) != NULL);
      }
      if (i < NRRD_MAX_SPACE)
      {
        print_error("Too few center entries!\n");
        return VIO_ERROR;
      }
    }
    else if (!strcasecmp("labels", line))
    {
      char *tmp_ptr = str_ptr;
      char *beg_ptr;

      i = 0;

      for (;;)
      {
        while (isspace(*tmp_ptr))
        {
          tmp_ptr++;
        }

        if (*tmp_ptr == '"')
        {
          if (i >= nrrd_ptr->dimension)
          {
            print_error("Too many labels entries!\n");
            return VIO_ERROR;
          }
          beg_ptr = ++tmp_ptr;
          while (*tmp_ptr != '"' && *tmp_ptr != '\0')
          {
            tmp_ptr++;
          }
          if (*tmp_ptr == '"')
          {
            *tmp_ptr++ = 0;
            nrrd_ptr->labels[i] = strdup(beg_ptr);
            i++;
          }
        }
        else
        {
          break;
        }
      }
      if (i < nrrd_ptr->dimension)
      {
        print_error("Too few labels entries!\n");
        return VIO_ERROR;
      }
    }
    else if (!strcasecmp("sizes", line))
    {
      char *num_ptr;
      i = 0;

      if ((num_ptr = strtok(str_ptr, " ")) != NULL)
      {
        do
        {
          if (i >= nrrd_ptr->dimension)
          {
            print_error("Too many size entries!\n");
            return VIO_ERROR;
          }
          nrrd_ptr->sizes[i] = strtol(num_ptr, NULL, 10);
          i++;
        } while ((num_ptr = strtok(NULL, " ")) != NULL);
      }
      if (i < nrrd_ptr->dimension)
      {
        print_error("Too few size entries!\n");
        return VIO_ERROR;
      }
    }
    else if (!strcasecmp("space", line))
    {
      if (!strcasecmp("RAS", str_ptr) ||
          !strcasecmp("right-anterior-superior", str_ptr))
      {
        /* Normal MINC case. */
        nrrd_ptr->space = NRRD_SPACE_RAS;
      }
      else if (!strcasecmp("LAS", str_ptr) ||
               !strcasecmp("left-anterior-superior", str_ptr))
      {
        /* inverted X w.r.t. MINC */
        nrrd_ptr->space = NRRD_SPACE_LAS;
      }
      else if (!strcasecmp("LPS", str_ptr) ||
               !strcasecmp("left-posterior-superior", str_ptr))
      {
        /* inverted X, Y w.r.t. MINC */
        nrrd_ptr->space = NRRD_SPACE_LPS;
      }
      else
      {
        print_error("Unknown NRRD space '%s'!\n", str_ptr);
        return VIO_ERROR;
      }
    }
    else if (!strcasecmp("space directions", line))
    {
      char *vec_ptr;
      i = 0;

      space_found++;

      if ((vec_ptr = strtok(str_ptr, " ")) != NULL)
      {
        do
        {
          if (i >= nrrd_ptr->dimension)
          {
            print_error("Too many space direction entries!\n");
            return VIO_ERROR;
          }
          if (!strcasecmp("none", vec_ptr))
          {
            /* no space direction for this axis */
          }
          else
          {
            sscanf(vec_ptr, "(%f, %f, %f)",
                   &nrrd_ptr->space_directions[i][0],
                   &nrrd_ptr->space_directions[i][1],
                   &nrrd_ptr->space_directions[i][2]);
          }
          i++;
        } while ((vec_ptr = strtok(NULL, " ")) != NULL);
      }
      if (i < nrrd_ptr->dimension)
      {
        print_error("Too few space direction entries!\n");
        return VIO_ERROR;
      }
    }
    else if (!strcasecmp("space units", line))
    {
      char *val_ptr;
      i = 0;

      space_found++;

      if ((val_ptr = strtok(str_ptr, " ")) != NULL)
      {
        do
        {
          if (i >= NRRD_MAX_SPACE)
          {
            print_error("Too many space space unit entries!\n");
            return VIO_ERROR;
          }
          if (!strcasecmp("mm", val_ptr) || !strcasecmp("\"mm\"", val_ptr))
          {
            nrrd_ptr->units[i] = NRRD_UNITS_MM;
          }
          else
          {
            print_error("WARNING: Unknown space units '%s'\n", val_ptr);
            nrrd_ptr->units[i] = NRRD_UNITS_NONE;
          }
          i++;
        } while ((val_ptr = strtok(NULL, " ")) != NULL);
      }
      if (i < NRRD_MAX_SPACE)
      {
        print_error("Too few space unit entries!\n");
        return VIO_ERROR;
      }
    }
    else if (!strcasecmp("space origin", line))
    {
      space_found++;

      sscanf(str_ptr, "(%f, %f, %f)",
             &nrrd_ptr->space_origin[0],
             &nrrd_ptr->space_origin[1],
             &nrrd_ptr->space_origin[2]);
    }
    else if (!strcasecmp("spacings", line))
    {
      char *num_ptr;
      i = 0;

      if ((num_ptr = strtok(str_ptr, " ")) != NULL)
      {
        do
        {
          if (i >= nrrd_ptr->dimension)
          {
            print_error("Too many size entries!\n");
            return VIO_ERROR;
          }
          nrrd_ptr->spacings[i] = strtof(num_ptr, NULL);
          i++;
        } while ((num_ptr = strtok(NULL, " ")) != NULL);
      }
      if (i < nrrd_ptr->dimension)
      {
        print_error("Too few size entries!\n");
        return VIO_ERROR;
      }
    }
    else
    {
      print_error("WARNING: Unknown NRRD field: '%s'\n", line);
    }
  }

  if (!space_found)
  {
    /* No space found, so we have to improvise...
     */
    int axis = 0;
    for (i = nrrd_ptr->dimension - 3; i < nrrd_ptr->dimension; i++)
    {
      nrrd_ptr->space_directions[i][axis] = 1;
      axis++;
    }
  }

  /* Set this field if it hasn't been configured already.
   */
  if (nrrd_ptr->endian == 0)
  {
    nrrd_ptr->endian = nrrd_get_system_endian();
  }
  return VIO_OK;
}

/**
 * Converts the fields in a NRRD header to the appropriate MINC attributes.
 * The code is substantially complicated by some of the plumbing and 
 * differences between MINC and NRRD. NRRD files can have arbitrary mappings
 * between the voxel axes and the world coordinate frame, and can appear in
 * any order. The ordering of the spatial axes is determined by examining
 * their components with respect to the spatial frame of reference.
 * 
 * \param nrrd_ptr The internal representation of the NRRD header.
 * \param spatial_axes The array that will hold the correspondence between
 * the file axes an their spatial interpretation.
 * \param mnc_index_from_file The array that will hold the correspondence
 * between the file axes and their internal ordering in the volume.
 * \param mnc_starts The MINC start values, in "file" order.
 * \param mnc_steps The MINC step values, in "file" order.
 * \param mnc_dircos The MINC direction cosine values, in "file" order.
 */
static void
nrrd_to_minc_attributes(nrrd_header_t nrrd_ptr,
                        int spatial_axes[],
                        int mnc_index_from_file[],
                        VIO_Real mnc_starts[],
                        VIO_Real mnc_steps[],
                        VIO_Real mnc_dircos[][VIO_N_DIMENSIONS])
{
  int i, j;
  int non_zero_count = 0;
  VIO_Transform mnc_xform;
  VIO_General_transform mnc_linear_xform;
  int axis = 0;
  int first_non_spatial = 3;

  make_identity_transform(&mnc_xform);

  /* Initialize with default indices. */
  for (i = 0; i < VIO_MAX_DIMENSIONS; i++)
  {
    mnc_index_from_file[i] = -1;
    mnc_starts[i] = 0;
    mnc_steps[i] = 1;
    spatial_axes[i] = -1;
  }

  for (i = 0; i < nrrd_ptr->dimension; i++)
  {
    float c_x = fabsf(nrrd_ptr->space_directions[i][VIO_X]);
    float c_y = fabsf(nrrd_ptr->space_directions[i][VIO_Y]);
    float c_z = fabsf(nrrd_ptr->space_directions[i][VIO_Z]);
    if (c_x > 0 || c_y > 0 || c_z > 0 ||
        nrrd_is_spatial_kind(nrrd_ptr->kinds[i]))
    {
      non_zero_count++;
      if (c_x > c_y && c_x > c_z)
      {
        spatial_axes[axis++] = VIO_X;
        mnc_index_from_file[i] = VIO_X;
      }
      if (c_y > c_x && c_y > c_z)
      {
        spatial_axes[axis++] = VIO_Y;
        mnc_index_from_file[i] = VIO_Y;
      }
      if (c_z > c_y && c_z > c_x)
      {
        spatial_axes[axis++] = VIO_Z;
        mnc_index_from_file[i] = VIO_Z;
      }
    }
    else
    {
      mnc_index_from_file[i] = first_non_spatial++;
    }
  }
  /* If only two spatial axes, invent the third one here.
   * TODO: This may be completely bogus, it's poorly tested.
   */
  if (non_zero_count == 2)
  {
    spatial_axes[2] = 3 - (spatial_axes[0] + spatial_axes[1]);
    nrrd_ptr->space_directions[2][spatial_axes[2]] = 1;
    mnc_index_from_file[2] = spatial_axes[2];
    non_zero_count++;
  }
  if (non_zero_count >= 3)
  {
    for (i = 0; i < nrrd_ptr->dimension; i++)
    {
      axis = mnc_index_from_file[i];
      if (axis >= 0 && axis < 3)      /* if spatial */
      {
        for (j = 0; j < 3; j++)
        {
          Transform_elem(mnc_xform, axis, j) = nrrd_ptr->space_directions[i][j];
        }
        Transform_elem(mnc_xform, axis, 3) = nrrd_ptr->space_origin[axis];
      }
    }
  }

  /* MINC is RAS, if the NRRD space is LAS or LPS, we need to
   * multiply the appropriate transform rows by -1.0.
   */
  if (nrrd_ptr->space == NRRD_SPACE_LAS)
  {
    for (j = 0; j < 4; j++)
    {
      Transform_elem(mnc_xform, 0, j) *= -1.0;
    }
  }
  else if (nrrd_ptr->space == NRRD_SPACE_LPS)
  {
    for (j = 0; j < 4; j++)
    {
      Transform_elem(mnc_xform, 0, j) *= -1.0;
      Transform_elem(mnc_xform, 1, j) *= -1.0;
    }
  }

  create_linear_transform(&mnc_linear_xform, &mnc_xform);

  convert_transform_to_starts_and_steps(&mnc_linear_xform,
                                        VIO_N_DIMENSIONS,
                                        NULL,
                                        spatial_axes,
                                        mnc_starts,
                                        mnc_steps,
                                        mnc_dircos);


  delete_general_transform(&mnc_linear_xform);
}

VIOAPI VIO_Status
initialize_nrrd_format_input(VIO_STR             filename,
                             VIO_Volume          volume,
                             volume_input_struct *in_ptr)
{
  int               sizes[VIO_MAX_DIMENSIONS];
  VIO_Real          steps[VIO_MAX_DIMENSIONS];
  VIO_Real          starts[VIO_MAX_DIMENSIONS];
  long              n_voxels_in_slice;
  nc_type           desired_nc_type;
  int               axis;
  nrrd_header_t     nrrd_ptr;
  VIO_Real          mnc_dircos[VIO_N_DIMENSIONS][VIO_N_DIMENSIONS];
  VIO_Real          mnc_steps[VIO_MAX_DIMENSIONS];
  VIO_Real          mnc_starts[VIO_MAX_DIMENSIONS];
  int               n_dimensions;
  nc_type           file_nc_type;
  VIO_BOOL          signed_flag;
  VIO_Real          min_voxel, max_voxel;
  VIO_Real          min_real, max_real;
  FILE              *fp;
  int               spatial_axes[VIO_MAX_DIMENSIONS];

  if ((fp = fopen(filename, "rb")) == NULL)
  {
    return VIO_ERROR;
  }

  if ((nrrd_ptr = calloc(sizeof(struct nrrd_header), 1)) == NULL)
  {
    fclose(fp);
    return VIO_ERROR;
  }

  /* Read in the NRRD file header and get a znzFile handle to the data.
   */
  if (nrrd_read_header(fp, nrrd_ptr) != VIO_OK)
  {
    print_error("ERROR reading header.\n");
    return VIO_ERROR;
  }
  if (nrrd_ptr->data_file[0] != 0)
  {
    char pathname[NRRD_MAX_PATH];
    /* Load the external data file if present.
     * If the data file field value does NOT start with a '/', we
     * prepend the path of the header file (from 'filename').
     */
    if (nrrd_ptr->data_file[0] != '/')
    {
      char *str_ptr = strrchr(filename, '/');
      if (str_ptr != NULL)
      {
        int n_chars = str_ptr - filename + 1;
        memcpy(pathname, filename, n_chars);
        pathname[n_chars] = 0;
        strcat(pathname, nrrd_ptr->data_file);
      }
      else
      {
        strcpy(pathname, nrrd_ptr->data_file);
      }
    }
    fclose(fp);
    fp = fopen(pathname, "rb");
    if (fp == NULL)
    {
      print_error("Failed to open data file: '%s'.\n", nrrd_ptr->data_file);
      free(nrrd_ptr);
      return VIO_ERROR;
    }
  }

  /* Translate from NRRD to VIO types.
   */
  switch (nrrd_ptr->type)
  {
  case NRRD_TYPE_UINT8:
    in_ptr->file_data_type = VIO_UNSIGNED_BYTE;
    file_nc_type = NC_BYTE;
    signed_flag = FALSE;
    break;
  case NRRD_TYPE_INT8:
    in_ptr->file_data_type = VIO_SIGNED_BYTE;
    file_nc_type = NC_BYTE;
    signed_flag = TRUE;
    break;
  case NRRD_TYPE_UINT16:
    in_ptr->file_data_type = VIO_UNSIGNED_SHORT;
    file_nc_type = NC_SHORT;
    signed_flag = FALSE;
    break;
  case NRRD_TYPE_INT16:
    in_ptr->file_data_type = VIO_SIGNED_SHORT;
    file_nc_type = NC_SHORT;
    signed_flag = TRUE;
    break;
  case NRRD_TYPE_UINT32:
    in_ptr->file_data_type = VIO_UNSIGNED_INT;
    file_nc_type = NC_INT;
    signed_flag = FALSE;
    break;
  case NRRD_TYPE_INT32:
    in_ptr->file_data_type = VIO_SIGNED_INT;
    file_nc_type = NC_INT;
    signed_flag = TRUE;
    break;
  case NRRD_TYPE_FLOAT32:
    in_ptr->file_data_type = VIO_FLOAT;
    file_nc_type = NC_FLOAT;
    signed_flag = TRUE;
    break;
  case NRRD_TYPE_FLOAT64:
    in_ptr->file_data_type = VIO_DOUBLE;
    file_nc_type = NC_DOUBLE;
    signed_flag = TRUE;
    break;
  default:
    print_error("Unsupported NRRD data type.\n");
    free(nrrd_ptr);
    fclose(fp);
    return VIO_ERROR;
  }

  n_dimensions = nrrd_ptr->dimension;

  /* Treat 1D or 2D files as 3D */
  while (n_dimensions < VIO_N_DIMENSIONS)
  {
    nrrd_ptr->sizes[n_dimensions++] = 1;
  }

  for (axis = 0; axis < n_dimensions; axis++)
  {
    in_ptr->sizes_in_file[axis] = nrrd_ptr->sizes[axis];
  }

  /* Decide how to store data in memory. */

  if ( get_volume_data_type(volume) == VIO_NO_DATA_TYPE )
  {
    desired_nc_type = file_nc_type;
  }
  else
  {
    desired_nc_type = get_volume_nc_data_type(volume, &signed_flag);
  }

  if( volume->spatial_axes[VIO_X] < 0 ||
      volume->spatial_axes[VIO_Y] < 0 ||
      volume->spatial_axes[VIO_Z] < 0 )
  {
    print_error("WARNING: setting NRRD spatial axes to XYZ.\n");
    volume->spatial_axes[VIO_X] = 0;
    volume->spatial_axes[VIO_Y] = 1;
    volume->spatial_axes[VIO_Z] = 2;
  }

  if (!set_volume_n_dimensions(volume, n_dimensions))
  {
    print_error("Problem setting number of dimensions.\n");
    free(nrrd_ptr);
    fclose(fp);
    return VIO_ERROR;
  }

#if DEBUG
  nrrd_print_header(stdout, nrrd_ptr);
#endif

  nrrd_to_minc_attributes(nrrd_ptr, spatial_axes,
                          in_ptr->axis_index_from_file,
                          mnc_starts, mnc_steps, mnc_dircos);

  /* Put the various arrays in the correct order. */

  for_less( axis, 0, n_dimensions )
  {
    int volume_axis = in_ptr->axis_index_from_file[axis];

    sizes[volume_axis] = in_ptr->sizes_in_file[axis];

    if (volume_axis >= 0 && volume_axis < 3)
    {
      int spatial_axis = spatial_axes[volume_axis];
      steps[volume_axis] = mnc_steps[spatial_axis];
      starts[volume_axis] = mnc_starts[spatial_axis];
      set_volume_direction_cosine(volume, volume_axis,
                                  mnc_dircos[spatial_axis]);
    }
    else
    {
      /* We just use defaults for non-spatial axes for now.
       */
      steps[volume_axis] = 1.0;
      starts[volume_axis] = 0.0;
    }
  }

#if DEBUG
  printf("         | Size | Step    | Start     | Direction Cosines\n");
  for_less( axis, 0, n_dimensions )
  {
    int volume_axis = in_ptr->axis_index_from_file[axis];
    printf("%2d %2d %2d | %4d | %7.3f | %9.4f |",
           axis,
           volume_axis,
           spatial_axes[axis],
           sizes[volume_axis],
           mnc_steps[volume_axis],
           mnc_starts[volume_axis]);
    if (volume_axis < 3)
    {
      printf(" (%7.4f %7.4f %7.4f)",
             mnc_dircos[volume_axis][0],
             mnc_dircos[volume_axis][1],
             mnc_dircos[volume_axis][2]);
    }
    printf("\n");
  }
#endif /* DEBUG */

  set_volume_separations( volume, steps );
  set_volume_starts( volume, starts );

  set_volume_type( volume, desired_nc_type, signed_flag, 0.0, 0.0 );
  set_volume_sizes( volume, sizes );

  n_voxels_in_slice = 1;
  for_less (axis, 0, n_dimensions - 1)
  {
    n_voxels_in_slice *= in_ptr->sizes_in_file[axis];
  }

  nrrd_find_data_range(nrrd_ptr, fp, &min_voxel, &max_voxel);

  min_real = min_voxel;
  max_real = max_voxel;

  /* As a special case, if we are converting the file to byte,
   * we need to prepare to scale the voxels into the final range.
   */
  if (get_volume_data_type(volume) == VIO_UNSIGNED_BYTE &&
      in_ptr->file_data_type != VIO_UNSIGNED_BYTE)
  {
    /* Set up the scaling for when we actually read the data.
     */
    in_ptr->min_value = min_voxel;
    in_ptr->max_value = max_voxel;

    min_voxel = 0;
    max_voxel = UCHAR_MAX;
  }
  else
  {
    in_ptr->min_value = 0.0;
    in_ptr->max_value = 1.0;
  }

  set_volume_voxel_range(volume, min_voxel, max_voxel);
  set_volume_real_range(volume, min_real, max_real);

  in_ptr->slice_index = 0;
  in_ptr->volume_file = (FILE *) fp;
  in_ptr->header_info = nrrd_ptr;
  in_ptr->generic_slice_buffer = malloc(n_voxels_in_slice *
                                        nrrd_type_to_size(nrrd_ptr->type));
  if (in_ptr->generic_slice_buffer == NULL)
  {
    print_error("ERROR allocating slice buffer.\n");
    return VIO_ERROR;
  }
  return VIO_OK;
}

VIOAPI void
delete_nrrd_format_input(volume_input_struct   *in_ptr)
{
  nrrd_header_t nrrd_ptr = (nrrd_header_t) in_ptr->header_info;
  FILE *fp = (FILE *) in_ptr->volume_file;
  int i;

  if (nrrd_ptr->encoding == NRRD_ENCODING_GZIP)
  {
    gzclose(nrrd_ptr->gzfp);
    nrrd_ptr->gzfp = NULL;
  }
  fclose(fp);
  for (i = 0; i < nrrd_ptr->dimension; i++)
  {
    if (nrrd_ptr->labels[i] != NULL)
    {
      free(nrrd_ptr->labels[i]);
    }
  }
  free(nrrd_ptr);
  free(in_ptr->generic_slice_buffer);
}

VIOAPI VIO_BOOL
input_more_nrrd_format_file(VIO_Volume          volume,
                            volume_input_struct *in_ptr,
                            VIO_Real            *fraction_done)
{
  nrrd_header_t  nrrd_ptr = (nrrd_header_t) in_ptr->header_info;
  FILE           *fp = (FILE *) in_ptr->volume_file;
  void           *data_ptr = in_ptr->generic_slice_buffer;
  int            data_ind = 0;
  double         value = 0;
  double         value_offset, value_scale;
  int            indices[VIO_MAX_DIMENSIONS];
  int            i, j;
  int            total_slices;
  int            n_dimensions = get_volume_n_dimensions( volume );
  int            vio_data_type = get_volume_data_type( volume );
  int            *inner_index;
  int            j_max;

  for_less(i, 0, VIO_MAX_DIMENSIONS)
    indices[i] = 0;

  total_slices = in_ptr->sizes_in_file[n_dimensions - 1];

  if ( in_ptr->slice_index < total_slices )
  {
    int     n_bytes_per_slice;
    int     n_bytes_read;

    n_bytes_per_slice = nrrd_type_to_size(nrrd_ptr->type);
    for_less (i, 0, n_dimensions - 1)
    {
      n_bytes_per_slice *= in_ptr->sizes_in_file[i];
    }

    /* If the memory for the volume has not been allocated yet,
     * initialize that memory now.
     */
    if (!volume_is_alloced(volume))
    {
      alloc_volume_data(volume);
      if (!volume_is_alloced(volume))
      {
        print_error("Failed to allocate volume.\n");
        return FALSE;
      }
    }

    n_bytes_read = nrrd_read_buffer(nrrd_ptr, data_ptr, n_bytes_per_slice, fp);
    if (n_bytes_read < n_bytes_per_slice)
    {
      print_error("Failed to read buffer (%d/%d).\n", n_bytes_read, n_bytes_per_slice);
      return FALSE;
    }

    if (vio_data_type == VIO_UNSIGNED_BYTE &&
        in_ptr->file_data_type != VIO_UNSIGNED_BYTE)
    {
      value_offset = in_ptr->min_value;
      value_scale = (in_ptr->max_value - in_ptr->min_value) /
        (VIO_Real) (NUM_BYTE_VALUES - 1);
    }
    else
    {
      /* Just do trivial scaling. */
      value_offset = 0.0;
      value_scale = 1.0;
    }

    /* Set up the indices.
     */
    inner_index = &indices[in_ptr->axis_index_from_file[0]];

    indices[in_ptr->axis_index_from_file[n_dimensions - 1]] = in_ptr->slice_index;

    j_max = (n_dimensions > 3) ? in_ptr->sizes_in_file[2] : 1;
    for_less( j, 0, j_max )
    {
      if (n_dimensions > 3)
        indices[in_ptr->axis_index_from_file[2]] = j;
      for_less( i, 0, in_ptr->sizes_in_file[1] )
      {
        indices[in_ptr->axis_index_from_file[1]] = i;
        for_less( *inner_index, 0, in_ptr->sizes_in_file[0])
        {
          switch ( nrrd_ptr->type )
          {
          case NRRD_TYPE_UINT8:
            value = ((uint8_t *) data_ptr)[data_ind++];
            break;
          case NRRD_TYPE_INT8:
            value = ((int8_t *) data_ptr)[data_ind++];
            break;
          case NRRD_TYPE_UINT16:
            value = ((uint16_t *) data_ptr)[data_ind++];
            break;
          case NRRD_TYPE_INT16:
            value = ((int16_t *) data_ptr)[data_ind++];
            break;
          case NRRD_TYPE_UINT32:
            value = ((uint32_t *) data_ptr)[data_ind++];
            break;
          case NRRD_TYPE_INT32:
            value = ((int32_t *) data_ptr)[data_ind++];
            break;
          case NRRD_TYPE_UINT64:
            value = ((uint64_t *) data_ptr)[data_ind++];
            break;
          case NRRD_TYPE_INT64:
            value = ((int64_t *) data_ptr)[data_ind++];
            break;
          case NRRD_TYPE_FLOAT32:
            value = ((float *) data_ptr)[data_ind++];
            break;
          case NRRD_TYPE_FLOAT64:
            value = ((double *) data_ptr)[data_ind++];
            break;
          default:
            handle_internal_error( "input_more_nrrd_format_file" );
            break;
          }
          value = (value - value_offset) / value_scale;
          set_volume_voxel_value( volume,
                                  indices[VIO_X],
                                  indices[VIO_Y],
                                  indices[VIO_Z],
                                  indices[3],
                                  indices[4],
                                  value);
        }
      }
    }
    in_ptr->slice_index++;      /* Advance to the next slice. */
  }

  *fraction_done = (VIO_Real) in_ptr->slice_index / total_slices;

  return (in_ptr->slice_index < total_slices);
}
