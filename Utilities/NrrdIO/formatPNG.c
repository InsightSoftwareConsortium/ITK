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

#define MAGIC "\211PNG"

int
_nrrdFormatPNG_available(void) {
  
#if TEEM_PNG
  return AIR_TRUE;
#else
  return AIR_FALSE;
#endif
}

int
_nrrdFormatPNG_nameLooksLike(const char *filename) {
  
  return airEndsWith(filename, NRRD_EXT_PNG);
}

int
_nrrdFormatPNG_fitsInto(const Nrrd *nrrd, const NrrdEncoding *encoding,
                        int useBiff) {
  static const char me[]="_nrrdFormatPNG_fitsInto";

#if !TEEM_PNG  /* ------------------------------------------- */

  AIR_UNUSED(nrrd);
  AIR_UNUSED(encoding);
  biffMaybeAddf(useBiff, NRRD,
                "%s: %s format not available in this Teem build",
                me, nrrdFormatPNG->name); 
  return AIR_FALSE;

#else  /* ------------------------------------------- */

  int ret;

  if (!( nrrd && encoding )) {
    biffMaybeAddf(useBiff, NRRD, "%s: got NULL nrrd (%p) or encoding (%p)",
                  me, AIR_CAST(void*, nrrd), AIR_CAST(void*, encoding));
    return AIR_FALSE;
  }
  if (!( nrrdTypeUChar == nrrd->type || nrrdTypeUShort == nrrd->type )) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: type must be %s or %s (not %s)", me,
                  airEnumStr(nrrdType, nrrdTypeUChar),
                  airEnumStr(nrrdType, nrrdTypeUShort),
                  airEnumStr(nrrdType, nrrd->type));
    return AIR_FALSE;
  }
  /* else */
  /* encoding ignored- always gzip */
  if (2 == nrrd->dim) {
    /* its a gray-scale image */
    ret = AIR_TRUE;
  } else if (3 == nrrd->dim) {
    if (!( 1 == nrrd->axis[0].size
           || 2 == nrrd->axis[0].size
           || 3 == nrrd->axis[0].size
           || 4 == nrrd->axis[0].size )) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: 1st axis size is " _AIR_SIZE_T_CNV 
                    ", not 1, 2, 3, or 4", me, nrrd->axis[0].size); 
      return AIR_FALSE;
    }
    /* else */
    ret = AIR_TRUE;
  } else {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: dimension is %d, not 2 or 3", me, nrrd->dim); 
    return AIR_FALSE;
  }
  return ret;

#endif   /* ------------------------------------------- */
}

int
_nrrdFormatPNG_contentStartsLike(NrrdIoState *nio) {

  return !strcmp(MAGIC, nio->line);
}

#if TEEM_PNG
void
_nrrdErrorHandlerPNG (png_structp png, png_const_charp message)
{
  static const char me[]="_nrrdErrorHandlerPNG";
  /* add PNG error message to biff */
  biffAddf(NRRD, "%s: PNG error: %s", me, message);
  /* longjmp back to the setjmp, return 1 */
  longjmp(png->jmpbuf, 1);
}

void
_nrrdWarningHandlerPNG (png_structp png, png_const_charp message)
{
  static const char me[]="_nrrdWarningHandlerPNG";
  AIR_UNUSED(png);
  /* add the png warning message to biff */
  biffAddf(NRRD, "%s: PNG warning: %s", me, message);
  /* no longjump, execution continues */
}

/* we need to use the file I/O callbacks on windows
   to make sure we can mix VC6 libpng with VC7 Teem */
#ifdef _WIN32
static void
_nrrdReadDataPNG (png_structp png, png_bytep data, png_size_t len)
{
  png_size_t read;
  read = (png_size_t)fread(data, (png_size_t)1, len, (FILE*)png->io_ptr);
  if (read != len) png_error(png, "file read error");
}
static void
_nrrdWriteDataPNG (png_structp png, png_bytep data, png_size_t len)
{
  png_size_t written;
  written = fwrite(data, 1, len, (FILE*)(png->io_ptr));
  if (written != len) png_error(png, "file write error");
}

static void
_nrrdFlushDataPNG (png_structp png)
{
   FILE *io_ptr = (FILE*)(png->io_ptr);
   if (io_ptr != NULL) fflush(io_ptr);
}
#endif /* _WIN32 */
#endif /* TEEM_PNG */

int
_nrrdFormatPNG_read(FILE *file, Nrrd *nrrd, NrrdIoState *nio) {
  static const char me[]="_nrrdFormatPNG_read";
#if TEEM_PNG
  png_structp png;
  png_infop info;
  png_bytep *row;
  png_uint_32 width, height, rowsize, hi;
  png_text* txt;
  int depth, type, i, channels, numtxt, ret;
  int ntype, ndim;
  size_t nsize[3];
#endif /* TEEM_PNG */

  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  if (!_nrrdFormatPNG_contentStartsLike(nio)) {
    biffAddf(NRRD, "%s: this doesn't look like a %s file", me, 
             nrrdFormatPNG->name);
    return 1;
  }

#if TEEM_PNG
  /* create png struct with the error handlers above */
  png = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL,
                               _nrrdErrorHandlerPNG,
                               _nrrdWarningHandlerPNG);
  if (png == NULL) {
    biffAddf(NRRD, "%s: failed to create PNG read struct", me);
    return 1;
  }
  /* create image info struct */
  info = png_create_info_struct(png);
  if (info == NULL) {
    png_destroy_read_struct(&png, NULL, NULL);
    biffAddf(NRRD, "%s: failed to create PNG image info struct", me);
    return 1;
  }
  /* set up png style error handling */
  if (setjmp(png->jmpbuf)) {
    /* the error is reported inside the handler, 
       but we still need to clean up and return */
    png_destroy_read_struct(&png, &info, NULL);
    return 1;
  }
  /* initialize png I/O */
#ifdef _WIN32
  png_set_read_fn(png, (png_voidp)file, _nrrdReadDataPNG);
#else
  png_init_io(png, file);
#endif
  /* if we are here, we have already read 6 bytes from the file */
  png_set_sig_bytes(png, 6);
  /* png_read_info() returns all information from the file 
     before the first data chunk */
  png_read_info(png, info);
  png_get_IHDR(png, info, &width, &height, &depth, &type, 
               NULL, NULL, NULL);
  /* expand paletted colors into rgb triplets */
  if (type == PNG_COLOR_TYPE_PALETTE)
    png_set_palette_to_rgb(png);
  /* expand grayscale images to 8 bits from 1, 2, or 4 bits */
  if (type == PNG_COLOR_TYPE_GRAY && depth < 8)
    png_set_gray_1_2_4_to_8(png);
  /* expand paletted or rgb images with transparency to full alpha
     channels so the data will be available as rgba quartets */
  if (png_get_valid(png, info, PNG_INFO_tRNS))
    png_set_tRNS_to_alpha(png);
  /* fix endianness for 16 bit formats */
  if (depth > 8 && airMyEndian == airEndianLittle)
    png_set_swap(png);
#if 0
  /* HEY GLK asks why is this commented out? */
  /* set up gamma correction */
  /* NOTE: screen_gamma is platform dependent,
     it can hardwired or set from a parameter/environment variable */
  if (png_get_sRGB(png_ptr, info_ptr, &intent)) {
    /* if the image has sRGB info, 
       pass in standard nrrd file gamma 1.0 */
    png_set_gamma(png_ptr, screen_gamma, 1.0);
  } else {
    double gamma;
    /* set image gamma if present */
    if (png_get_gAMA(png, info, &gamma))
      png_set_gamma(png, screen_gamma, gamma);
    else
      png_set_gamma(png, screen_gamma, 1.0);
  }
#endif
  /* update reader */
  png_read_update_info(png, info);  
  /* allocate memory for the image data */
  ntype = depth > 8 ? nrrdTypeUShort : nrrdTypeUChar;
  switch (type) {
  case PNG_COLOR_TYPE_GRAY:
    ndim = 2; nsize[0] = width; nsize[1] = height;
    nsize[2] = 1;  /* to simplify code below */
    break;
  case PNG_COLOR_TYPE_GRAY_ALPHA:
    ndim = 3; nsize[0] = 2; nsize[1] = width; nsize[2] = height;
    break;
  case PNG_COLOR_TYPE_RGB:
    ndim = 3; nsize[0] = 3; nsize[1] = width; nsize[2] = height;
    break;
  case PNG_COLOR_TYPE_RGB_ALPHA:
    ndim = 3; nsize[0] = 4; nsize[1] = width; nsize[2] = height;
    break;
  case PNG_COLOR_TYPE_PALETTE:
    /* TODO: merge this with the outer switch, needs to be tested */
    channels = png_get_channels(png, info);
    if (channels < 2) {
      ndim = 2; nsize[0] = width; nsize[1] = height;
    } else {
      ndim = 3; nsize[0] = channels; nsize[1] = width; nsize[2] = height;
    }
    break;
  default:
    png_destroy_read_struct(&png, &info, NULL);
    biffAddf(NRRD, "%s: unknown png type: %d", me, type);
    return 1;
    break;
  }
  if (nio->oldData
      && (nio->oldDataSize
          == (size_t)(nrrdTypeSize[ntype]*nsize[0]*nsize[1]*nsize[2]))) {
    ret = nrrdWrap_nva(nrrd, nio->oldData, ntype, ndim, nsize);
  } else {
    ret = nrrdMaybeAlloc_nva(nrrd, ntype, ndim, nsize);
  }
  if (ret) {
    png_destroy_read_struct(&png, &info, NULL);
    biffAddf(NRRD, "%s: failed to allocate nrrd", me);
    return 1;
  }
  /* query row size */
  rowsize = png_get_rowbytes(png, info);
  /* check byte size */
  if (nrrdElementNumber(nrrd)*nrrdElementSize(nrrd) != height*rowsize) {
    png_destroy_read_struct(&png, &info, NULL);
    biffAddf(NRRD, "%s: size mismatch", me);
    return 1;
  }
  /* set up row pointers */
  row = (png_bytep*)malloc(sizeof(png_bytep)*height);
  for (hi=0; hi<height; hi++) {
    row[hi] = &((png_bytep)nrrd->data)[hi*rowsize];
  }
  /* read the entire image in one pass */
  png_read_image(png, row);
  /* read all text fields from the text chunk */
  numtxt = png_get_text(png, info, &txt, NULL);
  for (i=0; i<numtxt; i++) {
    if (!strcmp(txt[i].key, NRRD_PNG_FIELD_KEY)) {
      int ret;
      nio->pos = 0;
      /* Reading PNGs teaches Gordon that his scheme for parsing nrrd header
         information is inappropriately specific to reading PNMs and NRRDs,
         since in this case the text from which we parse a nrrd field
         descriptor did NOT come from a line of text as read by
         _nrrdOneLine */
      nio->line = (char *)airFree(nio->line);
      nio->line = airStrdup(txt[i].text);
      ret = _nrrdReadNrrdParseField(nio, AIR_FALSE);
      if (ret) {
        const char* fs = airEnumStr(nrrdField, ret);
        if (nrrdField_comment == ret) {
          ret = 0;
          goto plain;
        }
        if (!_nrrdFieldValidInImage[ret]) {
          if (1 <= nrrdStateVerboseIO) {
            fprintf(stderr, "(%s: field \"%s\" (not allowed in PNG) "
                    "--> plain comment)\n", me, fs);
          }
          ret = 0;
          goto plain;
        }
        if (!nio->seen[ret] 
            && nrrdFieldInfoParse[ret](file, nrrd, nio, AIR_FALSE)) {
          if (1 <= nrrdStateVerboseIO) {
            fprintf(stderr, "(%s: unparsable info for field \"%s\" "
                    "--> plain comment)\n", me, fs);
          }
          ret = 0;
          goto plain;
        }
        nio->seen[ret] = AIR_TRUE;      
      plain:
        if (!ret) {
          if (nrrdCommentAdd(nrrd, nio->line)) {
            png_destroy_read_struct(&png, &info, NULL);
            biffAddf(NRRD, "%s: couldn't add comment", me);
            return 1;
          }
        }
      }
    } else if (!strcmp(txt[i].key, NRRD_PNG_COMMENT_KEY)) {
      char *p, *c;
      c = airStrtok(txt[i].text, "\n", &p);
      while (c) {
        if (nrrdCommentAdd(nrrd, c)) {
          png_destroy_read_struct(&png, &info, NULL);
          biffAddf(NRRD, "%s: couldn't add comment", me);
          return 1;
        }
        c = airStrtok(NULL, "\n", &p);
      }
    } else {
      if (nrrdKeyValueAdd(nrrd, txt[i].key, txt[i].text)) {
        png_destroy_read_struct(&png, &info, NULL);
        biffAddf(NRRD, "%s: couldn't add key/value pair", me);
        return 1;
      }
    }
  }
  /* finish reading */
  png_read_end(png, info);
  /* clean up */
  row = (png_byte**)airFree(row);
  png_destroy_read_struct(&png, &info, NULL);

  return 0;
#else
  biffAddf(NRRD, "%s: Sorry, this nrrd not compiled with PNG enabled", me);
  return 1;
#endif
}

int
_nrrdFormatPNG_write(FILE *file, const Nrrd *nrrd, NrrdIoState *nio) {
  static const char me[]="_nrrdFormatPNG_write";
#if TEEM_PNG
  int fi, depth, type, csize;
  unsigned int jj, numtxt, txtidx;
  png_structp png;
  png_infop info;
  png_bytep *row;
  png_uint_32 width, height, rowsize, hi;
  png_text *txt;
  char *key, *value;

  /* no need to check type and format, done in FitsInFormat */
  /* create png struct with the error handlers above */
  png = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL,
                                _nrrdErrorHandlerPNG,
                                _nrrdWarningHandlerPNG);
  if (png == NULL) {
    biffAddf(NRRD, "%s: failed to create PNG write struct", me);
    return 1;
  }
  /* create image info struct */
  info = png_create_info_struct(png);
  if (info == NULL) {
    png_destroy_write_struct(&png, NULL);
    biffAddf(NRRD, "%s: failed to create PNG image info struct", me);
    return 1;
  }
  /* set up error png style error handling */
  if (setjmp(png->jmpbuf))
  {
    /* the error is reported inside the error handler, 
       but we still need to clean up an return with an error */
    png_destroy_write_struct(&png, &info);
    return 1;
  }
  /* initialize png I/O */
#ifdef _WIN32
  png_set_write_fn(png, file, _nrrdWriteDataPNG, _nrrdFlushDataPNG);
#else
  png_init_io(png, file);        
#endif
  /* calculate depth, width, height, and row size */
  depth = nrrd->type == nrrdTypeUChar ? 8 : 16;
  switch (nrrd->dim) {
    case 2: /* g only */
    width = nrrd->axis[0].size;
    height = nrrd->axis[1].size;
    type = PNG_COLOR_TYPE_GRAY;
    rowsize = width*nrrdElementSize(nrrd);
    break;
    case 3: /* g, ga, rgb, rgba */
    width = nrrd->axis[1].size;
    height = nrrd->axis[2].size;
    rowsize = width*nrrd->axis[0].size*nrrdElementSize(nrrd);
    switch (nrrd->axis[0].size) {
      case 1:
      type = PNG_COLOR_TYPE_GRAY;
      break;
      case 2:
      type = PNG_COLOR_TYPE_GRAY_ALPHA;
      break;
      case 3:
      type = PNG_COLOR_TYPE_RGB;
      break;
      case 4:
      type = PNG_COLOR_TYPE_RGB_ALPHA;
      break;
      default:
      png_destroy_write_struct(&png, &info);
      biffAddf(NRRD, "%s: nrrd->axis[0].size (" _AIR_SIZE_T_CNV 
               ") not compatible with PNG", me, nrrd->axis[0].size);
      return 1;
      break;
    }
    break;
    default:
    png_destroy_write_struct(&png, &info);
    biffAddf(NRRD, "%s: dimension (%d) not compatible with PNG",
             me, nrrd->dim);
    return 1;
    break;
  }
  /* set image header info */
  png_set_IHDR(png, info, width, height, depth, type,
               PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_BASE,
               PNG_FILTER_TYPE_BASE);
  /* calculate numtxt and allocate txt[] array */
  numtxt = 0;
  for (fi=nrrdField_unknown+1; fi<nrrdField_last; fi++) {
    if (_nrrdFieldValidInImage[fi] && _nrrdFieldInteresting(nrrd, nio, fi)) {
      numtxt++;
    }
  }
  for (jj=0; jj<nrrdKeyValueSize(nrrd); jj++) {
    nrrdKeyValueIndex(nrrd, &key, &value, jj);
    /* HEY: why is the NULL check needed?? */
    if (NULL != key && NULL != value) {
      numtxt++;
    }
    free(key);
    free(value);
    key = NULL;
    value = NULL;
  }
  if (nrrd->cmtArr->len > 0) {
    /* all comments are put into single text field */
    numtxt += 1;
  }
  if (0 == numtxt) {
    txt = NULL;
  } else {
    txt = AIR_CAST(png_text *, calloc(numtxt, sizeof(png_text)));
    /* add nrrd fields to the text chunk */
    csize = 0;
    txtidx = 0;
    for (fi=nrrdField_unknown+1; fi<nrrdField_last; fi++) {
      if (_nrrdFieldValidInImage[fi] && _nrrdFieldInteresting(nrrd, nio, fi)) {
        txt[txtidx].key = airStrdup(NRRD_PNG_FIELD_KEY);
        txt[txtidx].compression = PNG_TEXT_COMPRESSION_NONE;
        _nrrdSprintFieldInfo(&(txt[txtidx].text), "", nrrd, nio, fi);
        txtidx++;
      }
    }
    /* add nrrd key/value pairs to the chunk */
    for (jj=0; jj<nrrdKeyValueSize(nrrd); jj++) {
      nrrdKeyValueIndex(nrrd, &key, &value, jj);
      if (NULL != key && NULL != value) {
        txt[txtidx].key = key;
        txt[txtidx].text = value;
        txt[txtidx].compression = PNG_TEXT_COMPRESSION_NONE;
        txtidx++;
      }
    }
    /* add nrrd comments as a single text field */
    if (nrrd->cmtArr->len > 0) {
      txt[txtidx].key = airStrdup(NRRD_PNG_COMMENT_KEY);
      txt[txtidx].compression = PNG_TEXT_COMPRESSION_NONE;
      for (jj=0; jj<nrrd->cmtArr->len; jj++) {
        csize += airStrlen(nrrd->cmt[jj]) + 1;
      }
      txt[txtidx].text = (png_charp)malloc(csize + 1);
      txt[txtidx].text[0] = 0;
      for (jj=0; jj<nrrd->cmtArr->len; jj++) {
        strcat(txt[txtidx].text, nrrd->cmt[jj]);
        strcat(txt[txtidx].text, "\n");
      }
      txtidx++;
    }
    png_set_text(png, info, txt, numtxt);
  }
  /* write header */
  png_write_info(png, info);
  /* fix endianness for 16 bit formats */
  if (depth > 8 && airMyEndian == airEndianLittle) {
    png_set_swap(png);
  }
  /* set up row pointers */
  row = (png_bytep*)malloc(sizeof(png_bytep)*height);
  for (hi=0; hi<height; hi++) {
    row[hi] = &((png_bytep)nrrd->data)[hi*rowsize];
  }
  png_set_rows(png, info, row);
  /* write the entire image in one pass */
  png_write_image(png, row);
  /* finish writing */
  png_write_end(png, info);
  /* clean up */
  if (txt) {
    for (jj=0; jj<numtxt; jj++) {
      txt[jj].key = (char *)airFree(txt[jj].key);
      txt[jj].text = (char *)airFree(txt[jj].text);
    }
    free(txt);
  }
  row = (png_byte**)airFree(row);
  png_destroy_write_struct(&png, &info);

  return 0;
#else
  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  biffAddf(NRRD, "%s: Sorry, this nrrd not compiled with PNG enabled", me);
  return 1;
#endif
}

const NrrdFormat
_nrrdFormatPNG = {
  "PNG",
  AIR_TRUE,   /* isImage */
  AIR_TRUE,   /* readable */
  AIR_FALSE,  /* usesDIO */
  _nrrdFormatPNG_available,
  _nrrdFormatPNG_nameLooksLike,
  _nrrdFormatPNG_fitsInto,
  _nrrdFormatPNG_contentStartsLike,
  _nrrdFormatPNG_read,
  _nrrdFormatPNG_write
};

const NrrdFormat *const
nrrdFormatPNG = &_nrrdFormatPNG;
