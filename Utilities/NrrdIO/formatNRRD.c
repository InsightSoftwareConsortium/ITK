/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2004, 2003, 2002, 2001, 2000, 1999, 1998 University of Utah
 
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

#define MAGIC0 "NRRD00.01"
#define MAGIC1 "NRRD0001"
#define MAGIC2 "NRRD0002"
#define MAGIC3 "NRRD0003"

int
_nrrdFormatNRRD_available(void) {
  
  return AIR_TRUE;
}

int
_nrrdFormatNRRD_nameLooksLike(const char *filename) {
  
  return (airEndsWith(filename, NRRD_EXT_NRRD)
          || airEndsWith(filename, NRRD_EXT_NHDR));
}

int
_nrrdFormatNRRD_fitsInto(const Nrrd *nrrd, const NrrdEncoding *encoding,
                         int useBiff) {
  char me[]="_nrrdFormatNRRD_fitsInto", err[AIR_STRLEN_MED];

  if (!( nrrd && encoding )) {
    sprintf(err, "%s: got NULL nrrd (%p) or encoding (%p)",
            me, nrrd, encoding);
    biffMaybeAdd(NRRD, err, useBiff); 
    return AIR_FALSE;
  }

  /* everything fits in a nrrd */
  return AIR_TRUE;
}

int
_nrrdFormatNRRD_contentStartsLike(NrrdIoState *nio) {
  
  return (!strcmp(MAGIC0, nio->line)
          || !strcmp(MAGIC1, nio->line)
          || !strcmp(MAGIC2, nio->line)
          || !strcmp(MAGIC3, nio->line)
          );
}

/*
** _nrrdHeaderCheck()
**
** consistency checks on relationship between fields of nrrd, (only)
** to be used after the headers is parsed, and before the data is read
**
*/
int
_nrrdHeaderCheck (Nrrd *nrrd, NrrdIoState *nio) {
  char me[]="_nrrdHeaderCheck", err[AIR_STRLEN_MED];
  int i;

  for (i=1; i<=NRRD_FIELD_MAX; i++) {
    if (!_nrrdFieldRequired[i])
      continue;
    if (!nio->seen[i]) {
      sprintf(err, "%s: didn't see required field: %s",
              me, airEnumStr(nrrdField, i));
      biffAdd(NRRD, err); return 1;
    }
  }
  if (nrrdTypeBlock == nrrd->type && 0 == nrrd->blockSize) {
    sprintf(err, "%s: type is %s, but missing field: %s", me,
            airEnumStr(nrrdType, nrrdTypeBlock),
            airEnumStr(nrrdField, nrrdField_block_size));
    biffAdd(NRRD, err); return 1;
  }
  /* this shouldn't actually be necessary ... */
  /* really? it saves all the data readers from doing it ... */
  if (!nrrdElementSize(nrrd)) {
    sprintf(err, "%s: nrrd reports zero element size!", me);
    biffAdd(NRRD, err); return 1;
  }
  /* _nrrdReadNrrdParse_sizes() checks axis[i].size, which completely
     determines the return of nrrdElementNumber() */
  if (airEndianUnknown == nio->endian
      && nio->encoding->endianMatters
      && 1 != nrrdElementSize(nrrd)) {
    sprintf(err, "%s: type (%s) and encoding (%s) require %s info", me,
            airEnumStr(nrrdType, nrrd->type),
            nio->encoding->name,
            airEnumStr(nrrdField, nrrdField_endian));
    biffAdd(NRRD, err); return 1;    
  }

  /* we don't really try to enforce consistency with the
     min/max/center/size information on each axis, other than the
     value checking done by the _nrrdReadNrrdParse_* functions,
     because we only really care that we know each axis size.  Past
     that, if the user messes it up, its not really our problem ... */

  return 0;
}

/*
** Note: currently, this will read key/value pairs from a NRRD0001
** file without any complaints, even though strictly speaking the
** presence of the key/value pairs is in violation of the format
*/
int
_nrrdFormatNRRD_read(FILE *file, Nrrd *nrrd, NrrdIoState *nio) {
  char me[]="_nrrdFormatNRRD_read", *err=NULL;
  /* NOTE: err has to be dynamically allocated because of the 
     arbitrary-sized input lines that it may have to copy */
  int ret, len;

  if (!_nrrdFormatNRRD_contentStartsLike(nio)) {
    sprintf(err, "%s: this doesn't look like a %s file", me,
            nrrdFormatNRRD->name);
    biffAdd(NRRD, err); return 1;
  }

  /* we use the non-NULL-ity nio->dataFile as the indicator (from
     _nrrdReadNrrdParse_data_file) that there was a detached data file,  
     so we'd better verify that it is already NULL */
  if (nio->dataFile) {
    sprintf(err, "%s: nio->dataFile was *not* NULL on entry; something "
            "is probably wrong", me);
    biffAdd(NRRD, err); return 1;
  }

  /* parse header lines */
  while (1) {
    nio->pos = 0;
    if (_nrrdOneLine(&len, nio, file)) {
      if ((err = (char*)malloc(AIR_STRLEN_MED))) {
        sprintf(err, "%s: trouble getting line of header", me);
        biffAdd(NRRD, err); free(err);
      }
      return 1;
    }
    if (len > 1) {
      ret = _nrrdReadNrrdParseField(nrrd, nio, AIR_TRUE);
      if (!ret) {
        if ((err = (char*)malloc(AIR_STRLEN_MED + strlen(nio->line)))) {
          sprintf(err, "%s: trouble parsing field in \"%s\"", me, nio->line);
          biffAdd(NRRD, err); free(err);
        }
        return 1;
      }
      /* comments and key/values are allowed multiple times */
      if (nio->seen[ret]
          && !(ret == nrrdField_comment || ret == nrrdField_keyvalue)) {
        if ((err = (char*)malloc(AIR_STRLEN_MED))) {
          sprintf(err, "%s: already set field %s", me, 
                  airEnumStr(nrrdField, ret));
          biffAdd(NRRD, err); free(err);
        }
        return 1;
      }
      if (_nrrdReadNrrdParseInfo[ret](nrrd, nio, AIR_TRUE)) {
        if ((err = (char*)malloc(AIR_STRLEN_MED))) {
          sprintf(err, "%s: trouble parsing %s info \"%s\"", me,
                  airEnumStr(nrrdField, ret), nio->line+nio->pos);
          biffAdd(NRRD, err); free(err);
        }
        return 1;
      }
      nio->seen[ret] = AIR_TRUE;
    } else {
      /* len <= 1: either we're at EOF, or we just read the empty
         line seperating header from data */
      break;
    }
  }

  if (!len                   /* we're at EOF ... */
      && !nio->dataFile) {   /* ... but _nrrdReadNrrdParse_data_file hasn't
                                opened the seperate data file */
    if ((err = (char*)malloc(AIR_STRLEN_MED))) {
      sprintf(err, "%s: hit end of header, but no \"%s\" given", me,
              airEnumStr(nrrdField, nrrdField_data_file));
      biffAdd(NRRD, err); free(err);
    }
    return 1;
  }
  
  if (_nrrdHeaderCheck(nrrd, nio)) {
    if ((err = (char*)malloc(AIR_STRLEN_MED))) {
      sprintf(err, "%s: %s", me, 
              (len ? "finished reading header, but there were problems"
               : "hit EOF before seeing a complete valid header"));
      biffAdd(NRRD, err); free(err);
    }
    return 1;
  }
  
  /* we seemed to have read in a valid header; now read the data */
  if (!nio->dataFile) {
    nio->detachedHeader = AIR_FALSE;
    nio->dataFile = file;
  } else {
    nio->detachedHeader = AIR_TRUE;
  }
  if (2 <= nrrdStateVerboseIO) {
    fprintf(stderr, "(%s: reading %s data ", me, nio->encoding->name);
    fflush(stderr);
  }
  if (nrrdLineSkip(nio)) {
    if ((err = (char*)malloc(AIR_STRLEN_MED))) {
      sprintf(err, "%s: couldn't skip lines", me);
      biffAdd(NRRD, err); free(err);
    }
    return 1;
  }
  if (!nio->encoding->isCompression) {
    /* bytes are skipped here for non-compression encodings, but are
       skipped within the decompressed stream for compression encodings */
    if (nrrdByteSkip(nrrd, nio)) {
      if ((err = (char*)malloc(AIR_STRLEN_MED))) {
        sprintf(err, "%s: couldn't skip bytes", me);
        biffAdd(NRRD, err); free(err);
      }
      return 1;
    }
  }

  if (nio->skipData) {
    nrrd->data = NULL;
  } else {
    if (nio->encoding->read(nrrd, nio)) {
      if (2 <= nrrdStateVerboseIO) {
        fprintf(stderr, "error!\n");
      }
      if ((err = (char*)malloc(AIR_STRLEN_MED))) {
        sprintf(err, "%s:", me);
        biffAdd(NRRD, err); free(err);
      }
      return 1;
    }
    if (airEndianUnknown != nio->endian) {
      /* we positively know the endianness of data just read */
      if (1 < nrrdElementSize(nrrd)
          && nio->encoding->endianMatters
          && nio->endian != AIR_ENDIAN) {
        /* endianness exposed in encoding, and its wrong */
        if (2 <= nrrdStateVerboseIO) {
          fprintf(stderr, "(%s: fixing endianness ... ", me);
          fflush(stderr);
        }
        nrrdSwapEndian(nrrd);
        if (2 <= nrrdStateVerboseIO) {
          fprintf(stderr, "done)");
          fflush(stderr);
        }
      }
    }
  }

  if (2 <= nrrdStateVerboseIO) {
    fprintf(stderr, "done)\n");
  }
  if (nio->keepNrrdDataFileOpen) {
    /* we don't touch nio->dataFile.  Either:
       1) its a part of an attached file, so nio->dataFile == file
       (argument to this function), and its up to the caller to make
       sure they don't close file
       2) its a detached file which was opened by
       _nrrdReadNrrdParse_data_file, and we leave it open
    */
  } else {
    if (nio->detachedHeader) {
      nio->dataFile = airFclose(nio->dataFile);
      /* fprintf(stderr, "!%s: nio->dataFile = %p\n", me, nio->dataFile); */
    } else {
      /* we set nio->dataFile to "file" above; set it back; but don't
         close file, since we didn't open it */
      nio->dataFile = NULL;
    }
  }
  
  return 0;
}

int
_nrrdFormatNRRD_write(FILE *file, const Nrrd *nrrd, NrrdIoState *nio) {
  char me[]="_nrrdFormatNRRD_write", err[AIR_STRLEN_MED], 
    tmp[AIR_STRLEN_HUGE], trueDataFN[AIR_STRLEN_HUGE];
  int i, givenDFN, createDFN;
  
  if (nio->detachedHeader) {
    /* whether or not nio->skipData, we have to contrive a filename to 
       say in the "data file" field, which is stored in nio->dataFN */
    givenDFN = !!airStrlen(nio->dataFN);
    createDFN = !!airStrlen(nio->path) && !!airStrlen(nio->base);
    if (!givenDFN && !createDFN) {
      sprintf(err, "%s: can't create data file name: nio's dataFN, "
              "path, and base all empty", me);
      biffAdd(NRRD, err); return 1;
    }
    if (givenDFN && createDFN) {
      sprintf(err, "%s: data file name can't be both explicit ("
              "nio->dataFN=\"%s\") and implicit "
              "(nio->path=\"%s\", nio->base=\"%s\")", me, 
              nio->dataFN, nio->path, nio->base);
      biffAdd(NRRD, err); return 1;
    }
    if (createDFN) {
      /* set both nio->dataFN and trueDataFN, even though trueDataFN
         will only be used if !nio->skipData */
      sprintf(tmp, "%s.%s", nio->base, nio->encoding->suffix);
      nio->dataFN = malloc(strlen(_nrrdRelativePathFlag)
                           + strlen(tmp) + 1);
      sprintf(nio->dataFN, "%s%s", _nrrdRelativePathFlag, tmp);
      sprintf(trueDataFN, "%s/%s", nio->path, tmp);
    }
  }
  
  /* we try to use the oldest format that will old the nrrd */
  fprintf(file, "%s\n", 
          (_nrrdFieldInteresting(nrrd, nio, nrrdField_kinds)
           ? MAGIC3
           : (nrrdKeyValueSize(nrrd) 
              ? MAGIC2 
              : MAGIC1)));

  /* this is where the majority of the header printing happens */
  for (i=1; i<=NRRD_FIELD_MAX; i++) {
    if (_nrrdFieldInteresting(nrrd, nio, i)) {
      _nrrdFprintFieldInfo (file, "", nrrd, nio, i);
    }
  }

  /* comments and key/values handled differently */
  for (i=0; i<nrrd->cmtArr->len; i++) {
    fprintf(file, "%c %s\n", NRRD_COMMENT_CHAR, nrrd->cmt[i]);
  }
  for (i=0; i<nrrd->kvpArr->len; i++) {
    _nrrdKeyValueFwrite(file, NULL, nrrd->kvp[0 + 2*i], nrrd->kvp[1 + 2*i]);
  }

  if (!nio->detachedHeader) {
    fprintf(file, "\n");
  }

  if (!nio->skipData) {
    if (nio->detachedHeader) {
      nio->dataFile = fopen(trueDataFN, "wb");
      if (!nio->dataFile) {
        sprintf(err, "%s: couldn't fopen(\"%s\",\"wb\"): %s",
                me, trueDataFN, strerror(errno));
        biffAdd(NRRD, err); return 1;
      }
    } else {
      nio->dataFile = file;
    }
    if (2 <= nrrdStateVerboseIO) {
      fprintf(stderr, "(%s: writing %s data ", me, nio->encoding->name);
      fflush(stderr);
    }
    if (nio->encoding->write(nrrd, nio)) {
      if (2 <= nrrdStateVerboseIO) {
        fprintf(stderr, "error!\n");
      }
      sprintf(err, "%s: couldn't write %s data", me, nio->encoding->name);
      biffAdd(NRRD, err); return 1;
    }
    if (2 <= nrrdStateVerboseIO) {
      fprintf(stderr, "done)\n");
    }
    if (nio->detachedHeader) {
      nio->dataFile = airFclose(nio->dataFile);
    } else {
      nio->dataFile = NULL;
    }
  }

  return 0;
}

const NrrdFormat
_nrrdFormatNRRD = {
  "NRRD",
  AIR_FALSE,  /* isImage */
  AIR_TRUE,   /* readable */
  AIR_TRUE,   /* usesDIO */
  _nrrdFormatNRRD_available,
  _nrrdFormatNRRD_nameLooksLike,
  _nrrdFormatNRRD_fitsInto,
  _nrrdFormatNRRD_contentStartsLike,
  _nrrdFormatNRRD_read,
  _nrrdFormatNRRD_write
};

const NrrdFormat *const
nrrdFormatNRRD = &_nrrdFormatNRRD;
