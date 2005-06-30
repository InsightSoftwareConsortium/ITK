/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2005  Gordon Kindlmann
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

#define MAGIC "NRRD"
#define MAGIC0 "NRRD00.01"
#define MAGIC1 "NRRD0001"
#define MAGIC2 "NRRD0002"
#define MAGIC3 "NRRD0003"
#define MAGIC4 "NRRD0004"
#define MAGIC5 "NRRD0005"

void
nrrdIoStateDataFileIterBegin(NrrdIoState *nio) {

  nio->dataFNIndex = -1;
  return;
}

#define _NEED_PATH(str) ('/' != (str)[0] && strcmp("-", (str)))

/*
** this is responsible for the header-relative path processing
**
** NOTE: if the filename is "-", then because it does not start with '/',
** it would normally be prefixed by nio->path, so it needs special handling
*/
int
nrrdIoStateDataFileIterNext(FILE **fileP, NrrdIoState *nio, int reading) {
  char me[]="nrrdIoStateDataFileIterNext", *err;
  char *fname=NULL;
  int ii, num, needPath;
  size_t maxl;
  airArray *mop;

  mop = airMopNew();
  airMopAdd(mop, (void*)fileP, (airMopper)airSetNull, airMopOnError);

  if (!fileP) {
    if ((err = (char*)malloc(AIR_STRLEN_MED))) {
      sprintf(err, "%s: got NULL pointer", me);
      biffAdd(NRRD, err); free(err);
    }
    airMopError(mop); return 1;
  }
  if (!_nrrdDataFNNumber(nio)) {
    if ((err = (char*)malloc(AIR_STRLEN_MED))) {
      sprintf(err, "%s: there appear to be zero datafiles!", me);
      biffAdd(NRRD, err); free(err);
    }
    airMopError(mop); return 1;
  }

  nio->dataFNIndex++;
  if (nio->dataFNIndex >= (int)_nrrdDataFNNumber(nio)) {
    /* there is no next data file, but we don't make that an error */
    nio->dataFNIndex = _nrrdDataFNNumber(nio);
    airMopOkay(mop);
    *fileP = NULL;
    return 0;
  }

  /* HEY: some of this error checking is done far more often than needed */
  if (nio->dataFNFormat || nio->dataFNArr->len) {
    needPath = AIR_FALSE;
    maxl = 0;
    if (nio->dataFNFormat) {
      needPath = _NEED_PATH(nio->dataFNFormat);
      /* assuming 10-digit integers is plenty big */
      maxl = 10 + strlen(nio->dataFNFormat);
    } else {
      for (ii=0; ii<nio->dataFNArr->len; ii++) {
        needPath |= _NEED_PATH(nio->dataFN[ii]);
        maxl = AIR_MAX(maxl, strlen(nio->dataFN[ii]));
      }
    }
    if (needPath && !airStrlen(nio->path)) {
      if ((err = (char*)malloc(AIR_STRLEN_MED))) {
        sprintf(err, "%s: need nio->path for header-relative datafiles", me);
        biffAdd(NRRD, err); free(err);
      }
      airMopError(mop); return 1;
    }
    fname = (char*)malloc(airStrlen(nio->path) + strlen("/") + maxl + 1);
    if (!fname) {
      if ((err = (char*)malloc(AIR_STRLEN_MED))) {
        sprintf(err, "%s: couldn't allocate filename buffer", me);
        biffAdd(NRRD, err); free(err);
      }
      airMopError(mop); return 1;
    }
    airMopAdd(mop, fname, airFree, airMopAlways);
  }

  if (nio->dataFNFormat) {
    /* ---------------------------------------------------------- */
    /* --------- base.%d <min> <max> <step> [<dim>] ------------- */
    /* ---------------------------------------------------------- */
    num = 0;
    for (ii = nio->dataFNMin; 
         ((nio->dataFNStep > 0 && ii <= nio->dataFNMax)
          || (nio->dataFNStep < 0 && ii >= nio->dataFNMax));
         ii += nio->dataFNStep) {
      if (num == nio->dataFNIndex) {
        break;
      }
      num += 1;
    }
    if (_NEED_PATH(nio->dataFNFormat)) {
      strcpy(fname, nio->path);
      strcat(fname, "/");
      sprintf(fname + strlen(nio->path) + strlen("/"), nio->dataFNFormat, ii);
    } else {
      sprintf(fname, nio->dataFNFormat, ii);
    }
  } else if (nio->dataFNArr->len) {
    /* ---------------------------------------------------------- */
    /* ------------------- LIST or single ----------------------- */
    /* ---------------------------------------------------------- */
    if (_NEED_PATH(nio->dataFN[nio->dataFNIndex])) {
      sprintf(fname, "%s/%s", nio->path, nio->dataFN[nio->dataFNIndex]);
    } else {
      strcpy(fname, nio->dataFN[nio->dataFNIndex]);
    }
  }
  /* else data file is attached */
  
  if (nio->dataFNFormat || nio->dataFNArr->len) {
    *fileP = airFopen(fname, reading ? stdin : stdout, reading ? "rb" : "wb");
    if (!(*fileP)) {
      if ((err = (char*)malloc(strlen(fname) + AIR_STRLEN_MED))) {
        sprintf(err, "%s: couldn't open \"%s\" (data file %d of %d) for %s",
                me, fname, nio->dataFNIndex+1, (int)_nrrdDataFNNumber(nio),
                reading ? "reading" : "writing");
        biffAdd(NRRD, err); free(err);
      }
      airMopError(mop); return 1;
    }
  } else {
    /* data file is attached */
    *fileP = nio->headerFile;
  }
  
  airMopOkay(mop);
  return 0;
}

/* 
** we try to use the oldest format that will hold the nrrd 
*/
int
_nrrdFormatNRRD_whichVersion(const Nrrd *nrrd, NrrdIoState *nio) {
  int ret;

  if (_nrrdFieldInteresting(nrrd, nio, nrrdField_measurement_frame)) {
    ret = 5;
  } else if (_nrrdFieldInteresting(nrrd, nio, nrrdField_thicknesses)
             || _nrrdFieldInteresting(nrrd, nio, nrrdField_space)
             || _nrrdFieldInteresting(nrrd, nio, nrrdField_space_dimension)
             || _nrrdFieldInteresting(nrrd, nio, nrrdField_sample_units)
             || airStrlen(nio->dataFNFormat) || nio->dataFNArr->len > 1) {
    ret = 4;
  } else if (_nrrdFieldInteresting(nrrd, nio, nrrdField_kinds)) {
    ret = 3;
  } else if (nrrdKeyValueSize(nrrd)) {
    ret = 2;
  } else {
    ret = 1;
  }
  return ret;
}

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
          || !strcmp(MAGIC4, nio->line)
          || !strcmp(MAGIC5, nio->line)
          );
}

/*
** _nrrdHeaderCheck()
**
** minimal consistency checks on relationship between fields of nrrd,
** only to be used after the headers is parsed, and before the data is
** read, to make sure that information required for reading data is in
** fact known.
**
** NOTE: this is not the place to do the sort of checking done by 
** nrrdCheck(), because it includes I/O-specific stuff
**
*/
int
_nrrdHeaderCheck (Nrrd *nrrd, NrrdIoState *nio, int checkSeen) {
  char me[]="_nrrdHeaderCheck", err[AIR_STRLEN_MED];
  int i;

  if (checkSeen) {
    for (i=1; i<=NRRD_FIELD_MAX; i++) {
      if (_nrrdFieldRequired[i] && !nio->seen[i]) {
        sprintf(err, "%s: didn't see required field: %s",
                me, airEnumStr(nrrdField, i));
        biffAdd(NRRD, err); return 1;
      }
    }
  }
  if (nrrdTypeBlock == nrrd->type && !nrrd->blockSize) {
    sprintf(err, "%s: type is %s, but missing field: %s", me,
            airEnumStr(nrrdType, nrrdTypeBlock),
            airEnumStr(nrrdField, nrrdField_block_size));
    biffAdd(NRRD, err); return 1;
  }
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
** NOTE: currently, this will read advanced NRRD format features 
** from old NRRD files (with old magic), such as key/value pairs
** from a NRRD0001 file, without any complaints even though strictly
** speaking these are violations of the format.
**
** NOTE: by giving a NULL "file", you can make this function basically
** do the work of reading in datafiles, without any header parsing 
*/
int
_nrrdFormatNRRD_read(FILE *file, Nrrd *nrrd, NrrdIoState *nio) {
  char me[]="_nrrdFormatNRRD_read", 
    *err; /* NOTE: err really does have to be dynamically 
             allocated because of the arbitrary-sized input lines
             that it may have to copy */
  int ret, len;
  size_t valsPerPiece;
  char *data;
  FILE *dataFile=NULL;

  /* record where the header is being read from for the sake of
     nrrdIoStateDataFileIterNext() */
  nio->headerFile = file;

  if (file) {
    if (!_nrrdFormatNRRD_contentStartsLike(nio)) {
      if ((err = (char*)malloc(AIR_STRLEN_MED))) {
        sprintf(err, "%s: this doesn't look like a %s file", me,
                nrrdFormatNRRD->name);
        biffAdd(NRRD, err); free(err); 
      }
      return 1;
    }
    /* parse all the header lines */
    do {
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
        if (nrrdFieldInfoParse[ret](file, nrrd, nio, AIR_TRUE)) {
          if ((err = (char*)malloc(AIR_STRLEN_MED))) {
            /* HEY: this error message should be printing out all the
               per-axis fields, not just the first
               HEY: if your stupid parsing functions didn't modify
               nio->line then you wouldn't have this problem ... */
            sprintf(err, "%s: trouble parsing %s info \"%s\"", me,
                    airEnumStr(nrrdField, ret), nio->line + nio->pos);
            biffAdd(NRRD, err); free(err);
          }
          return 1;
        }
        nio->seen[ret] = AIR_TRUE;
      }
    } while (len > 1);
    /* either
       0 == len: we're at EOF, or
       1 == len: we just read the empty line seperating header from data */
    if (0 == len 
        && !nio->dataFNFormat
        && 0 == nio->dataFNArr->len) { 
      /* we're at EOF, but there's apparently no seperate data file */
      if ((err = (char*)malloc(AIR_STRLEN_MED))) {
        sprintf(err, "%s: hit end of header, but no \"%s\" given", me,
                airEnumStr(nrrdField, nrrdField_data_file));
        biffAdd(NRRD, err); free(err);
      }
      return 1;
    }
  }
  if (_nrrdHeaderCheck(nrrd, nio, !!file)) {
    if ((err = (char*)malloc(AIR_STRLEN_MED))) {
      sprintf(err, "%s: %s", me, 
              (len ? "finished reading header, but there were problems"
               : "hit EOF before seeing a complete valid header"));
      biffAdd(NRRD, err); free(err);
    }
    return 1;
  }


  /* we seemed to have read in a valid header; now allocate the memory */
  /* for directIO-compatible allocation we need to get the first datafile */
  nrrdIoStateDataFileIterBegin(nio);
  if (nrrdIoStateDataFileIterNext(&dataFile, nio, AIR_TRUE)) {
    if ((err = (char*)malloc(AIR_STRLEN_MED))) {
      sprintf(err, "%s: couldn't open the first datafile", me);
      biffAdd(NRRD, err); free(err);
    }
    return 1;
  }
  if (nio->skipData) {
    nrrd->data = NULL;
    data = NULL;
  } else {
    if (_nrrdCalloc(nrrd, nio, dataFile)) {
      if ((err = (char*)malloc(AIR_STRLEN_MED))) {
        sprintf(err, "%s: couldn't allocate memory for data", me);
        biffAdd(NRRD, err); free(err);
      }
      return 1;
    }
    data = (char*)nrrd->data;
  }

  /* iterate through datafiles and read them in */
  /* NOTE: you have to open dataFile even in the case of skipData, because
     caller might have set keepNrrdDataFileOpen, in which case you need to
     do any line or byte skipping if it is specified */
  valsPerPiece = nrrdElementNumber(nrrd)/_nrrdDataFNNumber(nio);
  do {
    /* ---------------- skip, if need be */
    if (nrrdLineSkip(dataFile, nio)) {
      if ((err = (char*)malloc(AIR_STRLEN_MED))) {
        sprintf(err, "%s: couldn't skip lines", me);
        biffAdd(NRRD, err); free(err);
      }
      return 1;
    }
    if (!nio->encoding->isCompression) {
      /* bytes are skipped here for non-compression encodings, but are
         skipped within the decompressed stream for compression encodings */
      if (nrrdByteSkip(dataFile, nrrd, nio)) {
        if ((err = (char*)malloc(AIR_STRLEN_MED))) {
          sprintf(err, "%s: couldn't skip bytes", me);
          biffAdd(NRRD, err); free(err);
        }
        return 1;
      }
    }
    /* ---------------- read the data itself */
    if (2 <= nrrdStateVerboseIO) {
      fprintf(stderr, "(%s: reading %s data ... ", me, nio->encoding->name);
      fflush(stderr);
    }
    if (!nio->skipData) {
      if (nio->encoding->read(dataFile, data, valsPerPiece, nrrd, nio)) {
        if (2 <= nrrdStateVerboseIO) {
          fprintf(stderr, "error!\n");
        }
        if ((err = (char*)malloc(AIR_STRLEN_MED))) {
          sprintf(err, "%s:", me);
          biffAdd(NRRD, err); free(err);
        }
        return 1;
      }
    }
    if (2 <= nrrdStateVerboseIO) {
      fprintf(stderr, "done)\n");
    }
    /* ---------------- go to next data file */
    if (nio->keepNrrdDataFileOpen && _nrrdDataFNNumber(nio) == 1) {
      nio->dataFile = dataFile;
    } else {
      if (dataFile != nio->headerFile) {
        dataFile = airFclose(dataFile);
      }
    }
    data += valsPerPiece*nrrdElementSize(nrrd);
    if (nrrdIoStateDataFileIterNext(&dataFile, nio, AIR_TRUE)) {
      if ((err = (char*)malloc(AIR_STRLEN_MED))) {
        sprintf(err, "%s: couldn't get the next datafile", me);
        biffAdd(NRRD, err); free(err);
      }
      return 1;
    }
  } while (dataFile);

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
    
  return 0;
}

int
_nrrdFormatNRRD_write(FILE *file, const Nrrd *nrrd, NrrdIoState *nio) {
  char me[]="_nrrdFormatNRRD_write", err[AIR_STRLEN_MED], *tmp;
  int ii;
  airArray *mop;
  FILE *dataFile=NULL;
  size_t valsPerPiece;
  char *data;

  mop = airMopNew();

  if (nrrdTypeBlock == nrrd->type && nrrdEncodingAscii == nio->encoding) {
    sprintf(err, "%s: can't write nrrd type %s to %s", me,
            airEnumStr(nrrdType, nrrdTypeBlock),
            nrrdEncodingAscii->name);
    biffAdd(NRRD, err); airMopError(mop); return 1;
  }

  /* record where the header is being written to for the sake of
     nrrdIoStateDataFileIterNext() */
  nio->headerFile = file;

  /* we have to make sure that the data filename information is set
     (if needed), so that it can be printed by _nrrdFprintFieldInfo */
  if (nio->detachedHeader 
      && !nio->dataFNFormat
      && 0 == nio->dataFNArr->len) {
    /* NOTE: this means someone requested a detached header, but we
       don't already have implicit (via dataFNFormat) or explicit
       (via dataFN[]) information about the data file */
    /* NOTE: whether or not nio->skipData, we have to contrive a filename to 
       say in the "data file" field, which is stored in nio->dataFN[0],
       because the data filename will be "interesting", according to
       _nrrdFieldInteresting() */
    /* NOTE: Fri Feb  4 01:42:20 EST 2005 the way this is now set up, having
       a name in dataFN[0] will trump the name implied by nio->{path,base},
       which is a useful way for the user to explicitly set the output
       data filename (as with unu make -od) */
    if (!( !!airStrlen(nio->path) && !!airStrlen(nio->base) )) {
      sprintf(err, "%s: can't create data file name: nio's "
              "path and base empty", me);
      biffAdd(NRRD, err); airMopError(mop); return 1;
    }
    tmp = (char*)malloc(strlen(nio->base) 
                        + strlen(".")
                        + strlen(nio->encoding->suffix) + 1);
    if (!tmp) {
      sprintf(err, "%s: couldn't allocate data filename", me);
      biffAdd(NRRD, err); airMopError(mop); return 1;
    }
    airMopAdd(mop, tmp, airFree, airMopOnError);
    sprintf(tmp, "%s.%s", nio->base, nio->encoding->suffix);
    ii = airArrayLenIncr(nio->dataFNArr, 1);
    nio->dataFN[ii] = tmp;
  }
  
  fprintf(file, "%s%04d\n", MAGIC, _nrrdFormatNRRD_whichVersion(nrrd, nio));

  /* this is where the majority of the header printing happens */
  for (ii=1; ii<=NRRD_FIELD_MAX; ii++) {
    if (_nrrdFieldInteresting(nrrd, nio, ii)) {
      _nrrdFprintFieldInfo (file, "", nrrd, nio, ii);
    }
  }

  /* comments and key/values handled differently */
  for (ii=0; ii<nrrd->cmtArr->len; ii++) {
    fprintf(file, "%c %s\n", NRRD_COMMENT_CHAR, nrrd->cmt[ii]);
  }
  for (ii=0; ii<nrrd->kvpArr->len; ii++) {
    _nrrdKeyValueFwrite(file, NULL, nrrd->kvp[0 + 2*ii], nrrd->kvp[1 + 2*ii]);
  }

  if (!( nio->detachedHeader || _nrrdDataFNNumber(nio) > 1 )) {
    fprintf(file, "\n");
  }

  if (!nio->skipData) {
    nrrdIoStateDataFileIterBegin(nio);
    if (nrrdIoStateDataFileIterNext(&dataFile, nio, AIR_FALSE)) {
      sprintf(err, "%s: couldn't write the first datafile", me);
      biffAdd(NRRD, err); airMopError(mop); return 1;
    }
    
    valsPerPiece = nrrdElementNumber(nrrd)/_nrrdDataFNNumber(nio);
    data = (char*)nrrd->data;
    do {
      /* ---------------- write data */
      if (2 <= nrrdStateVerboseIO) {
        fprintf(stderr, "(%s: writing %s data ", me, nio->encoding->name);
        fflush(stderr);
      }
      if (nio->encoding->write(dataFile, data, valsPerPiece, nrrd, nio)) {
        if (2 <= nrrdStateVerboseIO) {
          fprintf(stderr, "error!\n");
        }
        sprintf(err, "%s: couldn't write %s data", me, nio->encoding->name);
        biffAdd(NRRD, err); airMopError(mop); return 1;
      }
      if (2 <= nrrdStateVerboseIO) {
        fprintf(stderr, "done)\n");
      }
      /* ---------------- go to next data file */
      if (dataFile != nio->headerFile) {
        dataFile = airFclose(dataFile);
      }
      data += valsPerPiece*nrrdElementSize(nrrd);
      if (nrrdIoStateDataFileIterNext(&dataFile, nio, AIR_TRUE)) {
        sprintf(err, "%s: couldn't get the next datafile", me);
        biffAdd(NRRD, err); airMopError(mop); return 1;
      }
    } while (dataFile);
  }

  airMopOkay(mop); 
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
