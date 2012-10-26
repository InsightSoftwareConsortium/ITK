/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2012, 2011, 2010, 2009  University of Chicago
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

#if TEEM_BZIP2
#include <bzlib.h>
#endif

/* The "/ *Teem:" (without space) comments in here are an experiment */

char _nrrdRelativePathFlag[] = "./";
char _nrrdFieldSep[] = " \t";
char _nrrdLineSep[] = "\r\n";
char _nrrdNoSpaceVector[] = "none";
char _nrrdTextSep[] = " ,\t";

/*
** return length of next "line" in nio->headerStringRead
*/
unsigned int
_nrrdHeaderStringOneLineStrlen(NrrdIoState *nio) {

  return AIR_CAST(unsigned int,
                  strcspn(nio->headerStringRead + nio->headerStrpos, _nrrdLineSep));
}

/*
** read next "line" in nio->headerStringRead
*/
unsigned int
_nrrdHeaderStringOneLine(NrrdIoState *nio) {
  unsigned int len1, len2;

  len1 = _nrrdHeaderStringOneLineStrlen(nio);
  strncpy(nio->line, nio->headerStringRead + nio->headerStrpos, len1);
  nio->line[len1] = '\0';
  nio->headerStrpos += len1;
  len2 = AIR_CAST(unsigned int,
                  strspn(nio->headerStringRead + nio->headerStrpos, _nrrdLineSep));
  nio->headerStrpos += len2;
  return len1;
}

/*
** _nrrdOneLine
**
** wrapper around airOneLine; does re-allocation of line buffer
** ("line") in the NrrdIoState if needed.  The return value semantics
** are similar, except that what airOneLine would return, we put
** in *lenP.  If there is an error (airOneLine returned 0,
** something couldn't be allocated), *lenP is set to 0, and
** we return 1.  HITTING EOF IS NOT ACTUALLY AN ERROR, see code
** below.  Otherwise we return 0.
**
** Does use biff
*/
int
_nrrdOneLine(unsigned int *lenP, NrrdIoState *nio, FILE *file) {
  static const char me[]="_nrrdOneLine";
  char **line;
  airArray *mop, *lineArr;
  airPtrPtrUnion appu;
  unsigned int lineIdx, len, needLen;

  if (!( lenP && nio && (file || nio->headerStringRead))) {
    biffAddf(NRRD, "%s: got NULL pointer (%p, %p, %p/%p)", me,
             AIR_CAST(void*, lenP), AIR_CAST(void*, nio),
             AIR_CAST(void*, file), nio->headerStringRead);
    return 1;
  }
  if (0 == nio->lineLen) {
    /* nio->line hasn't been allocated for anything */
    nio->lineLen = 3;
    nio->line = (char*)malloc(nio->lineLen);
    if (!nio->line) {
      biffAddf(NRRD, "%s: couldn't alloc %d-char line\n", me, nio->lineLen);
      *lenP = 0; return 1;
    }
  }
  if (file) {
    len = airOneLine(file, nio->line, nio->lineLen);
  } else {
    /* NOTE: NULL-ity error check above makes this safe */
    needLen = _nrrdHeaderStringOneLineStrlen(nio);
    if (needLen+1 > nio->lineLen) {
      nio->lineLen = needLen+1;
      airFree(nio->line);  /* lose previous allocated line */
      nio->line = (char*)malloc(nio->lineLen);
      if (!nio->line) {
        biffAddf(NRRD, "%s: couldn't alloc %d-char line\n",
                 me, nio->lineLen);
        *lenP = 0; return 1;
      }
    }
    len = _nrrdHeaderStringOneLine(nio);
  }
  if (len <= nio->lineLen) {
    /* otherwise we hit EOF (or end of nio->headerStringRead) before a
       newline, or the line (possibly empty) fit within the nio->line,
       neither of which is an error here */
    *lenP = len;
  } else {
    /* line didn't fit in buffer, so we have to increase line
       buffer size and put the line together in pieces */
    /* NOTE: this will never happen when reading from nio->headerStringRead */
    appu.cp = &line;
    lineArr = airArrayNew(appu.v, NULL, sizeof(char *), 1);
    if (!lineArr) {
      biffAddf(NRRD, "%s: couldn't allocate airArray", me);
      *lenP = 0; return 1;
    }
    airArrayPointerCB(lineArr, airNull, airFree);
    mop = airMopNew();
    airMopAdd(mop, lineArr, (airMopper)airArrayNuke, airMopAlways);
    while (len == nio->lineLen+1) {
      lineIdx = airArrayLenIncr(lineArr, 1);
      if (!lineArr->data) {
        biffAddf(NRRD, "%s: couldn't increment line buffer array", me);
        *lenP = 0; airMopError(mop); return 1;
      }
      line[lineIdx] = nio->line;
      nio->lineLen *= 2;
      nio->line = (char*)malloc(nio->lineLen);
      if (!nio->line) {
        biffAddf(NRRD, "%s: couldn't alloc %d-char line\n",
                 me, nio->lineLen);
        *lenP = 0; airMopError(mop); return 1;
      }
      len = airOneLine(file, nio->line, nio->lineLen);
    }
    /* last part did fit in nio->line buffer, also save this into line[] */
    lineIdx = airArrayLenIncr(lineArr, 1);
    if (!lineArr->data) {
      biffAddf(NRRD, "%s: couldn't increment line buffer array", me);
      *lenP = 0; airMopError(mop); return 1;
    }
    line[lineIdx] = nio->line;
    nio->lineLen *= 3;  /* for good measure */
    nio->line = (char*)malloc(nio->lineLen);
    if (!nio->line) {
      biffAddf(NRRD, "%s: couldn't alloc %d-char line\n", me, nio->lineLen);
      *lenP = 0; airMopError(mop); return 1;
    }
    /* now concatenate everything into a new nio->line */
    strcpy(nio->line, "");
    for (lineIdx=0; lineIdx<lineArr->len; lineIdx++) {
      strcat(nio->line, line[lineIdx]);
    }
    /* HEY: API is bad: *lenP should be a size_t pointer! */
    *lenP = AIR_UINT(strlen(nio->line)) + 1;
    airMopError(mop);
  }
  return 0;
}

/*
** _nrrdCalloc()
**
** allocates the data for the array, but only if necessary (as informed by
** nio->oldData and nio->oldDataSize).
**
** as a recent feature, this will handle the extra work of allocating
** memory in the special way required for direct IO, if possible.  For
** this to work, though, the FILE *file has to be passed.  Since file
** is not otherwise needed, it can be passed as NULL for non-direct-IO
** situations.  In any case, if the directIO-compatible allocation fails
** its not error, and we revert to regular allocation.
**
** NOTE: this assumes the checking that is done by _nrrdHeaderCheck
*/
int
_nrrdCalloc(Nrrd *nrrd, NrrdIoState *nio, FILE *file) {
  static const char me[]="_nrrdCalloc";
  size_t needDataSize;
  int fd;

  needDataSize = nrrdElementNumber(nrrd)*nrrdElementSize(nrrd);
  if (nio->oldData &&  needDataSize == nio->oldDataSize) {
    /* re-use old data */
    nrrd->data = nio->oldData;
    /* its not an error to have a directIO-incompatible pointer, so
       there's no other error checking to do here */
  } else {
    nrrd->data = airFree(nrrd->data);
    fd = file ? fileno(file) : -1;
    if (nrrdEncodingRaw == nio->encoding
        && -1 != fd
        && airNoDio_okay == airDioTest(fd, NULL, needDataSize)) {
      nrrd->data = airDioMalloc(needDataSize, fd);
    }
    if (!nrrd->data) {
      /* directIO-compatible allocation wasn't tried, or it failed */
      nrrd->data = malloc(needDataSize);
    }
    if (!nrrd->data) {
      char stmp1[AIR_STRLEN_SMALL], stmp2[AIR_STRLEN_SMALL];
      biffAddf(NRRD, "%s: couldn't allocate %s things of size %s", me,
               airSprintSize_t(stmp1, nrrdElementNumber(nrrd)),
               airSprintSize_t(stmp2, nrrdElementSize(nrrd)));
      return 1;
    }
  }
  /* make it look like it came from calloc(), as used by nrrdNew() */
  memset(nrrd->data, 0, needDataSize);
  return 0;
}

/*
******** nrrdLineSkip
**
** public for the sake of things like "unu make"
** uses the NrrdIoState for its line buffer (used by _nrrdOneLine)
*/
int
nrrdLineSkip(FILE *dataFile, NrrdIoState *nio) {
  static const char me[]="nrrdLineSkip";
  unsigned int lsi, skipRet;

  /* For compressed data: If you don't actually have ascii headers on
     top of your gzipped data then you will potentially huge lines
     while _nrrdOneLine looks for line terminations.  Quoting Gordon:
     "Garbage in, Garbage out." */

  if (!( dataFile && nio )) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }

  for (lsi=0; lsi<nio->lineSkip; lsi++) {
    if (_nrrdOneLine(&skipRet, nio, dataFile)) {
      biffAddf(NRRD, "%s: error skipping line %u of %u",
               me, lsi+1, nio->lineSkip);
      return 1;
    }
    if (!skipRet) {
      biffAddf(NRRD, "%s: hit EOF skipping line %u of %u",
               me, lsi+1, nio->lineSkip);
      return 1;
    }
  }
  return 0;
}

/*
******** nrrdByteSkip
**
** public for the sake of things like "unu make"
** uses nio for information about how much data should actually be skipped
** with negative byteSkip
*/
int
nrrdByteSkip(FILE *dataFile, Nrrd *nrrd, NrrdIoState *nio) {
  static const char me[]="nrrdByteSkip";
  int skipRet;
  size_t bsize;

  if (!( dataFile && nrrd && nio )) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (nio->encoding->isCompression) {
    biffAddf(NRRD, "%s: this function can't work with compressed "
             "encoding %s", me, nio->encoding->name);
    return 1;
  }
  if (nio->byteSkip < 0) {
    long backwards;
    if (nrrdEncodingRaw != nio->encoding) {
      biffAddf(NRRD, "%s: this function can do backwards byte skip only "
               "in %s encoding, not %s", me,
               nrrdEncodingRaw->name, nio->encoding->name);
      return 1;
    }
    if (stdin == dataFile) {
      biffAddf(NRRD, "%s: can't fseek on stdin", me);
      return 1;
    }
    bsize = nrrdElementNumber(nrrd)/_nrrdDataFNNumber(nio);
    bsize *= nrrdElementSize(nrrd);
    /* backwards is (positive) number of bytes AFTER data that we ignore */
    backwards = -nio->byteSkip - 1;
    /* HEY what if bsize fits in size_t but not in (signed) long? */
    if (fseek(dataFile, -AIR_CAST(long, bsize) - backwards, SEEK_END)) {
      char stmp[AIR_STRLEN_SMALL];
      biffAddf(NRRD, "%s: failed to fseek(dataFile, %s, SEEK_END)", me,
               airSprintSize_t(stmp, bsize));
      return 1;
    }
    if (nrrdStateVerboseIO >= 2) {
      fprintf(stderr, "(%s: actually skipped %d bytes)\n",
              me, (int)ftell(dataFile));
    }
  } else {
    if ((stdin == dataFile) || (-1==fseek(dataFile, nio->byteSkip, SEEK_CUR))) {
      long skipi;
      /* fseek failed, perhaps because we're reading stdin, so
         we revert to consuming the input one byte at a time */
      for (skipi=0; skipi<nio->byteSkip; skipi++) {
        skipRet = fgetc(dataFile);
        if (EOF == skipRet) {
          biffAddf(NRRD, "%s: hit EOF skipping byte %ld of %ld",
                   me, skipi, nio->byteSkip);
          return 1;
        }
      }
    }
  }
  return 0;
}

/*
** _nrrdRead()
**
** read in nrrd from a given file *OR* given string.  The main job of
** this function is to start reading the file/string, to determine the
** format, and then call the appropriate format's reader.  This means
** that the various encoding (data) readers can assume that
** nio->format is usefully set.
**
** If (file), the only input information that nio is used for is
** nio->path, so that detached header-relative data files can be
** found. If (string), the headerStr-related fields in the _nio will
** be set/used
*/
int
_nrrdRead(Nrrd *nrrd, FILE *file, const char *string, NrrdIoState *_nio) {
  static const char me[]="_nrrdRead";
  unsigned int llen;
  NrrdIoState *nio;
  int nfi;
  airArray *mop;

  /* sanity check, for good measure */
  if (!nrrdSanity()) {
    biffAddf(NRRD, "%s: sanity check FAILED: have to fix and re-compile",
             me);
    return 1;
  }

  if (!((file || string) && nrrd)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (file && string) {
    biffAddf(NRRD, "%s: can't read from both file and string", me);
    return 1;
  }
  mop = airMopNew();
  if (_nio) {
    nio = _nio;
  } else {
    nio = nrrdIoStateNew();
    if (!nio) {
      biffAddf(NRRD, "%s: couldn't alloc I/O struct", me);
      return 1;
    }
    airMopAdd(mop, nio, (airMopper)nrrdIoStateNix, airMopAlways);
  }

  /* remember old data pointer and allocated size.  Whether or not to
     free() this memory will be decided later */
  nio->oldData = nrrd->data;
  nio->oldDataSize = (nio->oldData
                      ? nrrdElementNumber(nrrd)*nrrdElementSize(nrrd)
                      : 0);
  /*
  fprintf(stderr, "!%s: nio->oldData = %p, oldDataSize = %d\n", me,
          nio->oldData, (int)(nio->oldDataSize));
  */
  nrrd->data = NULL;

  /* initialize given nrrd (but we have thwarted freeing existing memory)  */
  nrrdInit(nrrd);

  /* tell the nio where to find the string to read from */
  nio->headerStringRead = string;

  if (_nrrdOneLine(&llen, nio, file)) {
    biffAddf(NRRD, "%s: error getting first line (containing \"magic\")",
             me);
    airMopError(mop); return 1;
  }
  if (!llen) {
     biffAddf(NRRD, "%s: immediately hit EOF", me);
     airMopError(mop); return 1;
  }

  nio->format = nrrdFormatUnknown;
  for (nfi = nrrdFormatTypeUnknown+1;
       nfi < nrrdFormatTypeLast;
       nfi++) {
    if (nrrdFormatArray[nfi]->contentStartsLike(nio)) {
      nio->format = nrrdFormatArray[nfi];
      break;
    }
  }
  if (nrrdFormatUnknown == nio->format) {
    char linestart[AIR_STRLEN_SMALL], stmp[AIR_STRLEN_SMALL];
    airStrcpy(linestart, AIR_STRLEN_SMALL, nio->line);
    if (strlen(linestart) != strlen(nio->line)) {
      biffAddf(NRRD, "%s: couldn't parse (length %s) line starting "
               "with \"%s\" as magic or beginning of any recognized format",
               me, airSprintSize_t(stmp, strlen(nio->line)), linestart);
    } else {
      biffAddf(NRRD, "%s: couldn't parse \"%s\" as magic or beginning "
               "of any recognized format", me, nio->line);
    }
    airMopError(mop); return 1;
  }
  if (string && nrrdFormatNRRD != nio->format) {
    biffAddf(NRRD, "%s: sorry, can only read %s files from strings (not %s)",
             me, nrrdFormatNRRD->name, nio->format->name);
    airMopError(mop); return 1;
  }

  /* try to read the file */
  if (nio->format->read(file, nrrd, nio)) {
    biffAddf(NRRD, "%s: trouble reading %s file", me, nio->format->name);
    airMopError(mop); return 1;
  }

  /* reshape up grayscale images, if desired */
  if (nio->format->isImage && 2 == nrrd->dim && nrrdStateGrayscaleImage3D) {
    if (nrrdAxesInsert(nrrd, nrrd, 0)) {
      biffAddf(NRRD, "%s:", me);
      return 1;
    }
  }

  /* free prior memory if we didn't end up using it */
  /* HEY: could actually do a check on the nio to refine this */
  if (nio->oldData != nrrd->data) {
    nio->oldData = airFree(nio->oldData);
    nio->oldDataSize = 0;
  }

  /* finally, make sure that what we're returning isn't malformed somehow,
     except that we (probably stupidly) allow nrrd->data to be NULL, given
     the possibility of using nio->skipData */
  if (_nrrdCheck(nrrd, AIR_FALSE, AIR_TRUE)) {
    biffAddf(NRRD, "%s: problem with nrrd after reading", me);
    return 1;
  }

  airMopOkay(mop);
  return 0;
}

/*
******** nrrdRead()
**
** now just a wrapper around _nrrdRead(); reads a NRRD from a FILE *
*/
int
nrrdRead(Nrrd *nrrd, FILE *file, NrrdIoState *_nio) {
  static const char me[]="nrrdRead";

  if (_nrrdRead(nrrd, file, NULL, _nio)) {
    biffAddf(NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

/*
******** nrrdStringRead()
**
** also a wrapper around _nrrdRead(); reads a NRRD from a char *.
**
** Because the same underlying _nrrdRead() is used, the same semantics
** about using existing nrrd->data when possible applies, as does the
** action of nrrdStateGrayscaleImage3D
*/
int
nrrdStringRead(Nrrd *nrrd, const char *string, NrrdIoState *_nio) {
  static const char me[]="nrrdRead";

  if (_nrrdRead(nrrd, NULL, string, _nio)) {
    biffAddf(NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

/*
** _nrrdSplitName()
**
** splits a file name into a path and a base filename.  The path
** separator is '/', but there is a hack (thanks Torsten Rohlfing)
** which allows '\' to work on Windows.  The division between the path
** and the base is the last path separator in the file name.  The path
** is everything prior to this, and base is everything after (so the
** base does NOT start with the path separator).  If there is not a
** '/' in the name, or if a path separator appears as the last
** character, then the path is set to ".", and the name is copied into
** base.
*/
void
_nrrdSplitName(char **dirP, char **baseP, const char *name) {
  char *where;

  if (dirP) {
    *dirP = (char *)airFree(*dirP);
  }
  if (baseP) {
    *baseP = (char *)airFree(*baseP);
  }
  where = strrchr(name, '/');
#ifdef _WIN32
  /* Deal with Windows "\" path separators; thanks to Torsten Rohlfing */
  if ( !where || (strrchr(name, '\\') > where) ) {
    where = strrchr(name, '\\');
  }
#endif
  /* we found a valid break if the last directory character
     is somewhere in the string except the last character */
  if (where && airStrlen(where) > 1) {
    if (dirP) {
      *dirP = airStrdup(name);
      (*dirP)[where - name] = 0;
    }
    if (baseP) {
      *baseP = airStrdup(where + 1);
    }
  } else {
    /* if the name had no slash, its in the current directory, which
       means that we need to explicitly store "." as the header
       directory in case we have header-relative data. */
    if (dirP) {
      *dirP = airStrdup(".");
    }
    if (baseP) {
      *baseP = airStrdup(name);
    }
  }
  return;
}

/*
******** nrrdLoad()
**
**
**
** call tree for this, to help figure out what's going on
**
**   read.c/nrrdLoad
**    | read.c/_nrrdSplitName
**    | read.c/nrrdRead
**       | nio->format->read
**       = formatNRRD.c/_nrrdFormatNRRD_read:
**          | read.c/_nrrdOneLine
**          | parseNrrd.c/_nrrdReadNrrdParseField
**          | parseNrrd.c/nrrdFieldInfoParse[]
**          = parseNrrd.c/_nrrdReadNrrdParse_data_file
**             | fopen(dataName)
**          | formatNRRD.c/_nrrdHeaderCheck
**          | read.c/nrrdLineSkip
**          | read.c/nrrdByteSkip
**          | nio->encoding->read
**          = encodingRaw.c/_nrrdEncodingRaw_read
**             | read.c/_nrrdCalloc
**          | formatNRRD.c/nrrdSwapEndian
**          | miscAir.c/airFclose
**
** (more documentation here)
**
** sneakiness: returns 2 if the reason for problem was a failed fopen().
**
*/
int /*Teem: biff if (ret) */
nrrdLoad(Nrrd *nrrd, const char *filename, NrrdIoState *nio) {
  static const char me[]="nrrdLoad";
  FILE *file;
  airArray *mop;

  if (!(nrrd && filename)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  mop = airMopNew();
  if (!nio) {
    nio = nrrdIoStateNew();
    if (!nio) {
      biffAddf(NRRD, "%s: couldn't alloc I/O struct", me);
      return 1;
    }
    airMopAdd(mop, nio, (airMopper)nrrdIoStateNix, airMopAlways);
  }

  /* we save the directory of the filename given to us so that if it turns
     out that this is a detached header with a header-relative data file,
     then we will know how to find the data file */
  _nrrdSplitName(&(nio->path), NULL, filename);
  /* printf("!%s: |%s|%s|\n", me, nio->dir, nio->base); */

  if (!( file = airFopen(filename, stdin, "rb") )) {
    biffAddf(NRRD, "%s: fopen(\"%s\",\"rb\") failed: %s",
             me, filename, strerror(errno));
    airMopError(mop); return 2;
  }
  airMopAdd(mop, file, (airMopper)airFclose, airMopOnError);
  /* non-error exiting is handled below */

  if (nrrdRead(nrrd, file, nio)) {
    biffAddf(NRRD, "%s: trouble reading \"%s\"", me, filename);
    airMopError(mop); return 1;
  }

  if (nrrdFormatNRRD == nio->format
      && nio->keepNrrdDataFileOpen
      && file == nio->dataFile ) {
    /* we have to keep the datafile open.  If was attached, we can't
       close file, because that is the datafile.  If was detached,
       file != nio->dataFile, so we can close file.  */
  } else {
    /* always close non-NRRD files */
    airFclose(file);
  }

  airMopOkay(mop);
  return 0;
}

int
nrrdLoadMulti(Nrrd *const *nin, unsigned int ninLen,
              const char *fnameFormat,
              unsigned int numStart, NrrdIoState *nio) {
  static const char me[]="nrrdLoadMulti";
  char *fname;
  airArray *mop;
  unsigned int nii;

  if (!(nin && fnameFormat)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (!( _nrrdContainsPercentThisAndMore(fnameFormat, 'u') )) {
    biffAddf(NRRD, "%s: given format \"%s\" doesn't seem to "
             "have the \"%%u\" conversion specification to sprintf "
             "an unsigned int\n", me, fnameFormat);
    return 1;
  }

  mop = airMopNew();
  /* should be big enough for the number replacing the format sequence */
  fname = AIR_CAST(char *, malloc(strlen(fnameFormat) + 128));
  if (!(fname)) {
    biffAddf(NRRD, "%s: couldn't allocate local fname buffer", me);
    airMopError(mop); return 1;
  }
  airMopAdd(mop, fname, airFree, airMopAlways);

  for (nii=0; nii<ninLen; nii++) {
    unsigned int num;
    num = numStart + nii;
    sprintf(fname, fnameFormat, num);
    if (nrrdLoad(nin[nii], fname, nio)) {
      biffAddf(NRRD, "%s: trouble loading nin[%u] from %s", me, nii, fname);
      airMopError(mop); return 1;
    }
    /* HEY: GLK hopes that the nio doesn't have any state that needs
       resetting, but we can't call nrrdIoStateInit() because that
       would negate the purpose of sending in the nio for all but the
       first saved nrrd */
  }

  airMopOkay(mop);
  return 0;
}
