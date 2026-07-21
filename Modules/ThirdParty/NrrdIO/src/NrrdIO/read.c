/*
  NrrdIO: C library for NRRD file IO (with optional compressions)
  Copyright (C) 2009--2026  University of Chicago
  Copyright (C) 2005--2008  Gordon Kindlmann
  Copyright (C) 1998--2004  University of Utah

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

#ifdef TEEM_BZIP2
#  if defined(__clang__)
#    pragma clang diagnostic push
#    pragma clang diagnostic ignored "-Wreserved-identifier"
/*   because bzlib.h's "#define _BZLIB_H" has reserved identifier _BZLIB_H */
#  endif
#  include <bzlib.h>
#  if defined(__clang__)
#    pragma clang diagnostic pop
#  endif
#endif

/* (not apparently used) const char *const nrrd__RelativePathFlag = "./"; */
const char *const nrrd__FieldSep = " \t";
static const char *const nrrd__LineSep = "\r\n";
const char *const nrrd__NoSpaceVector = "none";
/* special string that signals that nRead() failed because it hit EOF,
   which is used in hestNrrd.c to give a different parse (error) return value,
   which in turn enables unu's quiet quitting */
const char *const nrrd__ReadHitEOF = ": immediately hit EOF";

/* returns non-zero if given char `cc` is in nrrd__FieldSep */
int /* Biff: (private) nope */
nrrd__CharIsFieldSep(char cc) {
  unsigned int ii, fslen = (unsigned int)strlen(nrrd__FieldSep);
  int ret = 0;
  for (ii = 0; ii < fslen; ii++) {
    if (cc == nrrd__FieldSep[ii]) {
      ret = 1;
      break;
    }
  }
  return ret;
}

/*
** return length of next "line" in nio->headerStringRead
*/
static size_t
headerStringOneLineStrlen(NrrdIoState *nio) {

  return strcspn(nio->headerStringRead + nio->headerStrpos, nrrd__LineSep);
}

/*
** read next "line" in nio->headerStringRead
*/
static size_t
headerStringOneLine(NrrdIoState *nio) {
  size_t len1, len2;

  len1 = headerStringOneLineStrlen(nio);
  strncpy(nio->line, nio->headerStringRead + nio->headerStrpos, len1);
  nio->line[len1] = '\0';
  nio->headerStrpos += len1;
  len2 = strspn(nio->headerStringRead + nio->headerStrpos, nrrd__LineSep);
  nio->headerStrpos += len2;
  return len1;
}

/*
 ******** nrrdOneLine
 *
 * wrapper around airOneLine when reading from `file`, though does re-allocation of line
 * buffer `line` in the NrrdIoState when needed.  The return value semantics
 * are similar, except that what airOneLine would return, we put
 * in `*lenP`.
 * If there is an error (airOneLine returned 0, something couldn't be allocated),
 * *lenP is set to 0, and we return 1.
 * HITTING EOF IS NOT ACTUALLY AN ERROR, see code below.
 * Otherwise we return 0.
 */
int /* Biff: 1 */
nrrdOneLine(size_t *lenP, NrrdIoState *nio, FILE *file) {
  static const char me[] = "nrrdOneLine";
  char **line;
  unsigned int lineIdx;
  size_t len;

  if (!(lenP && nio && (file || nio->headerStringRead))) {
    biffAddf(NRRD, "%s: got NULL pointer (%p, %p, %p/%p)", me, AIR_VOIDP(lenP),
             AIR_VOIDP(nio), AIR_VOIDP(file), AIR_CVOIDP(nio->headerStringRead));
    return 1;
  }
  if (0 == nio->lineSize) {
    /* nio->line hasn't been allocated for anything */
    nio->lineSize = 4;
    nio->line = (char *)malloc(nio->lineSize);
    if (!nio->line) {
      biffAddf(NRRD, "%s: couldn't alloc %zu-char line\n", me, nio->lineSize);
      *lenP = 0;
      return 1;
    }
  } else {
    if (nio->verbose >= 3) {
      fprintf(stderr, "%s: hello came in with nio->lineSize=%zu\n", me, nio->lineSize);
    }
  }
  if (file) {
    len = airOneLine(file, nio->line, nio->lineSize);
    if (nio->verbose >= 3) {
      fprintf(stderr, "%s: (A) got len=%zu |%s|=%zu (lineSize=%zu) from file\n", me, len,
              nio->line, strlen(nio->line), nio->lineSize);
    }
  } else {
    /* NOTE: NULL-ity error check above makes this safe */
    size_t needLen = headerStringOneLineStrlen(nio);
    if (needLen + 1 > nio->lineSize) {
      nio->lineSize = needLen + 1;
      airFree(nio->line); /* lose previous allocated line */
      nio->line = (char *)malloc(nio->lineSize);
      if (!nio->line) {
        biffAddf(NRRD, "%s: couldn't alloc %zu-char line\n", me, nio->lineSize);
        *lenP = 0;
        return 1;
      }
    }
    len = headerStringOneLine(nio);
  }
  if (len <= nio->lineSize) {
    /* otherwise we hit EOF (or end of nio->headerStringRead) before a
       newline, or the line (possibly empty) fit within the nio->line,
       neither of which is an error here */
    *lenP = len;
  } else {
    /* line didn't fit in buffer, so we have to increase line
       buffer size and put the line together in pieces */
    /* NOTE: this will never happen when reading from nio->headerStringRead */
    airArray *mop, *lineArr;
    airPtrPtrUnion appu;
    char *lout;
    size_t loutSize; /* lout used at very end */
    appu.cp = &line;
    lineArr = airArrayNew(appu.v, NULL, sizeof(char *), 1);
    if (!lineArr) {
      biffAddf(NRRD, "%s: couldn't allocate airArray", me);
      *lenP = 0;
      return 1;
    }
    airArrayPointerCB(lineArr, airNull, airFree);
    mop = airMopNew();
    airMopAdd(mop, lineArr, airArrayNuke_mop, airMopAlways);
    while (len == nio->lineSize + 1) { /* means airOneLine maxed out buffer */
      lineIdx = airArrayLenIncr(lineArr, 1);
      if (!lineArr->data) {
        biffAddf(NRRD, "%s: couldn't increment line buffer array", me);
        *lenP = 0;
        airMopError(mop);
        return 1;
      }
      line[lineIdx] = nio->line;
      nio->lineSize *= 2;
      nio->line = AIR_MALLOC(nio->lineSize, char);
      if (!nio->line) {
        biffAddf(NRRD, "%s: couldn't alloc %zu-char line\n", me, nio->lineSize);
        *lenP = 0;
        airMopError(mop);
        return 1;
      }
      len = airOneLine(file, nio->line, nio->lineSize);
      if (nio->verbose >= 3) {
        fprintf(stderr, "%s: (B) got len=%zu |%s|=%zu (lineSize=%zu) from file\n", me,
                len, nio->line, strlen(nio->line), nio->lineSize);
      }
    }
    /* last part did fit in nio->line buffer, also save this into line[]
       because we still need to expand nio->line to hold all previous parts */
    lineIdx = airArrayLenIncr(lineArr, 1);
    if (!lineArr->data) {
      biffAddf(NRRD, "%s: couldn't increment line buffer array", me);
      *lenP = 0;
      airMopError(mop);
      return 1;
    }
    line[lineIdx] = nio->line; /* contains final piece */
    /* this is even big enough for final '\0' because the sum of smaller powers of two is
     *less* than this power of 2 */
    loutSize = 2 * nio->lineSize;
    if (nio->verbose >= 3) {
      fprintf(stderr, "%s: concat of %u lines --> new lineLen=%zu\n", me, lineArr->len,
              loutSize);
    }
    lout = nio->line = AIR_MALLOC(loutSize, char);
    if (!nio->line) {
      biffAddf(NRRD, "%s: couldn't alloc %zu-char line\n", me, nio->lineSize);
      *lenP = 0;
      airMopError(mop);
      return 1;
    }
    nio->lineSize = loutSize;
    /* now concatenate everything into a new nio->line */
    lout[0] = '\0';
    for (lineIdx = 0; lineIdx < lineArr->len; lineIdx++) {
      SN_COPY(lout, loutSize, line[lineIdx]);
    }
    *lenP = strlen(nio->line) + 1;
    airMopError(mop);
  }
  return 0;
}

/*
** nrrd__Calloc()
**
** allocates the data for the array, but only if necessary (as informed by
** nio->oldData and nio->oldDataSize).
**
** NOTE: this assumes the checking that is done by nrrd__HeaderCheck
*/
int /* Biff: (private) 1 */
nrrd__Calloc(Nrrd *nrrd, NrrdIoState *nio) {
  static const char me[] = "nrrd__Calloc";
  size_t needDataSize;

  needDataSize = nrrdElementNumber(nrrd) * nrrdElementSize(nrrd);
  if (nio->oldData && needDataSize == nio->oldDataSize) {
    /* re-use old data */
    nrrd->data = nio->oldData;
    /* its not an error to have a directIO-incompatible pointer, so
       there's no other error checking to do here */
  } else {
    nrrd->data = airFree(nrrd->data);
    if (!nrrd->data) {
      /* allocate data if needed */
      nrrd->data = malloc(needDataSize);
    }
    if (!nrrd->data) {
      char stmp[2][AIR_STRLEN_SMALL + 1];
      biffAddf(NRRD, "%s: couldn't allocate %s things of size %s", me,
               airSprintSize_t(stmp[0], nrrdElementNumber(nrrd)),
               airSprintSize_t(stmp[1], nrrdElementSize(nrrd)));
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
** uses the NrrdIoState for its line buffer (used by nrrdOneLine)
*/
int /* Biff: 1 */
nrrdLineSkip(FILE *dataFile, NrrdIoState *nio) {
  static const char me[] = "nrrdLineSkip";
  unsigned int lsi;
  size_t skipRet;

  /* For compressed data: If you don't actually have ascii headers on
     top of your gzipped data then you will potentially huge lines
     while nrrdOneLine looks for line terminations.  Quoting Gordon:
     "Garbage in, Garbage out." */

  if (!(dataFile && nio)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }

  for (lsi = 0; lsi < nio->lineSkip; lsi++) {
    if (nrrdOneLine(&skipRet, nio, dataFile)) {
      biffAddf(NRRD, "%s: error skipping line %u of %u", me, lsi + 1, nio->lineSkip);
      return 1;
    }
    if (!skipRet) {
      biffAddf(NRRD, "%s: hit EOF skipping line %u of %u", me, lsi + 1, nio->lineSkip);
      return 1;
    }
  }
  return 0;
}

int /* Biff: (private) 1 */
nrrd__ByteSkipSkip(FILE *dataFile, Nrrd *nrrd, NrrdIoState *nio, long int byteSkip) {
  static const char me[] = "nrrd__ByteSkipSkip";
  int skipRet;
  size_t bsize;

  if (!(dataFile && nrrd && nio)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (nio->encoding->isCompression) {
    biffAddf(NRRD,
             "%s: this function can't work with compressed "
             "encoding %s",
             me, nio->encoding->name);
    return 1;
  }
  if (byteSkip < 0) {
    long backwards;
    if (nrrdEncodingRaw != nio->encoding) {
      biffAddf(NRRD,
               "%s: this function can do backwards byte skip only "
               "in %s encoding, not %s",
               me, nrrdEncodingRaw->name, nio->encoding->name);
      return 1;
    }
    if (stdin == dataFile) {
      biffAddf(NRRD, "%s: can't fseek on stdin", me);
      return 1;
    }
    bsize = nrrdElementNumber(nrrd) / nrrdIoDataFNNumber(nio);
    bsize *= nrrdElementSize(nrrd);
    /* backwards is (positive) number of bytes AFTER data that we ignore */
    backwards = -byteSkip - 1;
    /* HEY what if bsize fits in size_t but not in (signed) long? */
    if (fseek(dataFile, -AIR_CAST(long, bsize) - backwards, SEEK_END)) {
      char stmp[AIR_STRLEN_SMALL + 1];
      biffAddf(NRRD, "%s: failed to fseek(dataFile, %s, SEEK_END)", me,
               airSprintSize_t(stmp, bsize));
      return 1;
    }
    if (nio && nio->verbose >= 2) {
      fprintf(stderr, "(%s: actually skipped %d bytes)\n", me, (int)ftell(dataFile));
    }
  } else {
    if ((stdin == dataFile) || (-1 == fseek(dataFile, byteSkip, SEEK_CUR))) {
      long skipi;
      /* fseek failed, perhaps because we're reading stdin, so
         we revert to consuming the input one byte at a time */
      for (skipi = 0; skipi < byteSkip; skipi++) {
        skipRet = fgetc(dataFile);
        if (EOF == skipRet) {
          biffAddf(NRRD, "%s: hit EOF skipping byte %ld of %ld", me, skipi, byteSkip);
          return 1;
        }
      }
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
int /* Biff: 1 */
nrrdByteSkip(FILE *dataFile, Nrrd *nrrd, NrrdIoState *nio) {
  static const char me[] = "nrrdByteSkip";

  if (!(dataFile && nrrd && nio)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  /* HEY: with the advent of NRRD0006 per-file skips, maybe this is
     the function that should be public */
  if (nrrd__ByteSkipSkip(dataFile, nrrd, nio, nio->byteSkip)) {
    biffAddf(NRRD, "%s: trouble", me);
    return 1;
  }

  return 0;
}

/*
** nRead()
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
static int /* Biff: 1 */
nRead(Nrrd *nrrd, FILE *file, const char *string, NrrdIoState *_nio) {
  static const char me[] = "nRead";
  size_t llen;
  NrrdIoState *nio;
  int nfi;
  airArray *mop;

  /* sanity check, for good measure */
  if (!nrrdSanity()) {
    biffAddf(NRRD, "%s: sanity check FAILED: have to fix and re-compile", me);
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
    airMopAdd(mop, nio, nrrdIoStateNix_mop, airMopAlways);
  }

  /* remember old data pointer and allocated size.  Whether or not to
     free() this memory will be decided later */
  nio->oldData = nrrd->data;
  nio->oldDataSize = (nio->oldData ? nrrdElementNumber(nrrd) * nrrdElementSize(nrrd)
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

  if (nrrdOneLine(&llen, nio, file)) {
    biffAddf(NRRD, "%s: error getting first line (containing \"magic\")", me);
    airMopError(mop);
    return 1;
  }
  if (!llen) {
    biffAddf(NRRD, "%s%s", me, nrrd__ReadHitEOF);
    airMopError(mop);
    return 1;
  }

  nio->format = nrrdFormatUnknown;
  for (nfi = nrrdFormatTypeUnknown + 1; nfi < nrrdFormatTypeLast; nfi++) {
    if (nrrdFormatArray[nfi]->contentStartsLike(nio)) {
      nio->format = nrrdFormatArray[nfi];
      break;
    }
  }
  if (nrrdFormatUnknown == nio->format) {
    char linestart[AIR_STRLEN_SMALL + 1], stmp[AIR_STRLEN_SMALL + 1];
    airStrcpy(linestart, sizeof(linestart), nio->line);
    if (strlen(linestart) != strlen(nio->line)) {
      biffAddf(NRRD,
               "%s: couldn't parse (length %s) line starting "
               "with \"%s\" as magic or beginning of any recognized format",
               me, airSprintSize_t(stmp, strlen(nio->line)), linestart);
    } else {
      biffAddf(NRRD,
               "%s: couldn't parse \"%s\" as magic or beginning "
               "of any recognized format",
               me, nio->line);
    }
    airMopError(mop);
    return 1;
  }
  if (string && nrrdFormatNRRD != nio->format) {
    biffAddf(NRRD, "%s: sorry, can only read %s files from strings (not %s)", me,
             nrrdFormatNRRD->name, nio->format->name);
    airMopError(mop);
    return 1;
  }

  /* try to read the file */
  if (nio->format->read(file, nrrd, nio)) {
    biffAddf(NRRD, "%s: trouble reading %s file", me, nio->format->name);
    airMopError(mop);
    return 1;
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
  if (nrrdCheckMore(nrrd, AIR_FALSE, AIR_TRUE)) {
    biffAddf(NRRD, "%s: problem with nrrd after reading", me);
    return 1;
  }

  airMopOkay(mop);
  return 0;
}

/*
******** nrrdRead()
**
** now just a wrapper around nRead(); reads a NRRD from a FILE *
*/
int /* Biff: 1 */
nrrdRead(Nrrd *nrrd, FILE *file, NrrdIoState *_nio) {
  static const char me[] = "nrrdRead";

  if (nRead(nrrd, file, NULL, _nio)) {
    biffAddf(NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

/*
******** nrrdStringRead()
**
** also a wrapper around nRead(); reads a NRRD from a char *.
**
** Because the same underlying nRead() is used, the same semantics
** about using existing nrrd->data when possible applies, as does the
** action of nrrdStateGrayscaleImage3D
*/
int /* Biff: 1 */
nrrdStringRead(Nrrd *nrrd, const char *string, NrrdIoState *_nio) {
  static const char me[] = "nrrdStringRead";

  if (nRead(nrrd, NULL, string, _nio)) {
    biffAddf(NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

/*
** nrrd__SplitName()
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
nrrd__SplitName(char **dirP, char **baseP, const char *name) {
  const char *where;

  if (dirP) {
    *dirP = (char *)airFree(*dirP);
  }
  if (baseP) {
    *baseP = (char *)airFree(*baseP);
  }
  where = strrchr(name, '/');
#ifdef _WIN32
  /* Deal with Windows "\" path separators; thanks to Torsten Rohlfing */
  if (!where || (strrchr(name, '\\') > where)) {
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
**    | read.c/nrrd__SplitName
**    | read.c/nrrdRead
**       | nio->format->read
**       = formatNRRD.c/nrrd__FormatNRRD_read:
**          | read.c/nrrdOneLine
**          | parseNrrd.c/nrrd__ReadNrrdParseField
**          | parseNrrd.c/nrrdFieldInfoParse[]
**          = parseNrrd.c/nrrd__ReadNrrdParse_data_file
**             | fopen(dataName)
**          | formatNRRD.c/nrrd__HeaderCheck
**          | read.c/nrrdLineSkip
**          | read.c/nrrdByteSkip
**          | nio->encoding->read
**          = encodingRaw.c/nrrd__EncodingRaw_read
**             | read.c/nrrd__Calloc
**          | formatNRRD.c/nrrdSwapEndian
**          | miscAir.c/airFclose
**
** (more documentation here)
**
** sneakiness: returns 2 if the reason for problem was a failed fopen().
** BUT: this return 2 is NOT the same as the 2 returned from
** nrrdHestNrrd->parse() or nrrdHestNrrdNoTTY->parse() or nrrdHestIter->parse()
** as a way to initiate quiet quitting for hestParseOrExit
*/
int /* Biff: 1|2 */
nrrdLoad(Nrrd *nrrd, const char *filename, NrrdIoState *nio) {
  static const char me[] = "nrrdLoad";
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
    airMopAdd(mop, nio, nrrdIoStateNix_mop, airMopAlways);
  }

  /* we save the directory of the filename given to us so that if it turns
     out that this is a detached header with a header-relative data file,
     then we will know how to find the data file */
  nrrd__SplitName(&(nio->path), NULL, filename);
  /* printf("!%s: |%s|%s|\n", me, nio->dir, nio->base); */

  if (!(file = airFopen(filename, stdin, "rb"))) {
    biffAddf(NRRD, "%s: fopen(\"%s\",\"rb\") failed: %s", me, filename, strerror(errno));
    airMopError(mop);
    return 2;
  }
  airMopAdd(mop, file, airFclose_mop, airMopOnError);
  /* non-error exiting is handled below */

  if (nrrdRead(nrrd, file, nio)) {
    biffAddf(NRRD, "%s: trouble reading \"%s\"", me, filename);
    airMopError(mop);
    return 1;
  }

  if (nrrdFormatNRRD == nio->format && nio->keepNrrdDataFileOpen
      && file == nio->dataFile) {
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

int /* Biff: 1 */
nrrdLoadMulti(Nrrd *const *nin, unsigned int ninLen, const char *fnameFormat,
              unsigned int numStart, NrrdIoState *nio) {
  static const char me[] = "nrrdLoadMulti";
  char *fname;
  size_t fnameSize;
  airArray *mop;
  unsigned int nii;

  if (!(nin && fnameFormat)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (!(nrrdContainsPercentThisAndMore(fnameFormat, 'u'))) {
    biffAddf(NRRD,
             "%s: given format \"%s\" doesn't seem to "
             "have the \"%%u\" conversion specification to s(n)printf "
             "an unsigned int\n",
             me, fnameFormat);
    return 1;
  }

  mop = airMopNew();
  /* should be big enough for the number replacing the format sequence */
  fnameSize = strlen(fnameFormat) + 128 + 1;
  fname = AIR_MALLOC(fnameSize, char);
  if (!fname) {
    biffAddf(NRRD, "%s: couldn't allocate local fname buffer", me);
    airMopError(mop);
    return 1;
  }
  airMopAdd(mop, fname, airFree, airMopAlways);

  for (nii = 0; nii < ninLen; nii++) {
    unsigned int num;
    num = numStart + nii;
    snprintf(fname, fnameSize, fnameFormat, num);
    if (nrrdLoad(nin[nii], fname, nio)) {
      biffAddf(NRRD, "%s: trouble loading nin[%u] from %s", me, nii, fname);
      airMopError(mop);
      return 1;
    }
    /* HEY: GLK hopes that the nio doesn't have any state that needs
       resetting, but we can't call nrrdIoStateInit() because that
       would negate the purpose of sending in the nio for all but the
       first saved nrrd */
  }

  airMopOkay(mop);
  return 0;
}
