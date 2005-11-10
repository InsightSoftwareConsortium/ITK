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

#if TEEM_BZIP2
#include <bzlib.h>
#endif

#include "teem32bit.h"

char _nrrdRelativePathFlag[] = "./";
char _nrrdFieldSep[] = " \t";
char _nrrdNoSpaceVector[] = "none";
char _nrrdTextSep[] = " ,\t";

typedef union {
  char ***c;
  void **v;
} _cppu;

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
_nrrdOneLine (unsigned int *lenP, NrrdIoState *nio, FILE *file) {
  char me[]="_nrrdOneLine", err[AIR_STRLEN_MED], **line;
  airArray *mop, *lineArr;
  int lineIdx;
  _cppu u;
  unsigned int len;

  if (!( lenP && nio && file)) {
    sprintf(err, "%s: got NULL pointer (%p, %p, %p)", me,
            lenP, nio, file);
    biffAdd(NRRD, err); return 1;
  }
  if (0 == nio->lineLen) {
    /* nio->line hasn't been allocated for anything */
    nio->line = (char*)calloc(3, sizeof(char));
    nio->lineLen = 3;
  }
  len = airOneLine(file, nio->line, nio->lineLen);
  if (len <= nio->lineLen) {
    /* otherwise we hit EOF before a newline, or the line (possibly empty)
       fit within the nio->line, neither of which is an error here */
    *lenP = len;
  } else {
    /* line didn't fit in buffer, so we have to increase line
       buffer size and put the line together in pieces */
    u.c = &line;
    lineArr = airArrayNew(u.v, NULL, sizeof(char *), 1);
    if (!lineArr) {
      sprintf(err, "%s: couldn't allocate airArray", me);
      biffAdd(NRRD, err); *lenP = 0; return 1;
    }
    airArrayPointerCB(lineArr, airNull, airFree);
    mop = airMopNew();
    airMopAdd(mop, lineArr, (airMopper)airArrayNuke, airMopAlways);
    while (len == nio->lineLen+1) {
      lineIdx = airArrayLenIncr(lineArr, 1);
      if (-1 == lineIdx) {
        sprintf(err, "%s: couldn't increment line buffer array", me);
        biffAdd(NRRD, err); *lenP = 0; airMopError(mop); return 1;
      }
      line[lineIdx] = nio->line;
      nio->lineLen *= 2;
      nio->line = (char*)malloc(nio->lineLen);
      if (!nio->line) {
        sprintf(err, "%s: couldn't alloc %d-char line\n", me, nio->lineLen);
        biffAdd(NRRD, err); *lenP = 0; airMopError(mop); return 1;
      }
      len = airOneLine(file, nio->line, nio->lineLen);
    }
    /* last part did fit in nio->line buffer, also save this into line[] */
    lineIdx = airArrayLenIncr(lineArr, 1);
    if (!lineArr->data) {
      sprintf(err, "%s: couldn't increment line buffer array", me);
      biffAdd(NRRD, err); *lenP = 0; airMopError(mop); return 1;
    }
    line[lineIdx] = nio->line;
    nio->lineLen *= 3;  /* for good measure */
    nio->line = (char*)malloc(nio->lineLen);
    if (!nio->line) {
      sprintf(err, "%s: couldn't alloc %d-char line\n", me, nio->lineLen);
      biffAdd(NRRD, err); *lenP = 0; airMopError(mop); return 1;
    }
    /* now concatenate everything into a new nio->line */
    strcpy(nio->line, "");
    for (lineIdx=0; lineIdx<(int)lineArr->len; lineIdx++) {
      strcat(nio->line, line[lineIdx]);
    }
    *lenP = strlen(nio->line) + 1;
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
_nrrdCalloc (Nrrd *nrrd, NrrdIoState *nio, FILE *file) {
  char me[]="_nrrdCalloc", err[AIR_STRLEN_MED];
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
      sprintf(err, "%s: couldn't allocate " _AIR_SIZE_T_CNV 
              " things of size " _AIR_SIZE_T_CNV,
              me, nrrdElementNumber(nrrd), nrrdElementSize(nrrd));
      biffAdd(NRRD, err); return 1;
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
nrrdLineSkip (FILE *dataFile, NrrdIoState *nio) {
  unsigned int lsi, skipRet;
  char me[]="nrrdLineSkip", err[AIR_STRLEN_MED];

  /* For compressed data: If you don't actually have ascii headers on
     top of your gzipped data then you will potentially huge lines
     while _nrrdOneLine looks for line terminations.  Quoting Gordon:
     "Garbage in, Garbage out." */
  
  if (!( dataFile && nio )) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }

  for (lsi=0; lsi<nio->lineSkip; lsi++) {
    if (_nrrdOneLine(&skipRet, nio, dataFile)) {
      sprintf(err, "%s: error skipping line %u of %u",
              me, lsi+1, nio->lineSkip);
      biffAdd(NRRD, err); return 1;
    }
    if (!skipRet) {
      sprintf(err, "%s: hit EOF skipping line %u of %u",
              me, lsi+1, nio->lineSkip);
      biffAdd(NRRD, err); return 1;
    }
  }
  return 0;
}

/*
******** nrrdByteSkip
**
** public for the sake of things like "unu make"
** uses nio for information about how much data should actually be skipped
** with -1 == byteSkip
*/
int
nrrdByteSkip (FILE *dataFile, Nrrd *nrrd, NrrdIoState *nio) {
  int i, skipRet;
  char me[]="nrrdByteSkip", err[AIR_STRLEN_MED];
  size_t bsize;

  if (!( dataFile && nrrd && nio )) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  if (nio->byteSkip < -1) {
    sprintf(err, "%s: byteSkip %d not valid", me, nio->byteSkip);
    biffAdd(NRRD, err); return 1;
  }
  if (-1 == nio->byteSkip) {
    if (nrrdEncodingRaw != nio->encoding) {
      sprintf(err, "%s: can do backwards byte skip only in %s "
              "encoding, not %s", me,
              nrrdEncodingRaw->name, nio->encoding->name);
      biffAdd(NRRD, err); return 1;
    }
    if (stdin == dataFile) {
      sprintf(err, "%s: can't fseek on stdin", me);
      biffAdd(NRRD, err); return 1;
    }
    bsize = nrrdElementNumber(nrrd)/_nrrdDataFNNumber(nio);
    bsize *= nrrdElementSize(nrrd);
    if (fseek(dataFile, -((long)bsize), SEEK_END)) {
      sprintf(err, "%s: failed to fseek(dataFile, " _AIR_SIZE_T_CNV
              ", SEEK_END)", me, bsize);
      biffAdd(NRRD, err); return 1;      
    }
    if (nrrdStateVerboseIO >= 2) {
      fprintf(stderr, "(%s: actually skipped %d bytes)\n",
              me, (int)ftell(dataFile));
    }
  } else {
    for (i=1; i<=nio->byteSkip; i++) {
      skipRet = fgetc(dataFile);
      if (EOF == skipRet) {
        sprintf(err, "%s: hit EOF skipping byte %d of %d",
                me, i, nio->byteSkip);
        biffAdd(NRRD, err); return 1;
      }
    }
  }
  return 0;
}

/*
******** nrrdRead()
**
** read in nrrd from a given file.  The main job of this function is to
** start reading the file, to determine the format, and then call the
** appropriate format's reader.  This means that the various encoding
** (data) readers can assume that nio->format is usefully set.
**
** The only input information that nio is used for is nio->path, so that
** detached header-relative data files can be found.  
**
*/
int
nrrdRead (Nrrd *nrrd, FILE *file, NrrdIoState *nio) {
  char me[]="nrrdRead", err[AIR_STRLEN_MED];
  unsigned int llen;
  int nfi;
  airArray *mop; 

  /* sanity check, for good measure */
  if (!nrrdSanity()) {
    sprintf(err, "%s: sanity check FAILED: have to fix and re-compile", me);
    biffAdd(NRRD, err); return 1;
  }

  if (!(file && nrrd)) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  mop = airMopNew();
  if (!nio) {
    nio = nrrdIoStateNew();
    if (!nio) {
      sprintf(err, "%s: couldn't alloc I/O struct", me);
      biffAdd(NRRD, err); return 1;
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

  if (_nrrdOneLine(&llen, nio, file)) {
    sprintf(err, "%s: error getting first line (containing \"magic\")", me);
    biffAdd(NRRD, err); airMopError(mop); return 1;
  }
  if (!llen) {
     sprintf(err, "%s: immediately hit EOF", me);
    biffAdd(NRRD, err); airMopError(mop); return 1;
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
    sprintf(err, "%s: couldn't parse \"%s\" as magic or beginning of "
            "any recognized format", me, nio->line);
    biffAdd(NRRD, err); airMopError(mop); return 1;
  }
  
  /* try to read the file */
  if (nio->format->read(file, nrrd, nio)) {
    sprintf(err, "%s: trouble reading %s file", me, nio->format->name);
    biffAdd(NRRD, err); airMopError(mop); return 1;
  }

  /* reshape up grayscale images, if desired */
  if (nio->format->isImage && 2 == nrrd->dim && nrrdStateGrayscaleImage3D) {
    if (nrrdAxesInsert(nrrd, nrrd, 0)) {
      sprintf(err, "%s:", me);
      biffAdd(NRRD, err); return 1;
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
    sprintf(err, "%s: problem with nrrd after reading", me);
    biffAdd(NRRD, err); return 1;
  }

  airMopOkay(mop);
  return 0;
}

/*
** _nrrdSplitName()
**
** splits a file name into a path and a base filename.  The directory
** seperator is assumed to be '/'.  The division between the path
** and the base is the last '/' in the file name.  The path is
** everything prior to this, and base is everything after (so the
** base does NOT start with '/').  If there is not a '/' in the name,
** or if a '/' appears as the last character, then the path is set to
** ".", and the name is copied into base.
*/
void
_nrrdSplitName (char **dirP, char **baseP, const char *name) {
  char *where;
  
  if (dirP) {
    *dirP = (char *)airFree(*dirP);
  }
  if (baseP) {
    *baseP = (char *)airFree(*baseP);
  }
  where = strrchr(name, '/');
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

1) its in the same file.  ElementDataFile is "LOCAL"

2) its in a list of files. ElementDataFile is "LIST", and what follows
in the header is a list of files, one filename per line.  By default,
there is one slice per sample on the slowest axis, but you can do
otherwise with, for example, "LIST 3", which means that there will be
a 3D slab per file.

3) slices in numbered files. ElementDataFile is, for example,
"file%03d.blah <min> <max> <step>", where the first part is a
printf-style string containing a format sequence for an integer value,
and <min>, <max>, and <step> are integer values that specify the min,
max, and increment value for naming the numbered slices.  Note that if
you use something like "file%d.blah", you automatically get the
correct ordering between "file2.blah" and "file10.blah".

I plan on shamelessly copying this, just like I shamelessly copied the
"byte skip: -1" feature from MetaIO.  The minor differences are:

- the datafile is "LOCAL" by default, as in, no "data file: " field is
  given in the NRRD.  This is the current behavior for attached
  headers.

- When using the pattern for numbered files, the final <step> value
  will be optional, and by default 1.

- This will work for multiple compressed files.


**
** (more documentation here)
**
** sneakiness: returns 2 if the reason for problem was a failed fopen().
** 
*/
int
nrrdLoad (Nrrd *nrrd, const char *filename, NrrdIoState *nio) {
  char me[]="nrrdLoad", err[AIR_STRLEN_MED];
  FILE *file;
  airArray *mop;

  if (!(nrrd && filename)) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  mop = airMopNew();
  if (!nio) {
    nio = nrrdIoStateNew();
    if (!nio) {
      sprintf(err, "%s: couldn't alloc I/O struct", me);
      biffAdd(NRRD, err); return 1;
    }
    airMopAdd(mop, nio, (airMopper)nrrdIoStateNix, airMopAlways);
  }
  
  /* we save the directory of the filename given to us so that if it turns
     out that this is a detached header with a header-relative data file,
     then we will know how to find the data file */
  _nrrdSplitName(&(nio->path), NULL, filename);
  /* printf("!%s: |%s|%s|\n", me, nio->dir, nio->base); */

  if (!( file = airFopen(filename, stdin, "rb") )) {
    sprintf(err, "%s: fopen(\"%s\",\"rb\") failed: %s", 
            me, filename, strerror(errno));
    biffAdd(NRRD, err); airMopError(mop); return 2;
  }
  airMopAdd(mop, file, (airMopper)airFclose, airMopOnError);
  /* non-error exiting is handled below */

  if (nrrdRead(nrrd, file, nio)) {
    sprintf(err, "%s: trouble reading \"%s\"", me, filename);
    biffAdd(NRRD, err); airMopError(mop); return 1;
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
