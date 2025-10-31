/*
  NrrdIO: C library for NRRD file IO (with optional compressions)
  Copyright (C) 2009--2025  University of Chicago
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

static int
_nrrdEncodingZRL_available(void) {

  return AIR_TRUE;
}

static int
_nrrdEncodingZRL_read(FILE *file, void *data, size_t elementNum, Nrrd *nrrd,
                      NrrdIoState *nio) {
  unsigned char *output_buffer = (unsigned char *)data;
  size_t toread = elementNum * nrrdElementSize(nrrd);
  int cc, dd;
  unsigned int j = 0;
  AIR_UNUSED(nio);

  while (j < toread) {
    cc = fgetc(file);
    if (cc == 0) {
      dd = fgetc(file);
      if (dd == 0) {
        dd = fgetc(file);
        j += dd + fgetc(file) * 256;
      } else {
        j += (unsigned char)dd;
      }
    } else {
      output_buffer[j] = (unsigned char)cc;
      j++;
    }
  }

  return 0;
}

static int /* Biff: 0 */
_nrrdEncodingZRL_write(FILE *file, const void *data, size_t elementNum, const Nrrd *nrrd,
                       NrrdIoState *nio) {
  static const char me[] = "_nrrdEncodingZRL_write";

  AIR_UNUSED(file);
  AIR_UNUSED(data);
  AIR_UNUSED(elementNum);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  biffAddf(NRRD, "%s: sorry, currently a read-only encoding", me);

  return 0;
}

const NrrdEncoding _nrrdEncodingZRL
  = {"zrl",     /* name */
     "zrl",     /* suffix */
     AIR_TRUE,  /* endianMatters */
     AIR_FALSE, /* isCompression: HEY this is a hack: this IS certainly a
                   compression. However, with compressed encodings the nrrd
                   format has no way of specifying whether a byteskip
                   between be outside the encoding (in the uncompressed
                   data) vs inside the encoding (within the compuressed
                   data).  To date the convention has been that byte skip is
                   done *inside* compressions, but for the ZRL-encoded data
                   as currently generated, the relevant byte skipping is
                   certainly *outside* the compression.  Thus we claim
                   ignorance about how ZRL is a compression, so that byte
                   skipping can be used. */
     _nrrdEncodingZRL_available,
     _nrrdEncodingZRL_read,
     _nrrdEncodingZRL_write};

const NrrdEncoding *const nrrdEncodingZRL = &_nrrdEncodingZRL;
