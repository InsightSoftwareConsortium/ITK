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

static int
encodingZRL_available(void) {

  return AIR_TRUE;
}

static int /* Biff: 1 */
encodingZRL_read(FILE *file, void *data, size_t elementNum, Nrrd *nrrd,
                 NrrdIoState *nio) {
  static const char me[] = "encodingZRL_read";
  /* assume output buffer has been initialized to all zeros */
  unsigned char *output_buffer = (unsigned char *)data;
  size_t toread = elementNum * nrrdElementSize(nrrd);
  unsigned int red = 0; /* how much has been read */
  AIR_UNUSED(nio);

  while (red < toread) {
    int cc = fgetc(file);
    if (cc == 0) {
      int dd = fgetc(file);
      if (dd == 0) {
        /* two zero bytes: next two bytes give (unsigned short) number of zero bytes */
        int hi, lo = fgetc(file);
        if (EOF == lo) {
          biffAddf(NRRD, "%s: after %u, hit EOF at low byte of zero count", me, red);
          return 1;
        }
        hi = fgetc(file);
        if (EOF == hi) {
          biffAddf(NRRD, "%s: after %u, hit EOF at high byte of zero count", me, red);
          return 1;
        }
        red += AIR_UINT(lo + hi * 256);
      } else {
        /* single zero byte followed by non-zero byte `dd`: `dd` zero bytes */
        red += (unsigned char)dd;
      }
    } else {
      /* non-zero bytes are copied one by one */
      output_buffer[red] = (unsigned char)cc;
      red++;
    }
  }

  return 0;
}

static int /* Biff: 0 */
encodingZRL_write(FILE *file, const void *data, size_t elementNum, const Nrrd *nrrd,
                  NrrdIoState *nio) {
  static const char me[] = "encodingZRL_write";

  AIR_UNUSED(file);
  AIR_UNUSED(data);
  AIR_UNUSED(elementNum);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  biffAddf(NRRD, "%s: sorry, currently a read-only encoding", me);

  return 0;
}

const NrrdEncoding nrrd__EncodingZRL
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
     encodingZRL_available,
     encodingZRL_read,
     encodingZRL_write};

const NrrdEncoding *const nrrdEncodingZRL = &nrrd__EncodingZRL;
