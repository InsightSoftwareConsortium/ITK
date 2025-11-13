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

/*
** these aren't "const"s because the user should be able to change
** default behavior- until a more sophisticated mechanism for this
** kind of control is developed, it seems simple and usable enough to
** have this be global state which we agree to treat nicely, as in,
** threads shouldn't be changing these willy-nilly.
**
** What IS a "default"?  A default is the assertion of a certain
** choice in situations where the user hasn't set it explicitly, but
** COULD.  The pad value in resampling is a good example: it is set by
** a constructor to nrrdDefaultResamplePadValue, but the user can also set it
** explicitly.
*/

int nrrdDefaultWriteEncodingType = nrrdEncodingTypeRaw;
int nrrdDefaultWriteBareText = AIR_TRUE;
int nrrdDefaultWriteMoreThanFloatInText = AIR_FALSE;
unsigned int nrrdDefaultWriteCharsPerLine = 75;
unsigned int nrrdDefaultWriteValsPerLine = 8;
int nrrdDefaultCenter = nrrdCenterCell;
/* matches how nrrdStateVerboseIO (gone in Teem2) was initialized */
int nrrdDefaultVerboseIO = 0;

/* these aren't really "defaults" because there's no other channel for
   specifying this information.  It is just global state.  Obviously,
   like defaults, they are not thread-safe if different threads ever
   set them differently. */
int nrrdStateKeyValuePairsPropagate = AIR_FALSE;
int nrrdStateAlwaysSetContent = AIR_TRUE;
int nrrdStateDisableContent = AIR_FALSE;
const char *nrrdStateUnknownContent = NRRD_UNKNOWN;
int nrrdStateGrayscaleImage3D = AIR_FALSE;
/* there is no sane reason to change this initialization */
int nrrdStateKeyValueReturnInternalPointers = AIR_FALSE;
/* Making the default for this be AIR_TRUE means that nrrd is not only
   completely conservative about updating kind, but purposely stupid.
   Nrrd is only going to implement the most converative kind of logic
   anyway, based on existing sementics nailed down by the format spec. */
int nrrdStateKindNoop = AIR_FALSE;

/* these are helper functions for min/max testing */
airLLong /* Biff: (private) nope */
_nrrdLLongMaxHelp(airLLong val) {
  return val * 2 + 1;
}
airLLong /* Biff: (private) nope */
_nrrdLLongMinHelp(airLLong val) {
  return val * 2;
}
airULLong /* Biff: (private) nope */
_nrrdULLongMaxHelp(airULLong val) {
  return val + 1;
}

/* should the acceptance (or not) of malformed NRRD header fields
   embedded in PNM or text comments be controlled here? */

/* Are there other assumptions currently built into nrrd which could
   stand to be user-controllable? */
