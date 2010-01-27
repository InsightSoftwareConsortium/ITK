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
unsigned int nrrdDefaultWriteCharsPerLine = 75;
unsigned int nrrdDefaultWriteValsPerLine = 8;
/* ---- BEGIN non-NrrdIO */
int nrrdDefaultResampleBoundary = nrrdBoundaryBleed;
int nrrdDefaultResampleType = nrrdTypeDefault;
int nrrdDefaultResampleRenormalize = AIR_TRUE;
int nrrdDefaultResampleRound = AIR_TRUE;
int nrrdDefaultResampleClamp = AIR_TRUE;
int nrrdDefaultResampleCheap = AIR_FALSE;
double nrrdDefaultResamplePadValue = 0.0;
double nrrdDefaultKernelParm0 = 1.0; 
/* ---- END non-NrrdIO */
int nrrdDefaultCenter = nrrdCenterCell;
double nrrdDefaultSpacing = 1.0;

/* these aren't really "defaults" because there's no other channel for
   specifying this information.  It is just global state.  Obviously,
   like defaults, they are not thread-safe if different threads ever
   set them differently. */
int nrrdStateVerboseIO = 1; /* NrrdIO-hack-003 */
int nrrdStateKeyValuePairsPropagate = AIR_FALSE;
/* ---- BEGIN non-NrrdIO */
int nrrdStateBlind8BitRange = AIR_TRUE;
int nrrdStateMeasureType = nrrdTypeFloat;
int nrrdStateMeasureModeBins = 1024;
int nrrdStateMeasureHistoType = nrrdTypeFloat;
int nrrdStateDisallowIntegerNonExist = AIR_TRUE;
/* ---- END non-NrrdIO */
int nrrdStateAlwaysSetContent = AIR_TRUE;
int nrrdStateDisableContent = AIR_FALSE;
char *nrrdStateUnknownContent = NRRD_UNKNOWN;
int nrrdStateGrayscaleImage3D = AIR_FALSE;
/* there is no sane reason to change this initialization */
int nrrdStateKeyValueReturnInternalPointers = AIR_FALSE;
/* Making the default for this be AIR_TRUE means that nrrd is not only
   completely conservative about updating kind, but purposely stupid.
   Nrrd is only going to implement the most converative kind of logic
   anyway, based on existing sementics nailed down by the format spec. */
int nrrdStateKindNoop = AIR_FALSE;

/* should the acceptance (or not) of malformed NRRD header fields 
   embedded in PNM or text comments be controlled here? */

/* Are there other assumptions currently built into nrrd which could
   stand to be user-controllable? */

/* ---- BEGIN non-NrrdIO */

const char *const nrrdEnvVarDefaultWriteEncodingType
  = "NRRD_DEFAULT_WRITE_ENCODING_TYPE";
const char *const nrrdEnvVarDefaultWriteBareText
  = "NRRD_DEFAULT_WRITE_BARE_TEXT";
const char *const nrrdEnvVarDefaultWriteBareTextOld 
  = "NRRD_DEF_WRITE_BARE_TEXT";
const char *const nrrdEnvVarDefaultCenter
  = "NRRD_DEFAULT_CENTER";
const char *const nrrdEnvVarDefaultCenterOld
  = "NRRD_DEF_CENTER";
const char *const nrrdEnvVarDefaultWriteCharsPerLine
  = "NRRD_DEFAULT_WRITE_CHARS_PER_LINE";
const char *const nrrdEnvVarDefaultWriteValsPerLine
  = "NRRD_DEFAULT_WRITE_VALS_PER_LINE";
const char *const nrrdEnvVarDefaultKernelParm0
  = "NRRD_DEFAULT_KERNEL_PARM0";
const char *const nrrdEnvVarDefaultSpacing
  = "NRRD_DEFAULT_SPACING";

const char *const nrrdEnvVarStateKindNoop
  = "NRRD_STATE_KIND_NOOP";
const char *const nrrdEnvVarStateVerboseIO
  = "NRRD_STATE_VERBOSE_IO";
const char *const nrrdEnvVarStateKeyValuePairsPropagate 
  = "NRRD_STATE_KEYVALUEPAIRS_PROPAGATE";
const char *const nrrdEnvVarStateBlind8BitRange
  = "NRRD_STATE_BLIND_8_BIT_RANGE";
const char *const nrrdEnvVarStateAlwaysSetContent
  = "NRRD_STATE_ALWAYS_SET_CONTENT";
const char *const nrrdEnvVarStateDisableContent
  = "NRRD_STATE_DISABLE_CONTENT";
const char *const nrrdEnvVarStateMeasureType
  = "NRRD_STATE_MEASURE_TYPE";
const char *const nrrdEnvVarStateMeasureModeBins
  = "NRRD_STATE_MEASURE_MODE_BINS";
const char *const nrrdEnvVarStateMeasureHistoType
  = "NRRD_STATE_MEASURE_HISTO_TYPE";
const char *const nrrdEnvVarStateGrayscaleImage3D
  = "NRRD_STATE_GRAYSCALE_IMAGE_3D";

/*
**        -1: unset, or bad args    ==> *val NOT set
**  AIR_TRUE: set in a valid way    ==> *val set (to something)
** AIR_FALSE: set in an invalid way ==> *val NOT set
*/

int
nrrdGetenvBool(int *val, char **envStr, const char *envVar) {
  char *env;
  int tmp;

  if (!(val && envVar)) {
    return -1;
  }
  env = getenv(envVar);
  if (envStr) {
    *envStr = env;
  }
  if (!env) {
    return -1;
  }
  if (!strlen(env)) {
    /* for bools, being merely set (but not to any string) means "true" */
    *val = AIR_TRUE;
    return AIR_TRUE;
  }
  tmp = airEnumVal(airBool, env);
  if (airEnumUnknown(airBool) == tmp) {
    return AIR_FALSE;
  } else {
    *val = tmp;
    return AIR_TRUE;
  }
}

int
nrrdGetenvEnum(int *val, char **envStr, const airEnum *enm,
               const char *envVar) {
  char *env;
  int tmp;

  if (!(val && envVar)) {
    return -1;
  }
  env = getenv(envVar);
  if (envStr) {
    *envStr = env;
  }
  if (!env) {
    return -1;
  }
  tmp = airEnumVal(enm, env);
  if (airEnumUnknown(enm) == tmp) {
    return AIR_FALSE;
  } else {
    *val = tmp;
    return AIR_TRUE;
  }
}

int
nrrdGetenvUInt(unsigned int *val, char **envStr, const char *envVar) {
  char *env;
  unsigned int tmp;

  if (!(val && envVar)) {
    return -1;
  }
  env = getenv(envVar);
  if (envStr) {
    *envStr = env;
  }
  if (!env) {
    return -1;
  }
  if (1 != sscanf(env, "%u", &tmp)) {
    return AIR_FALSE;
  } else {
    *val = tmp;
    return AIR_TRUE;
  }
}

int
nrrdGetenvInt(int *val, char **envStr, const char *envVar) {
  char *env;
  int tmp;

  if (!(val && envVar)) {
    return -1;
  }
  env = getenv(envVar);
  if (envStr) {
    *envStr = env;
  }
  if (!env) {
    return -1;
  }
  if (1 != sscanf(env, "%d", &tmp)) {
    return AIR_FALSE;
  } else {
    *val = tmp;
    return AIR_TRUE;
  }
}

int
nrrdGetenvDouble(double *val, char **envStr, const char *envVar) {
  char *env;
  double tmp;

  if (!(val && envVar)) {
    return -1;
  }
  env = getenv(envVar);
  if (envStr) {
    *envStr = env;
  }
  if (!env) {
    return -1;
  }
  if (1 != sscanf(env, "%lf", &tmp)) {
    return AIR_FALSE;
  } else {
    *val = tmp;
    return AIR_TRUE;
  }
}

void
nrrdDefaultGetenv(void) {
  
  /* these two pre-date Def --> Default rename */
  if (-1 == nrrdGetenvBool(/**/ &nrrdDefaultWriteBareText, NULL,
                           nrrdEnvVarDefaultWriteBareTextOld)) {
    nrrdGetenvBool(/**/ &nrrdDefaultWriteBareText, NULL,
                   nrrdEnvVarDefaultWriteBareText);
  }
  if (-1 == nrrdGetenvEnum(/**/ &nrrdDefaultCenter, NULL, nrrdCenter,
                           nrrdEnvVarDefaultCenterOld)) {
    nrrdGetenvEnum(/**/ &nrrdDefaultCenter, NULL, nrrdCenter,
                   nrrdEnvVarDefaultCenter);
  }
  /* these post-date the Def --> Default rename */

  nrrdGetenvEnum(/**/ &nrrdDefaultWriteEncodingType, NULL, nrrdEncodingType,
                 nrrdEnvVarDefaultWriteEncodingType);
  nrrdGetenvUInt(/**/ &nrrdDefaultWriteCharsPerLine, NULL,
                 nrrdEnvVarDefaultWriteCharsPerLine);
  nrrdGetenvUInt(/**/ &nrrdDefaultWriteValsPerLine, NULL,
                 nrrdEnvVarDefaultWriteValsPerLine);
  nrrdGetenvDouble(/**/ &nrrdDefaultKernelParm0, NULL,
                   nrrdEnvVarDefaultKernelParm0);
  nrrdGetenvDouble(/**/ &nrrdDefaultSpacing, NULL,
                   nrrdEnvVarDefaultSpacing);

  return;
}

void
nrrdStateGetenv(void) {

  nrrdGetenvBool(/**/ &nrrdStateKindNoop, NULL,
                 nrrdEnvVarStateKindNoop);
  nrrdGetenvInt(/**/ &nrrdStateVerboseIO, NULL,
                nrrdEnvVarStateVerboseIO);
  nrrdGetenvBool(/**/ &nrrdStateKeyValuePairsPropagate, NULL,
                 nrrdEnvVarStateKeyValuePairsPropagate);
  nrrdGetenvBool(/**/ &nrrdStateBlind8BitRange, NULL,
                 nrrdEnvVarStateBlind8BitRange);
  nrrdGetenvBool(/**/ &nrrdStateAlwaysSetContent, NULL,
                 nrrdEnvVarStateAlwaysSetContent);
  nrrdGetenvBool(/**/ &nrrdStateDisableContent, NULL,
                 nrrdEnvVarStateDisableContent);
  nrrdGetenvEnum(/**/ &nrrdStateMeasureType, NULL, nrrdType,
                 nrrdEnvVarStateMeasureType);
  nrrdGetenvInt(/**/ &nrrdStateMeasureModeBins, NULL,
                nrrdEnvVarStateMeasureModeBins);
  nrrdGetenvEnum(/**/ &nrrdStateMeasureHistoType, NULL, nrrdType,
                 nrrdEnvVarStateMeasureHistoType);
  nrrdGetenvBool(/**/ &nrrdStateGrayscaleImage3D, NULL,
                 nrrdEnvVarStateGrayscaleImage3D);

  return;
}

/* ---- END non-NrrdIO */
