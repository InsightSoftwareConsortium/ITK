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

#include "teem32bit.h"

/*
******** nrrdAxesInsert
**
** like reshape, but preserves axis information on old axes, and
** this is only for adding a "stub" axis with length 1.  All other
** axis attributes are initialized as usual.
*/
int
nrrdAxesInsert(Nrrd *nout, const Nrrd *nin, int ax) {
  char me[]="nrrdAxesInsert", func[]="axinsert", err[AIR_STRLEN_MED];
  int d;
  
  if (!(nout && nin)) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  if (!AIR_IN_CL(0, ax, nin->dim)) {
    sprintf(err, "%s: given axis (%d) outside valid range [0, %d]",
            me, ax, nin->dim);
    biffAdd(NRRD, err); return 1;
  }
  if (NRRD_DIM_MAX == nin->dim) {
    sprintf(err, "%s: given nrrd already at NRRD_DIM_MAX (%d)",
            me, NRRD_DIM_MAX);
    biffAdd(NRRD, err); return 1;
  }
  if (nout != nin) {
    if (nrrdCopy(nout, nin)) {
      sprintf(err, "%s:", me);
      biffAdd(NRRD, err); return 1;
    }
    /* HEY: comments have been copied, perhaps that's not appropriate */
  }
  nout->dim = 1 + nin->dim;
  for (d=nin->dim-1; d>=ax; d--) {
    _nrrdAxisInfoCopy(&(nout->axis[d+1]), &(nin->axis[d]),
                      NRRD_AXIS_INFO_NONE);
  }
  /* the ONLY thing we can say about the new axis is its size */
  _nrrdAxisInfoInit(&(nout->axis[ax]));
  if (!nrrdStateKindNoop) {
    /* except maybe the kind */
    nout->axis[ax].kind = nrrdKindStub;
  }
  nout->axis[ax].size = 1;
  if (nrrdContentSet(nout, func, nin, "%d", ax)) {
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); return 1;
  }
  nrrdPeripheralCopy(nout, nin);
  return 0;
}

