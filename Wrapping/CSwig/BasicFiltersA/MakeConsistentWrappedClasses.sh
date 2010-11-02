#!/bin/bash
#This is a utilty script that is useful for generating a consistent set
#of wrapping definitions.  In an attempt to make the interpreted environment
#match the compiled environment as much as possible, it will be important
#to keep all the wrapped functionallity consistent across filters.
#

Prefix=wrap_itk
Postfix=.cxx


MAKE_ALL_FILTER_TYPES="MedianImageFilter NeighborhoodConnectedImageFilter IsolatedConnectedImageFilter GradientMagnitudeImageFilter FastMarchingImageFilter RegionOfInterestImageFilter"

MAKE_ONLY_FLOAT_TYPES=""

for WRAP_OBJECT2_TARGET in ${MAKE_ALL_FILTER_TYPES}; do
CURRFILE=${Prefix}${WRAP_OBJECT2_TARGET}${Postfix}
DATESTAMP=`date '+%F-%R'`
mv ${CURRFILE} ${CURRFILE}.${DATESTAMP}
echo "Building ${CURRFILE}"
cat > ${CURRFILE} << FILE_EOF
/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include "itk${WRAP_OBJECT2_TARGET}.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

//=================================
//THIS FILE GENERATED WITH $0
//=================================

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itk${WRAP_OBJECT2_TARGET});
  namespace wrappers
  {
    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT2(${WRAP_OBJECT2_TARGET}, image::F2 , image::F2 , itk${WRAP_OBJECT2_TARGET}F2F2  );
    ITK_WRAP_OBJECT2(${WRAP_OBJECT2_TARGET}, image::D2 , image::D2 , itk${WRAP_OBJECT2_TARGET}D2D2  );

    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT2(${WRAP_OBJECT2_TARGET}, image::F3 , image::F3 , itk${WRAP_OBJECT2_TARGET}F3F3  );
    ITK_WRAP_OBJECT2(${WRAP_OBJECT2_TARGET}, image::D3 , image::D3 , itk${WRAP_OBJECT2_TARGET}D3D3  );
  }
}
#endif
FILE_EOF

done


