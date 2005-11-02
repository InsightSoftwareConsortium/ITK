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

  Program:   Insight Segmentation & Registration Toolkit
  Module:    \MakeConsistentWrappedClasses.sh{WRAP_OBJECT2_TARGET}.cxx,v \$
  Language:  C++
  Date:      \$Date: 2005-11-02 21:45:13 $
  Version:   \$Revision: 1.2 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImage.h"
#include "itk${WRAP_OBJECT2_TARGET}.h"

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
    ITK_WRAP_OBJECT2(${WRAP_OBJECT2_TARGET}, image::UC2, image::UC2, itk${WRAP_OBJECT2_TARGET}UC2UC2);
    ITK_WRAP_OBJECT2(${WRAP_OBJECT2_TARGET}, image::US2, image::US2, itk${WRAP_OBJECT2_TARGET}US2US2);
    ITK_WRAP_OBJECT2(${WRAP_OBJECT2_TARGET}, image::UI2, image::UI2, itk${WRAP_OBJECT2_TARGET}UI2UI2);
    ITK_WRAP_OBJECT2(${WRAP_OBJECT2_TARGET}, image::SC2, image::SC2, itk${WRAP_OBJECT2_TARGET}SC2SC2);
    ITK_WRAP_OBJECT2(${WRAP_OBJECT2_TARGET}, image::SS2, image::SS2, itk${WRAP_OBJECT2_TARGET}SS2SS2);
    ITK_WRAP_OBJECT2(${WRAP_OBJECT2_TARGET}, image::SI2, image::SI2, itk${WRAP_OBJECT2_TARGET}SI2SI2);

    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT2(${WRAP_OBJECT2_TARGET}, image::F3 , image::F3 , itk${WRAP_OBJECT2_TARGET}F3F3  );
    ITK_WRAP_OBJECT2(${WRAP_OBJECT2_TARGET}, image::D3 , image::D3 , itk${WRAP_OBJECT2_TARGET}D3D3  );
    ITK_WRAP_OBJECT2(${WRAP_OBJECT2_TARGET}, image::UC3, image::UC3, itk${WRAP_OBJECT2_TARGET}UC3UC3);
    ITK_WRAP_OBJECT2(${WRAP_OBJECT2_TARGET}, image::US3, image::US3, itk${WRAP_OBJECT2_TARGET}US3US3);
    ITK_WRAP_OBJECT2(${WRAP_OBJECT2_TARGET}, image::UI3, image::UI3, itk${WRAP_OBJECT2_TARGET}UI3UI3);
    ITK_WRAP_OBJECT2(${WRAP_OBJECT2_TARGET}, image::SC3, image::SC3, itk${WRAP_OBJECT2_TARGET}SC3SC3);
    ITK_WRAP_OBJECT2(${WRAP_OBJECT2_TARGET}, image::SS3, image::SS3, itk${WRAP_OBJECT2_TARGET}SS3SS3);
    ITK_WRAP_OBJECT2(${WRAP_OBJECT2_TARGET}, image::SI3, image::SI3, itk${WRAP_OBJECT2_TARGET}SI3SI3);
  }
}
#endif
FILE_EOF

done

for WRAP_OBJECT2_TARGET in ${MAKE_ONLY_FLOAT_TYPES}; do
CURRFILE=${Prefix}${WRAP_OBJECT2_TARGET}${Postfix}
echo "Building ${CURRFILE}"
cat > ${CURRFILE} << FILE_EOF
/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    \MakeConsistentWrappedClasses.sh{WRAP_OBJECT2_TARGET}.cxx,v \$
  Language:  C++
  Date:      \$Date: 2005-11-02 21:45:13 $
  Version:   \$Revision: 1.2 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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


