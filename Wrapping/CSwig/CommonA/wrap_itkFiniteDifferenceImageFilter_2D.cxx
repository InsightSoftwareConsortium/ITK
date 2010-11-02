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
#include "itkImage.h"
#include "itkFiniteDifferenceImageFilter.h"
#include "itkVector.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkFiniteDifferenceImageFilter_2D);
  namespace wrappers
  {
    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::F2 , image::F2 , itkFiniteDifferenceImageFilterF2F2  );
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::D2 , image::D2 , itkFiniteDifferenceImageFilterD2D2  );
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::UC2, image::F2, itkFiniteDifferenceImageFilterUC2F2);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::US2, image::F2, itkFiniteDifferenceImageFilterUS2F2);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::UI2, image::F2, itkFiniteDifferenceImageFilterUI2F2);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::SC2, image::F2, itkFiniteDifferenceImageFilterSC2F2);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::SS2, image::F2, itkFiniteDifferenceImageFilterSS2F2);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::SI2, image::F2, itkFiniteDifferenceImageFilterSI2F2);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::F2 , image::VF2 ,itkFiniteDifferenceImageFilterF2VF2 );
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::VF2 , image::VF2 ,itkFiniteDifferenceImageFilterVF2VF2 );
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::US2, image::VF2, itkFiniteDifferenceImageFilterUS2VF2);
  }
}
#endif
