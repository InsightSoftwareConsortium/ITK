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
  const char* const group = ITK_WRAP_GROUP(itkFiniteDifferenceImageFilter_3D);
  namespace wrappers
  {
    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::F3 , image::F3 , itkFiniteDifferenceImageFilterF3F3  );
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::D3 , image::D3 , itkFiniteDifferenceImageFilterD3D3  );
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::UC3, image::F3, itkFiniteDifferenceImageFilterUC3F3);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::US3, image::F3, itkFiniteDifferenceImageFilterUS3F3);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::UI3, image::F3, itkFiniteDifferenceImageFilterUI3F3);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::SC3, image::F3, itkFiniteDifferenceImageFilterSC3F3);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::SS3, image::F3, itkFiniteDifferenceImageFilterSS3F3);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::SI3, image::F3, itkFiniteDifferenceImageFilterSI3F3);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::F3 , image::VF3 ,itkFiniteDifferenceImageFilterF3VF3);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::VF3 , image::VF3 ,itkFiniteDifferenceImageFilterVF3VF3);
    ITK_WRAP_OBJECT2(FiniteDifferenceImageFilter, image::US3, image::VF3, itkFiniteDifferenceImageFilterUS3VF3);
  }
}
#endif
