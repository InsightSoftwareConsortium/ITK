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
#include "itkRescaleIntensityImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkRescaleIntensityImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(RescaleIntensityImageFilter, image::F2, image::US2,
                                     itkRescaleIntensityImageFilterF2US2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(RescaleIntensityImageFilter, image::US2, image::F2,
                                     itkRescaleIntensityImageFilterUS2F2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(RescaleIntensityImageFilter, image::F2, image::F2,
                                     itkRescaleIntensityImageFilterF2F2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(RescaleIntensityImageFilter, image::F2, image::US2,
                                     itkRescaleIntensityImageFilterF2US2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(RescaleIntensityImageFilter, image::F2, image::UC2,
                                     itkRescaleIntensityImageFilterF2UC2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(RescaleIntensityImageFilter, image::US2, image::UC2,
                                     itkRescaleIntensityImageFilterUS2UC2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(RescaleIntensityImageFilter, image::US2, image::US2,
                                     itkRescaleIntensityImageFilterUS2US2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(RescaleIntensityImageFilter, image::F3, image::US3,
                                     itkRescaleIntensityImageFilterF3US3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(RescaleIntensityImageFilter, image::F3, image::F3,
                                     itkRescaleIntensityImageFilterF3F3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(RescaleIntensityImageFilter, image::US3, image::US3,
                                     itkRescaleIntensityImageFilterUS3US3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(RescaleIntensityImageFilter, image::US3, image::F3,
                                     itkRescaleIntensityImageFilterUS3F3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(RescaleIntensityImageFilter, image::F3, image::UC3,
                                     itkRescaleIntensityImageFilterF3UC3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(RescaleIntensityImageFilter, image::US3, image::UC3,
                                     itkRescaleIntensityImageFilterUS3UC3);
  }
}


#endif
