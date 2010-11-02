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
#include "itkPermuteAxesImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkPermuteAxesImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::F2, itkPermuteAxesImageFilterF2);
    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::F3, itkPermuteAxesImageFilterF3);

    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::D2, itkPermuteAxesImageFilterD2);
    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::D3, itkPermuteAxesImageFilterD3);

    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::UC2, itkPermuteAxesImageFilterUC2);
    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::UC3, itkPermuteAxesImageFilterUC3);

    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::US2, itkPermuteAxesImageFilterUS2);
    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::US3, itkPermuteAxesImageFilterUS3);

    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::UI2, itkPermuteAxesImageFilterUI2);
    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::UI3, itkPermuteAxesImageFilterUI3);

    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::SC2, itkPermuteAxesImageFilterSC2);
    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::SC3, itkPermuteAxesImageFilterSC3);

    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::SS2, itkPermuteAxesImageFilterSS2);
    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::SS3, itkPermuteAxesImageFilterSS3);

    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::SI2, itkPermuteAxesImageFilterSI2);
    ITK_WRAP_OBJECT1(PermuteAxesImageFilter, image::SI3, itkPermuteAxesImageFilterSI3);
  }
}

#endif
