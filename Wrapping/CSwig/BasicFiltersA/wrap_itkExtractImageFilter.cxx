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
#include "itkExtractImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkExtractImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT2(ExtractImageFilter,  image::F2,  image::F2,   itkExtractImageFilterF2F2);
    ITK_WRAP_OBJECT2(ExtractImageFilter,  image::D2,  image::D2,   itkExtractImageFilterD2D2);
    ITK_WRAP_OBJECT2(ExtractImageFilter, image::UC2, image::UC2, itkExtractImageFilterUC2UC2);
    ITK_WRAP_OBJECT2(ExtractImageFilter, image::US2, image::US2, itkExtractImageFilterUS2US2);
    ITK_WRAP_OBJECT2(ExtractImageFilter, image::UI2, image::UI2, itkExtractImageFilterUI2UI2);
    ITK_WRAP_OBJECT2(ExtractImageFilter, image::SC2, image::SC2, itkExtractImageFilterSC2SC2);
    ITK_WRAP_OBJECT2(ExtractImageFilter, image::SS2, image::SS2, itkExtractImageFilterSS2SS2);
    ITK_WRAP_OBJECT2(ExtractImageFilter, image::SI2, image::SI2, itkExtractImageFilterSI2SI2);

    ITK_WRAP_OBJECT2(ExtractImageFilter,  image::F3,  image::F3,   itkExtractImageFilterF3F3);
    ITK_WRAP_OBJECT2(ExtractImageFilter,  image::D3,  image::D3,   itkExtractImageFilterD3D3);
    ITK_WRAP_OBJECT2(ExtractImageFilter, image::UC3, image::UC3, itkExtractImageFilterUC3UC3);
    ITK_WRAP_OBJECT2(ExtractImageFilter, image::US3, image::US3, itkExtractImageFilterUS3US3);
    ITK_WRAP_OBJECT2(ExtractImageFilter, image::UI3, image::UI3, itkExtractImageFilterUI3UI3);
    ITK_WRAP_OBJECT2(ExtractImageFilter, image::SC3, image::SC3, itkExtractImageFilterSC3SC3);
    ITK_WRAP_OBJECT2(ExtractImageFilter, image::SS3, image::SS3, itkExtractImageFilterSS3SS3);
    ITK_WRAP_OBJECT2(ExtractImageFilter, image::SI3, image::SI3, itkExtractImageFilterSI3SI3);

    ITK_WRAP_OBJECT2(ExtractImageFilter,  image::F3,  image::F2,   itkExtractImageFilterF3F2);
    ITK_WRAP_OBJECT2(ExtractImageFilter,  image::D3,  image::D2,   itkExtractImageFilterD3D2);
    ITK_WRAP_OBJECT2(ExtractImageFilter, image::UC3, image::UC2, itkExtractImageFilterUC3UC2);
    ITK_WRAP_OBJECT2(ExtractImageFilter, image::US3, image::US2, itkExtractImageFilterUS3US2);
    ITK_WRAP_OBJECT2(ExtractImageFilter, image::UI3, image::UI2, itkExtractImageFilterUI3UI2);
    ITK_WRAP_OBJECT2(ExtractImageFilter, image::SC3, image::SC2, itkExtractImageFilterSC3SC2);
    ITK_WRAP_OBJECT2(ExtractImageFilter, image::SS3, image::SS2, itkExtractImageFilterSS3SS2);
    ITK_WRAP_OBJECT2(ExtractImageFilter, image::SI3, image::SI2, itkExtractImageFilterSI3SI2);

  }
}

#endif
