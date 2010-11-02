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
#include "itkAnisotropicDiffusionImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkAnisotropicDiffusionImageFilter_2D);
  namespace wrappers
  {
    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::F2 , image::F2 , itkAnisotropicDiffusionImageFilterF2F2  );
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::D2 , image::D2 , itkAnisotropicDiffusionImageFilterD2D2  );
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::UC2, image::F2, itkAnisotropicDiffusionImageFilterUC2F2);
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::US2, image::F2, itkAnisotropicDiffusionImageFilterUS2F2);
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::UI2, image::F2, itkAnisotropicDiffusionImageFilterUI2F2);
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::SC2, image::F2, itkAnisotropicDiffusionImageFilterSC2F2);
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::SS2, image::F2, itkAnisotropicDiffusionImageFilterSS2F2);
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::SI2, image::F2, itkAnisotropicDiffusionImageFilterSI2F2);
  }
}
#endif
