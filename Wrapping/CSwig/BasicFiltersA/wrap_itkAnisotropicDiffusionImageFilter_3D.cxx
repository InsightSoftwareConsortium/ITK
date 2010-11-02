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
  const char* const group = ITK_WRAP_GROUP(itkAnisotropicDiffusionImageFilter_3D);
  namespace wrappers
  {
    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::F3 , image::F3 , itkAnisotropicDiffusionImageFilterF3F3  );
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::D3 , image::D3 , itkAnisotropicDiffusionImageFilterD3D3  );
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::UC3, image::F3, itkAnisotropicDiffusionImageFilterUC3F3);
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::US3, image::F3, itkAnisotropicDiffusionImageFilterUS3F3);
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::UI3, image::F3, itkAnisotropicDiffusionImageFilterUI3F3);
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::SC3, image::F3, itkAnisotropicDiffusionImageFilterSC3F3);
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::SS3, image::F3, itkAnisotropicDiffusionImageFilterSS3F3);
    ITK_WRAP_OBJECT2(AnisotropicDiffusionImageFilter, image::SI3, image::F3, itkAnisotropicDiffusionImageFilterSI3F3);
  }
}
#endif
