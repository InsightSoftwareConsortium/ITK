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
#include "itkSigmoidImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkSigmoidImageFilter);
  namespace wrappers
  {
    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(SigmoidImageFilter, image::F2 , image::F2 , itkSigmoidImageFilterF2F2  );
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(SigmoidImageFilter, image::D2 , image::D2 , itkSigmoidImageFilterD2D2  );
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(SigmoidImageFilter, image::UC2, image::UC2, itkSigmoidImageFilterUC2UC2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(SigmoidImageFilter, image::US2, image::US2, itkSigmoidImageFilterUS2US2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(SigmoidImageFilter, image::UI2, image::UI2, itkSigmoidImageFilterUI2UI2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(SigmoidImageFilter, image::SC2, image::SC2, itkSigmoidImageFilterSC2SC2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(SigmoidImageFilter, image::SS2, image::SS2, itkSigmoidImageFilterSS2SS2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(SigmoidImageFilter, image::SI2, image::SI2, itkSigmoidImageFilterSI2SI2);

    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(SigmoidImageFilter, image::F3 , image::F3 , itkSigmoidImageFilterF3F3  );
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(SigmoidImageFilter, image::D3 , image::D3 , itkSigmoidImageFilterD3D3  );
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(SigmoidImageFilter, image::UC3, image::UC3, itkSigmoidImageFilterUC3UC3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(SigmoidImageFilter, image::US3, image::US3, itkSigmoidImageFilterUS3US3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(SigmoidImageFilter, image::UI3, image::UI3, itkSigmoidImageFilterUI3UI3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(SigmoidImageFilter, image::SC3, image::SC3, itkSigmoidImageFilterSC3SC3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(SigmoidImageFilter, image::SS3, image::SS3, itkSigmoidImageFilterSS3SS3);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(SigmoidImageFilter, image::SI3, image::SI3, itkSigmoidImageFilterSI3SI3);
  }
}


#endif
