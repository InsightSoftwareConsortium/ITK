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
#include "itkCastImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkCastImageFilter_2D);
  namespace wrappers
  {
    //Cast to self
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F2, image::F2,
                                     itkCastImageFilterF2F2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::D2, image::D2,
                                     itkCastImageFilterD2D2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::UC2, image::UC2,
                                     itkCastImageFilterUC2UC2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::US2, image::US2,
                                     itkCastImageFilterUS2US2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::UI2, image::UI2,
                                     itkCastImageFilterUI2UI2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::SC2, image::SC2,
                                     itkCastImageFilterSC2SC2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::SS2, image::SS2,
                                     itkCastImageFilterSS2SS2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::SI2, image::SI2,
                                     itkCastImageFilterSI2SI2);

    //Double to/from float 2D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F2, image::D2,
                                     itkCastImageFilterF2D2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::D2, image::F2,
                                     itkCastImageFilterD2F2);

    //Unsigned char to/from float 2D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F2, image::UC2,
                                     itkCastImageFilterF2UC2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::UC2, image::F2,
                                     itkCastImageFilterUC2F2);
    //Unsigned short to/from float 2D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F2, image::US2,
                                     itkCastImageFilterF2US2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::US2, image::F2,
                                     itkCastImageFilterUS2F2);
    //Unsigned int to/from float 2D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F2, image::UI2,
                                     itkCastImageFilterF2UI2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::UI2, image::F2,
                                     itkCastImageFilterUI2F2);

    //Signed char to/from float 2D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F2, image::SC2,
                                     itkCastImageFilterF2SC2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::SC2, image::F2,
                                     itkCastImageFilterSC2F2);
    //Signed short to/from float 2D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F2, image::SS2,
                                     itkCastImageFilterF2SS2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::SS2, image::F2,
                                     itkCastImageFilterSS2F2);
    //Signed int to/from float 2D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::F2, image::SI2,
                                     itkCastImageFilterF2SI2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::SI2, image::F2,
                                     itkCastImageFilterSI2F2);
    //Unsigned char to/from unsigned short 2D
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::US2, image::UC2,
                                     itkCastImageFilterUS2UC2);
    ITK_WRAP_OBJECT2_WITH_SUPERCLASS(CastImageFilter, image::UC2, image::US2,
                                     itkCastImageFilterUC2US2);

  }
}
#endif
