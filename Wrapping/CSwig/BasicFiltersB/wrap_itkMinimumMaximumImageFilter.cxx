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
#include "itkMinimumMaximumImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkMinimumMaximumImageFilter);
  namespace wrappers
  {
    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT1(MinimumMaximumImageFilter, image::F2,
                     itkMinimumMaximumImageFilterF2);
    ITK_WRAP_OBJECT1(MinimumMaximumImageFilter, image::US2,
                     itkMinimumMaximumImageFilterUS2);

    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT1(MinimumMaximumImageFilter, image::F3,
                     itkMinimumMaximumImageFilterF3);
    ITK_WRAP_OBJECT1(MinimumMaximumImageFilter, image::US3,
                     itkMinimumMaximumImageFilterUS3);
  }
}
#endif
