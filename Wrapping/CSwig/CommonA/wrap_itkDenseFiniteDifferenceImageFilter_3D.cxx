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
#include "itkDenseFiniteDifferenceImageFilter.h"
#include "itkVector.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group =
  ITK_WRAP_GROUP(itkDenseFiniteDifferenceImageFilter_3D);
  namespace wrappers
  {
    // vector image wrapped Filters
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter,
                     image::VF3, image::VF3,
                     itkDenseFiniteDifferenceImageFilterVF3VF3);

    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::F3 , image::F3 , itkDenseFiniteDifferenceImageFilterF3F3  );
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::D3 , image::D3 , itkDenseFiniteDifferenceImageFilterD3D3  );
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::UC3, image::F3, itkDenseFiniteDifferenceImageFilterUC3F3);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::US3, image::F3, itkDenseFiniteDifferenceImageFilterUS3F3);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::UI3, image::F3, itkDenseFiniteDifferenceImageFilterUI3F3);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::SC3, image::F3, itkDenseFiniteDifferenceImageFilterSC3F3);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::SS3, image::F3, itkDenseFiniteDifferenceImageFilterSS3F3);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::SI3, image::F3, itkDenseFiniteDifferenceImageFilterSI3F3);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::F3 , image::VF3 ,itkDenseFiniteDifferenceImageFilterF3VF3);
    ITK_WRAP_OBJECT2(DenseFiniteDifferenceImageFilter, image::US3, image::VF3, itkDenseFiniteDifferenceImageFilterUS3VF3);
  }
}

#endif
