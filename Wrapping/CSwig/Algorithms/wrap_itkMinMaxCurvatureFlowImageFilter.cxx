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
#include "itkMinMaxCurvatureFlowImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

//=================================
//THIS FILE GENERATED WITH MakeConsistentWrappedClasses.sh
//=================================

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkMinMaxCurvatureFlowImageFilter);
  namespace wrappers
  {
    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT2(MinMaxCurvatureFlowImageFilter, image::F2 , image::F2 , itkMinMaxCurvatureFlowImageFilterF2F2  );
    ITK_WRAP_OBJECT2(MinMaxCurvatureFlowImageFilter, image::D2 , image::D2 , itkMinMaxCurvatureFlowImageFilterD2D2  );

    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT2(MinMaxCurvatureFlowImageFilter, image::F3 , image::F3 , itkMinMaxCurvatureFlowImageFilterF3F3  );
    ITK_WRAP_OBJECT2(MinMaxCurvatureFlowImageFilter, image::D3 , image::D3 , itkMinMaxCurvatureFlowImageFilterD3D3  );
  }
}
#endif
