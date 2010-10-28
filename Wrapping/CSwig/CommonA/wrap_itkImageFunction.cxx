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
#include "itkImageFunction.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkImageFunction);
  namespace wrappers
  {
    // wrap ImageFunction
    ITK_WRAP_OBJECT3(ImageFunction, image::F2, double, float, itkImageFunctionF2DF);
    ITK_WRAP_OBJECT3(ImageFunction, image::F3, double, float, itkImageFunctionF3DF);
    ITK_WRAP_OBJECT3(ImageFunction, image::US2, double, float, itkImageFunctionUS2DF);
    ITK_WRAP_OBJECT3(ImageFunction, image::US3, double, float, itkImageFunctionUS3DF);

    ITK_WRAP_OBJECT3(ImageFunction, image::F2, double, double, itkImageFunctionF2DD);
    ITK_WRAP_OBJECT3(ImageFunction, image::F3, double, double, itkImageFunctionF3DD);
    ITK_WRAP_OBJECT3(ImageFunction, image::US2, double, double, itkImageFunctionUS2DD);
    ITK_WRAP_OBJECT3(ImageFunction, image::US3, double, double, itkImageFunctionUS3DD);
  }
}

#endif
