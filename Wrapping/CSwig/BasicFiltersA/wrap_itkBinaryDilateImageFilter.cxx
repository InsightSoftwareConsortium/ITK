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
#include "itkBinaryDilateImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"
#include "itkCSwigBinaryBallStructuringElement.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkBinaryDilateImageFilter);
  namespace wrappers
  {
    // NOTE: since both the BinaryDilateImageFilter and BinaryErodeImageFilter derive from the same superclass, only one of
    //       them should do the wrapping WITH_SUPERCLASS.
    //
    //===========2D Wrapped Filters==============
    ITK_WRAP_OBJECT3(BinaryDilateImageFilter, image::F2 , image::F2 , structuringElement::F2,   itkBinaryDilateImageFilterF2F2  );
    ITK_WRAP_OBJECT3(BinaryDilateImageFilter, image::UC2, image::UC2, structuringElement::UC2,  itkBinaryDilateImageFilterUC2UC2);
    ITK_WRAP_OBJECT3(BinaryDilateImageFilter, image::US2, image::US2, structuringElement::US2,  itkBinaryDilateImageFilterUS2US2);

    //===========3D Wrapped Filters==============
    ITK_WRAP_OBJECT3(BinaryDilateImageFilter, image::F3 , image::F3 , structuringElement::F3,   itkBinaryDilateImageFilterF3F3  );
    ITK_WRAP_OBJECT3(BinaryDilateImageFilter, image::UC3, image::UC3, structuringElement::UC3,  itkBinaryDilateImageFilterUC3UC3);
    ITK_WRAP_OBJECT3(BinaryDilateImageFilter, image::US3, image::US3, structuringElement::US3,  itkBinaryDilateImageFilterUS3US3);
  }
}


#endif
