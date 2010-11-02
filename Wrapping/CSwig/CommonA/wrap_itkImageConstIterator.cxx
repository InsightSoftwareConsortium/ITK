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
#include "itkImageConstIterator.h"
#include "itkCovariantVector.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkImageConstIterator);

#define ITK_WRAP_ITERATOR(name, arg1, wrapname)  typedef itk::name<arg1 > wrapname;

  namespace wrappers
  {
    ITK_WRAP_ITERATOR(ImageConstIterator, image::F2 , itkImageConstIteratorF2 );
    ITK_WRAP_ITERATOR(ImageConstIterator, image::D2 , itkImageConstIteratorD2 );
    ITK_WRAP_ITERATOR(ImageConstIterator, image::UC2, itkImageConstIteratorUC2);
    ITK_WRAP_ITERATOR(ImageConstIterator, image::US2, itkImageConstIteratorUS2);
    ITK_WRAP_ITERATOR(ImageConstIterator, image::UI2, itkImageConstIteratorUI2);
    ITK_WRAP_ITERATOR(ImageConstIterator, image::UL2, itkImageConstIteratorUL2);
    ITK_WRAP_ITERATOR(ImageConstIterator, image::SC2, itkImageConstIteratorSC2);
    ITK_WRAP_ITERATOR(ImageConstIterator, image::SS2, itkImageConstIteratorSS2);
    ITK_WRAP_ITERATOR(ImageConstIterator, image::SI2, itkImageConstIteratorSI2);
    ITK_WRAP_ITERATOR(ImageConstIterator, image::VF2 , itkImageConstIteratorVF2 );
    ITK_WRAP_ITERATOR(ImageConstIterator, image::CVF2 , itkImageConstIteratorCVF2 );

    ITK_WRAP_ITERATOR(ImageConstIterator, image::F3 , itkImageConstIteratorF3 );
    ITK_WRAP_ITERATOR(ImageConstIterator, image::D3 , itkImageConstIteratorD3 );
    ITK_WRAP_ITERATOR(ImageConstIterator, image::UC3, itkImageConstIteratorUC3);
    ITK_WRAP_ITERATOR(ImageConstIterator, image::US3, itkImageConstIteratorUS3);
    ITK_WRAP_ITERATOR(ImageConstIterator, image::UI3, itkImageConstIteratorUI3);
    ITK_WRAP_ITERATOR(ImageConstIterator, image::UL3, itkImageConstIteratorUL3);
    ITK_WRAP_ITERATOR(ImageConstIterator, image::SC3, itkImageConstIteratorSC3);
    ITK_WRAP_ITERATOR(ImageConstIterator, image::SS3, itkImageConstIteratorSS3);
    ITK_WRAP_ITERATOR(ImageConstIterator, image::SI3, itkImageConstIteratorSI3);
    ITK_WRAP_ITERATOR(ImageConstIterator, image::VF3 , itkImageConstIteratorVF3 );
    ITK_WRAP_ITERATOR(ImageConstIterator, image::CVF3 , itkImageConstIteratorCVF3 );
  }
}
#endif
