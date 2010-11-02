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
#include "itkImageRegionConstIterator.h"
#include "itkCovariantVector.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkImageRegionConstIterator);

#define ITK_WRAP_ITERATOR(name, arg1, wrapname)  typedef itk::name<arg1 > wrapname;

  namespace wrappers
  {
    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::F2 , itkImageRegionConstIteratorF2 );
    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::D2 , itkImageRegionConstIteratorD2 );
    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::UC2, itkImageRegionConstIteratorUC2);
    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::US2, itkImageRegionConstIteratorUS2);
    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::UI2, itkImageRegionConstIteratorUI2);
    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::UL2, itkImageRegionConstIteratorUL2);
    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::SC2, itkImageRegionConstIteratorSC2);
    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::SS2, itkImageRegionConstIteratorSS2);
    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::SI2, itkImageRegionConstIteratorSI2);
    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::VF2 , itkImageRegionConstIteratorVF2 );
    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::CVF2 , itkImageRegionConstIteratorCVF2 );

    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::F3 , itkImageRegionConstIteratorF3 );
    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::D3 , itkImageRegionConstIteratorD3 );
    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::UC3, itkImageRegionConstIteratorUC3);
    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::US3, itkImageRegionConstIteratorUS3);
    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::UI3, itkImageRegionConstIteratorUI3);
    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::UL3, itkImageRegionConstIteratorUL3);
    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::SC3, itkImageRegionConstIteratorSC3);
    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::SS3, itkImageRegionConstIteratorSS3);
    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::SI3, itkImageRegionConstIteratorSI3);
    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::VF3 , itkImageRegionConstIteratorVF3 );
    ITK_WRAP_ITERATOR(ImageRegionConstIterator, image::CVF3 , itkImageRegionConstIteratorCVF3 );
  }
}
#endif
