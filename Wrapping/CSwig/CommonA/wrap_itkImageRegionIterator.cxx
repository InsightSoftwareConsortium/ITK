/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkImageRegionIterator.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkVector.h"
#include "itkCovariantVector.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigImages.h"
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkImageRegionIterator);

#define ITK_WRAP_ITERATOR(name, arg1, wrapname)  typedef itk::name<arg1 > wrapname; 
  
  namespace wrappers
  {
    ITK_WRAP_ITERATOR(ImageRegionIterator, image::F2 , itkImageRegionIteratorF2 );
    ITK_WRAP_ITERATOR(ImageRegionIterator, image::D2 , itkImageRegionIteratorD2 );
    ITK_WRAP_ITERATOR(ImageRegionIterator, image::UC2, itkImageRegionIteratorUC2);
    ITK_WRAP_ITERATOR(ImageRegionIterator, image::US2, itkImageRegionIteratorUS2);
    ITK_WRAP_ITERATOR(ImageRegionIterator, image::UI2, itkImageRegionIteratorUI2);
    ITK_WRAP_ITERATOR(ImageRegionIterator, image::UL2, itkImageRegionIteratorUL2);
    ITK_WRAP_ITERATOR(ImageRegionIterator, image::SC2, itkImageRegionIteratorSC2);
    ITK_WRAP_ITERATOR(ImageRegionIterator, image::SS2, itkImageRegionIteratorSS2);
    ITK_WRAP_ITERATOR(ImageRegionIterator, image::SI2, itkImageRegionIteratorSI2);
    ITK_WRAP_ITERATOR(ImageRegionIterator, image::VF2 , itkImageRegionIteratorVF2 );
    ITK_WRAP_ITERATOR(ImageRegionIterator, image::CVF2 , itkImageRegionIteratorCVF2 );

    ITK_WRAP_ITERATOR(ImageRegionIterator, image::F3 , itkImageRegionIteratorF3 );
    ITK_WRAP_ITERATOR(ImageRegionIterator, image::D3 , itkImageRegionIteratorD3 );
    ITK_WRAP_ITERATOR(ImageRegionIterator, image::UC3, itkImageRegionIteratorUC3);
    ITK_WRAP_ITERATOR(ImageRegionIterator, image::US3, itkImageRegionIteratorUS3);
    ITK_WRAP_ITERATOR(ImageRegionIterator, image::UI3, itkImageRegionIteratorUI3);
    ITK_WRAP_ITERATOR(ImageRegionIterator, image::UL3, itkImageRegionIteratorUL3);
    ITK_WRAP_ITERATOR(ImageRegionIterator, image::SC3, itkImageRegionIteratorSC3);
    ITK_WRAP_ITERATOR(ImageRegionIterator, image::SS3, itkImageRegionIteratorSS3);
    ITK_WRAP_ITERATOR(ImageRegionIterator, image::SI3, itkImageRegionIteratorSI3);
    ITK_WRAP_ITERATOR(ImageRegionIterator, image::VF3 , itkImageRegionIteratorVF3 );
    ITK_WRAP_ITERATOR(ImageRegionIterator, image::CVF3 , itkImageRegionIteratorCVF3 );
  }
}
#endif
