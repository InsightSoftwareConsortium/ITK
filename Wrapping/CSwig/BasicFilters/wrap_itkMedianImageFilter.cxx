/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkMedianImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMedianImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkMedianImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT2(MedianImageFilter, image::F2, image::F2, 
                     itkMedianImageFilterF2F2);
    ITK_WRAP_OBJECT2(MedianImageFilter, image::US2, image::US2, 
                     itkMedianImageFilterUS2US2);
    ITK_WRAP_OBJECT2(MedianImageFilter, image::F3, image::F3, 
                     itkMedianImageFilterF3F3);
    ITK_WRAP_OBJECT2(MedianImageFilter, image::US3, image::US3, 
                     itkMedianImageFilterUS3US3);
  }
}


#endif
