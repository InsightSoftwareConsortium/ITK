/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkStatisticsImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImage.h"
#include "itkStatisticsImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkStatisticsImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(StatisticsImageFilter, image::F2, itkStatisticsImageFilterF2);
    ITK_WRAP_OBJECT1(StatisticsImageFilter, image::F3, itkStatisticsImageFilterF3);
    ITK_WRAP_OBJECT1(StatisticsImageFilter, image::US2, itkStatisticsImageFilterUS2);
    ITK_WRAP_OBJECT1(StatisticsImageFilter, image::US3, itkStatisticsImageFilterUS3);
  }
}

#endif
