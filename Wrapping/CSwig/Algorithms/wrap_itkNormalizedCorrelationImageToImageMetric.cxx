/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkNormalizedCorrelationImageToImageMetric.cxx
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
#include "itkNormalizedCorrelationImageToImageMetric.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkNormalizedCorrelationImageToImageMetric);
  namespace wrappers
  {
    ITK_WRAP_OBJECT2(NormalizedCorrelationImageToImageMetric, image::F2, image::F2,
                     NormalizedCorrelationImageToImageMetricF2F2);
    ITK_WRAP_OBJECT2(NormalizedCorrelationImageToImageMetric, image::F3, image::F3,
                     NormalizedCorrelationImageToImageMetricF3F3);
    ITK_WRAP_OBJECT2(NormalizedCorrelationImageToImageMetric, image::US2, image::US2,
                     NormalizedCorrelationImageToImageMetricUS2US2);
    ITK_WRAP_OBJECT2(NormalizedCorrelationImageToImageMetric, image::US3, image::US3,
                     NormalizedCorrelationImageToImageMetricUS3US3);
  }
}


#endif
