/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkMeanReciprocalSquareDifferenceImageToImageMetric.cxx
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
#include "itkMeanReciprocalSquareDifferenceImageToImageMetric.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkMeanReciprocalSquareDifferenceImageToImageMetric);
  namespace wrappers
  {
    ITK_WRAP_OBJECT2(MeanReciprocalSquareDifferenceImageToImageMetric, image::F2, image::F2,
                     itkMeanReciprocalSquareDifferenceImageToImageMetricF2F2);
    ITK_WRAP_OBJECT2(MeanReciprocalSquareDifferenceImageToImageMetric, image::F3, image::F3,
                     itkMeanReciprocalSquareDifferenceImageToImageMetricF3F3);
    ITK_WRAP_OBJECT2(MeanReciprocalSquareDifferenceImageToImageMetric, image::US2, image::US2,
                     itkMeanReciprocalSquareDifferenceImageToImageMetricUS2US2);
    ITK_WRAP_OBJECT2(MeanReciprocalSquareDifferenceImageToImageMetric, image::US3, image::US3,
                     itkMeanReciprocalSquareDifferenceImageToImageMetricUS3US3);
  }
}

#endif
