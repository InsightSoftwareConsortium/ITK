/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_ITKAlgorithms.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifdef CABLE_CONFIGURATION
#include "wrap_ITKAlgorithms.h"
namespace _cable_
{
  const char* const package = ITK_WRAP_PACKAGE;
  const char* const package_version = ITK_WRAP_PACKAGE_VERSION;
  const char* const groups[] =
  {
    ITK_WRAP_GROUP(itkCurvatureFlowImageFilter),
    ITK_WRAP_GROUP(itkHistogramMatchingImageFilter),
    ITK_WRAP_GROUP(itkImageRegistrationMethod),
    ITK_WRAP_GROUP(itkImageToImageMetric),
    ITK_WRAP_GROUP(itkMeanSquaresImageToImageMetric),
    ITK_WRAP_GROUP(itkMutualInformationImageToImageMetric),
    ITK_WRAP_GROUP(itkNormalizedCorrelationImageToImageMetric),
    ITK_WRAP_GROUP(itkPatternIntensityImageToImageMetric),
    ITK_WRAP_GROUP(itkSimpleFuzzyConnectednessImageFilterBase),
    ITK_WRAP_GROUP(itkSimpleFuzzyConnectednessScalarImageFilter)
  };
}
#endif
