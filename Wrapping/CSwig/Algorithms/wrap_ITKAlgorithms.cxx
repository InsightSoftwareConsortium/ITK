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
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const package = ITK_WRAP_PACKAGE_NAME(ITK_WRAP_PACKAGE);
  const char* const groups[] =
  {
    ITK_WRAP_GROUP(itkCurvatureFlowImageFilter),
    ITK_WRAP_GROUP(itkHistogramMatchingImageFilter),
    ITK_WRAP_GROUP(itkImageRegistrationMethod),
    ITK_WRAP_GROUP(itkImageToImageMetric),
    ITK_WRAP_GROUP(itkMeanSquaresImageToImageMetric),
    ITK_WRAP_GROUP(itkMutualInformationImageToImageMetric),
    ITK_WRAP_GROUP(itkNormalizedCorrelationImageToImageMetric),
    ITK_WRAP_GROUP(itkOtsuThresholdImageCalculator),
    ITK_WRAP_GROUP(itkMeanReciprocalSquareDifferenceImageToImageMetric),
    ITK_WRAP_GROUP(itkSimpleFuzzyConnectednessImageFilterBase),
    ITK_WRAP_GROUP(itkSimpleFuzzyConnectednessScalarImageFilter),
    ITK_WRAP_GROUP(itkThresholdSegmentationLevelSetImageFilter),
    ITK_WRAP_GROUP(itkGeodesicActiveContourLevelSetImageFilter),
    ITK_WRAP_GROUP(itkShapeDetectionLevelSetImageFilter),
    ITK_WRAP_GROUP(itkCurvesLevelSetImageFilter),
    ITK_WRAP_GROUP(itkNarrowBandCurvesLevelSetImageFilter)
  };
}
#endif
