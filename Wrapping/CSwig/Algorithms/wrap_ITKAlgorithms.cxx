/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_ITKAlgorithms.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
    ITK_WRAP_GROUP(itkDemonsRegistrationFilter),
    ITK_WRAP_GROUP(itkHistogramMatchingImageFilter),
    ITK_WRAP_GROUP(itkImageRegistrationMethod),
    ITK_WRAP_GROUP(itkImageToImageMetric),
    ITK_WRAP_GROUP(itkMeanSquaresImageToImageMetric),
    ITK_WRAP_GROUP(itkMutualInformationImageToImageMetric),
    ITK_WRAP_GROUP(itkMultiResolutionImageRegistrationMethod),
    ITK_WRAP_GROUP(itkNormalizedCorrelationImageToImageMetric),
    ITK_WRAP_GROUP(itkOtsuThresholdImageCalculator),
    ITK_WRAP_GROUP(itkMeanReciprocalSquareDifferenceImageToImageMetric),
    ITK_WRAP_GROUP(itkThresholdSegmentationLevelSetImageFilter),
    ITK_WRAP_GROUP(itkGeodesicActiveContourLevelSetImageFilter),
    ITK_WRAP_GROUP(itkShapeDetectionLevelSetImageFilter),
    ITK_WRAP_GROUP(itkCurvesLevelSetImageFilter),
    ITK_WRAP_GROUP(itkNarrowBandLevelSetImageFilter),     
    ITK_WRAP_GROUP(itkNarrowBandCurvesLevelSetImageFilter),
    ITK_WRAP_GROUP(itkMattesMutualInformationImageToImageMetric),
    ITK_WRAP_GROUP(itkPDEDeformableRegistrationFilter),
    ITK_WRAP_GROUP(itkRecursiveMultiResolutionPyramidImageFilter),
    ITK_WRAP_GROUP(itkVoronoiSegmentationImageFilter),
    ITK_WRAP_GROUP(itkWatershedImageFilter),
    ITK_WRAP_GROUP(itkSegmentationLevelSetImageFilter),
    ITK_WRAP_GROUP(itkTreeNodeSO),
    ITK_WRAP_GROUP(itkSparseFieldLevelSetImageFilter),
    ITK_WRAP_GROUP(itkSymmetricForcesDemonsRegistrationFilter),
    ITK_WRAP_GROUP(itkLevelSetFunction)
  };
}
#endif
