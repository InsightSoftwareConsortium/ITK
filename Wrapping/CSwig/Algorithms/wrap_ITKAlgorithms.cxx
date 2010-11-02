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
