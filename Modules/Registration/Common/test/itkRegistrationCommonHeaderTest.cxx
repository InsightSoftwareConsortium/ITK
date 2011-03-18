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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>

#include "itkCommandVnlIterationUpdate.h"
#include "itkPDEDeformableRegistrationFunction.h"
#include "itkPointSetToPointSetMetric.h"
#include "itkMeanSquaresPointSetToImageMetric.h"
#include "itkMeanSquaresHistogramImageToImageMetric.txx"
#include "itkPointSetToImageRegistrationMethod.txx"
#include "itkImageToImageMetric.txx"
#include "itkPointSetToSpatialObjectDemonsRegistration.txx"
#include "itkNormalizedCorrelationPointSetToImageMetric.txx"
#include "itkMultiResolutionImageRegistrationMethod.h"
#include "itkKullbackLeiblerCompareHistogramImageToImageMetric.txx"
#include "itkPointSetToImageMetric.txx"
#include "itkImageRegistrationMethodImageSource.h"
#include "itkHistogramImageToImageMetric.txx"
#include "itkMeanSquaresPointSetToImageMetric.txx"
#include "itkSimpleMultiResolutionImageRegistrationUI.h"
#include "itkMatchCardinalityImageToImageMetric.txx"
#include "itkMeanSquareRegistrationFunction.txx"
#include "itkGradientDifferenceImageToImageMetric.txx"
#include "itkPointSetToPointSetRegistrationMethod.txx"
#include "itkKappaStatisticImageToImageMetric.txx"
#include "itkImageRegistrationMethod.h"
#include "itkMeanSquaresImageToImageMetric.txx"
#include "itkImageToSpatialObjectRegistrationMethod.h"
#include "itkMutualInformationImageToImageMetric.txx"
#include "itkMultiResolutionImageRegistrationMethod.txx"
#include "itkCorrelationCoefficientHistogramImageToImageMetric.h"
#include "itkEuclideanDistancePointMetric.txx"
#include "itkRecursiveMultiResolutionPyramidImageFilter.txx"
#include "itkRecursiveMultiResolutionPyramidImageFilter.h"
#include "itkNormalizedCorrelationImageToImageMetric.txx"
#include "itkMeanReciprocalSquareDifferenceImageToImageMetric.txx"
#include "itkNormalizedMutualInformationHistogramImageToImageMetric.txx"
#include "itkCommandIterationUpdate.h"
#include "itkMultiResolutionPyramidImageFilter.txx"
#include "itkImageRegistrationMethod.txx"
#include "itkMutualInformationHistogramImageToImageMetric.txx"
#include "itkNormalizedMutualInformationHistogramImageToImageMetric.h"
#include "itkMattesMutualInformationImageToImageMetric.txx"
#include "itkPointSetToPointSetMetric.txx"
#include "itkCompareHistogramImageToImageMetric.txx"
#include "itkImageToSpatialObjectRegistrationMethod.txx"
#include "itkImageToSpatialObjectMetric.txx"
#include "itkCorrelationCoefficientHistogramImageToImageMetric.txx"
#include "itkMeanReciprocalSquareDifferencePointSetToImageMetric.txx"



int itkRegistrationCommonHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
