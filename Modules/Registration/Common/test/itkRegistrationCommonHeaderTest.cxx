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
#include "itkMeanSquaresHistogramImageToImageMetric.hxx"
#include "itkPointSetToImageRegistrationMethod.hxx"
#include "itkImageToImageMetric.hxx"
#include "itkPointSetToSpatialObjectDemonsRegistration.hxx"
#include "itkNormalizedCorrelationPointSetToImageMetric.hxx"
#include "itkMultiResolutionImageRegistrationMethod.h"
#include "itkKullbackLeiblerCompareHistogramImageToImageMetric.hxx"
#include "itkPointSetToImageMetric.hxx"
#include "itkImageRegistrationMethodImageSource.h"
#include "itkHistogramImageToImageMetric.hxx"
#include "itkMeanSquaresPointSetToImageMetric.hxx"
#include "itkSimpleMultiResolutionImageRegistrationUI.h"
#include "itkMatchCardinalityImageToImageMetric.hxx"
#include "itkMeanSquareRegistrationFunction.hxx"
#include "itkGradientDifferenceImageToImageMetric.hxx"
#include "itkPointSetToPointSetRegistrationMethod.hxx"
#include "itkKappaStatisticImageToImageMetric.hxx"
#include "itkImageRegistrationMethod.h"
#include "itkMeanSquaresImageToImageMetric.hxx"
#include "itkImageToSpatialObjectRegistrationMethod.h"
#include "itkMutualInformationImageToImageMetric.hxx"
#include "itkMultiResolutionImageRegistrationMethod.hxx"
#include "itkCorrelationCoefficientHistogramImageToImageMetric.h"
#include "itkEuclideanDistancePointMetric.hxx"
#include "itkRecursiveMultiResolutionPyramidImageFilter.hxx"
#include "itkRecursiveMultiResolutionPyramidImageFilter.h"
#include "itkNormalizedCorrelationImageToImageMetric.hxx"
#include "itkMeanReciprocalSquareDifferenceImageToImageMetric.hxx"
#include "itkNormalizedMutualInformationHistogramImageToImageMetric.hxx"
#include "itkCommandIterationUpdate.h"
#include "itkMultiResolutionPyramidImageFilter.hxx"
#include "itkImageRegistrationMethod.hxx"
#include "itkMutualInformationHistogramImageToImageMetric.hxx"
#include "itkNormalizedMutualInformationHistogramImageToImageMetric.h"
#include "itkMattesMutualInformationImageToImageMetric.hxx"
#include "itkPointSetToPointSetMetric.hxx"
#include "itkCompareHistogramImageToImageMetric.hxx"
#include "itkImageToSpatialObjectRegistrationMethod.hxx"
#include "itkImageToSpatialObjectMetric.hxx"
#include "itkCorrelationCoefficientHistogramImageToImageMetric.hxx"
#include "itkMeanReciprocalSquareDifferencePointSetToImageMetric.hxx"



int itkRegistrationCommonHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
