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

#include "itkAccumulateImageFilter.hxx"
#include "itkAdaptiveHistogramEqualizationImageFilter.hxx"
#include "itkBinaryProjectionImageFilter.h"
#include "itkGetAverageSliceImageFilter.hxx"
#include "itkHistogramAlgorithmBase.hxx"
#include "itkImageMomentsCalculator.hxx"
#include "itkImagePCADecompositionCalculator.hxx"
#include "itkImagePCAShapeModelEstimator.hxx"
#include "itkImageShapeModelEstimatorBase.hxx"
#include "itkLabelStatisticsImageFilter.hxx"
#include "itkMaximumProjectionImageFilter.h"
#include "itkMeanProjectionImageFilter.h"
#include "itkMedianProjectionImageFilter.h"
#include "itkMinimumMaximumImageFilter.hxx"
#include "itkMinimumProjectionImageFilter.h"
#include "itkNormalizedCorrelationImageFilter.hxx"
#include "itkProjectionImageFilter.hxx"
#include "itkStandardDeviationProjectionImageFilter.h"
#include "itkStatisticsImageFilter.hxx"
#include "itkSumProjectionImageFilter.h"



int itkImageStatisticsHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
