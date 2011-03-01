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

#include "itkBinaryThresholdImageFunction.h"
#include "itkBinaryThresholdImageFunction.txx"
//#include "itkBSplineInterpolateImageFunction.h"  BUG 11905
//#include "itkBSplineInterpolateImageFunction.txx"
//#include "itkBSplineResampleImageFunction.h"
#include "itkCentralDifferenceImageFunction.h"
#include "itkCentralDifferenceImageFunction.txx"
#include "itkCovarianceImageFunction.h"
#include "itkCovarianceImageFunction.txx"
#include "itkExtrapolateImageFunction.h"
#include "itkGaussianBlurImageFunction.h"
#include "itkGaussianBlurImageFunction.txx"
#include "itkGaussianDerivativeImageFunction.h"
#include "itkGaussianDerivativeImageFunction.txx"
#include "itkImageFunction.h"
#include "itkImageFunction.txx"
#include "itkInterpolateImageFunction.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkLinearInterpolateImageFunction.txx"
#include "itkMahalanobisDistanceThresholdImageFunction.h"
#include "itkMahalanobisDistanceThresholdImageFunction.txx"
#include "itkMeanImageFunction.h"
#include "itkMeanImageFunction.txx"
#include "itkMedianImageFunction.h"
#include "itkMedianImageFunction.txx"
#include "itkNearestNeighborExtrapolateImageFunction.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkNeighborhoodBinaryThresholdImageFunction.h"
#include "itkNeighborhoodBinaryThresholdImageFunction.txx"
#include "itkNeighborhoodOperatorImageFunction.h"
#include "itkNeighborhoodOperatorImageFunction.txx"
#include "itkRayCastInterpolateImageFunction.h"
#include "itkRayCastInterpolateImageFunction.txx"
#include "itkScatterMatrixImageFunction.h"
#include "itkScatterMatrixImageFunction.txx"
#include "itkSumOfSquaresImageFunction.h"
#include "itkSumOfSquaresImageFunction.txx"
#include "itkVarianceImageFunction.h"
#include "itkVarianceImageFunction.txx"
#include "itkVectorInterpolateImageFunction.h"
#include "itkVectorLinearInterpolateImageFunction.h"
#include "itkVectorLinearInterpolateImageFunction.txx"
#include "itkVectorMeanImageFunction.h"
#include "itkVectorMeanImageFunction.txx"
#include "itkVectorNearestNeighborInterpolateImageFunction.h"
#include "itkWindowedSincInterpolateImageFunction.h"
#include "itkWindowedSincInterpolateImageFunction.txx"



int itkImageFunctionHeaderTest ( int , char ** )
{

  return EXIT_SUCCESS;
}
