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

#include "itkBinaryThresholdImageFunction.hxx"
//BUG 11905
//#include "itkBSplineInterpolateImageFunction.hxx"
//#include "itkBSplineResampleImageFunction.h"
#include "itkCentralDifferenceImageFunction.hxx"
#include "itkCovarianceImageFunction.hxx"
#include "itkExtrapolateImageFunction.h"
#include "itkGaussianBlurImageFunction.hxx"
#include "itkGaussianDerivativeImageFunction.hxx"
#include "itkImageFunction.hxx"
#include "itkInterpolateImageFunction.h"
#include "itkLinearInterpolateImageFunction.hxx"
#include "itkMahalanobisDistanceThresholdImageFunction.hxx"
#include "itkMeanImageFunction.hxx"
#include "itkMedianImageFunction.hxx"
#include "itkNearestNeighborExtrapolateImageFunction.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkNeighborhoodBinaryThresholdImageFunction.hxx"
#include "itkNeighborhoodOperatorImageFunction.hxx"
#include "itkRayCastInterpolateImageFunction.hxx"
#include "itkScatterMatrixImageFunction.hxx"
#include "itkSumOfSquaresImageFunction.hxx"
#include "itkVarianceImageFunction.hxx"
#include "itkVectorInterpolateImageFunction.h"
#include "itkVectorLinearInterpolateImageFunction.hxx"
#include "itkVectorMeanImageFunction.hxx"
#include "itkVectorNearestNeighborInterpolateImageFunction.h"
#include "itkWindowedSincInterpolateImageFunction.hxx"



int itkImageFunctionHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
