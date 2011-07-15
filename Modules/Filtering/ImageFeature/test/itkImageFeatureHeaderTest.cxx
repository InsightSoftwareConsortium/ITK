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

#include "itkBilateralImageFilter.hxx"
#include "itkSobelEdgeDetectionImageFilter.hxx"
#include "itkDerivativeImageFilter.h"
#include "itkSimpleContourExtractorImageFilter.hxx"
#include "itkLaplacianSharpeningImageFilter.hxx"
#include "itkHessianRecursiveGaussianImageFilter.hxx"
#include "itkGradientVectorFlowImageFilter.h"
#include "itkLaplacianRecursiveGaussianImageFilter.hxx"
#include "itkHoughTransform2DCirclesImageFilter.hxx"
#include "itkCannyEdgeDetectionImageFilter.hxx"
#include "itkDerivativeImageFilter.hxx"
#include "itkZeroCrossingImageFilter.hxx"
#include "itkHessian3DToVesselnessMeasureImageFilter.hxx"
#include "itkHoughTransform2DLinesImageFilter.h"
#include "itkZeroCrossingBasedEdgeDetectionImageFilter.hxx"
#include "itkGradientVectorFlowImageFilter.hxx"
#include "itkLaplacianImageFilter.hxx"
#include "itkHoughTransform2DLinesImageFilter.hxx"



int itkImageFeatureHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
