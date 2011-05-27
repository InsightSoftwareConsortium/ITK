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

#include "itkAtanImageFilter.h"
#include "itkPolylineMaskImageFilter.txx"
#include "itkAddImageFilter.h"
#include "itkSymmetricEigenAnalysisImageFilter.h"
#include "itkBinaryMagnitudeImageFilter.h"
#include "itkVectorRescaleIntensityImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkLog10ImageFilter.h"
#include "itkAtan2ImageFilter.h"
#include "itkComplexToModulusImageFilter.h"
#include "itkConstrainedValueAdditionImageFilter.h"
#include "itkTernaryMagnitudeImageFilter.h"
#include "itkNormalizeImageFilter.txx"
#include "itkRescaleIntensityImageFilter.txx"
#include "itkTernaryAddImageFilter.h"
#include "itkPolylineMask2DImageFilter.txx"
#include "itkMaximumImageFilter.h"
#include "itkRGBToLuminanceImageFilter.h"
#include "itkVectorExpandImageFilter.txx"
#include "itkSinImageFilter.h"
#include "itkHistogramMatchingImageFilter.txx"
#include "itkCosImageFilter.h"
#include "itkComplexToImaginaryImageFilter.h"
#include "itkSigmoidImageFilter.h"
#include "itkDivideImageFilter.h"
#include "itkNotImageFilter.h"
#include "itkInvertIntensityImageFilter.txx"
#include "itkMinimumImageFilter.h"
#include "itkLogImageFilter.h"
#include "itkAdaptImageFilter.h"
#include "itkExpNegativeImageFilter.h"
#include "itkMaskImageFilter.h"
#include "itkSquareImageFilter.h"
#include "itkMatrixIndexSelectionImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkNaryMaximumImageFilter.h"
#include "itkAsinImageFilter.h"
#include "itkTernaryMagnitudeSquaredImageFilter.h"
#include "itkVectorIndexSelectionCastImageFilter.h"
#include "itkAcosImageFilter.h"
#include "itkComplexToPhaseImageFilter.h"
#include "itkWeightedAddImageFilter.h"
#include "itkMaskNegatedImageFilter.h"
#include "itkAndImageFilter.h"
#include "itkAbsImageFilter.h"
#include "itkEdgePotentialImageFilter.h"
#include "itkNaryAddImageFilter.h"
#include "itkTanImageFilter.h"
#include "itkExpImageFilter.h"
#include "itkXorImageFilter.h"
#include "itkConstrainedValueDifferenceImageFilter.h"
#include "itkModulusImageFilter.txx"
#include "itkVectorRescaleIntensityImageFilter.txx"
#include "itkNaryFunctorImageFilter.txx"
#include "itkOrImageFilter.h"
#include "itkIntensityWindowingImageFilter.txx"
#include "itkComplexToRealImageFilter.h"
#include "itkBoundedReciprocalImageFilter.h"
#include "itkShiftScaleImageFilter.txx"
#include "itkSqrtImageFilter.h"



int itkImageIntensityHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
