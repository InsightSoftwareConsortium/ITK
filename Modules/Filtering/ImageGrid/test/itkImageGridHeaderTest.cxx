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

#include "itkInterpolateImageFilter.txx"
#include "itkPadImageFilter.txx"
#include "itkConstantPadImageFilter.txx"
#include "itkPasteImageFilter.txx"
#include "itkBSplineCenteredL2ResampleImageFilterBase.h"
#include "itkShrinkImageFilter.txx"
#include "itkWarpImageFilter.txx"
#include "itkFlipImageFilter.txx"
#include "itkBSplineUpsampleImageFilter.txx"
#include "itkResampleImageFilter.txx"
#include "itkBSplineDecompositionImageFilter.txx"
#include "itkExpandImageFilter.txx"
#include "itkCropImageFilter.h"
#include "itkWarpVectorImageFilter.txx"
#include "itkOrientImageFilter.txx"
#include "itkBSplineCenteredResampleImageFilterBase.txx"
#include "itkInterpolateImagePointsFilter.txx"
#include "itkRegionOfInterestImageFilter.txx"
#include "itkBSplineResampleImageFilterBase.txx"
#include "itkMirrorPadImageFilter.txx"
#include "itkVectorResampleImageFilter.txx"
#include "itkWrapPadImageFilter.txx"
#include "itkBSplineDecompositionImageFilter.h"
#include "itkTileImageFilter.txx"
#include "itkBSplineCenteredL2ResampleImageFilterBase.txx"
#include "itkRegionOfInterestImageFilter.h"
#include "itkPermuteAxesImageFilter.txx"
#include "itkBSplineCenteredResampleImageFilterBase.h"
#include "itkChangeInformationImageFilter.txx"
#include "itkCropImageFilter.txx"
#include "itkBSplineL2ResampleImageFilterBase.txx"
#include "itkVectorResampleImageFilter.h"
#include "itkBSplineDownsampleImageFilter.txx"
#include "itkZeroFluxNeumannPadImageFilter.txx"


int itkImageGridHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
