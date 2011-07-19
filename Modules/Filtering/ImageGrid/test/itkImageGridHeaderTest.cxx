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

#include "itkInterpolateImageFilter.hxx"
#include "itkPadImageFilter.hxx"
#include "itkConstantPadImageFilter.hxx"
#include "itkPasteImageFilter.hxx"
#include "itkBSplineCenteredL2ResampleImageFilterBase.h"
#include "itkShrinkImageFilter.hxx"
#include "itkWarpImageFilter.hxx"
#include "itkFlipImageFilter.hxx"
#include "itkBSplineUpsampleImageFilter.hxx"
#include "itkResampleImageFilter.hxx"
#include "itkBSplineDecompositionImageFilter.hxx"
#include "itkExpandImageFilter.hxx"
#include "itkCropImageFilter.h"
#include "itkWarpVectorImageFilter.hxx"
#include "itkOrientImageFilter.hxx"
#include "itkBSplineCenteredResampleImageFilterBase.hxx"
#include "itkInterpolateImagePointsFilter.hxx"
#include "itkRegionOfInterestImageFilter.hxx"
#include "itkBSplineResampleImageFilterBase.hxx"
#include "itkMirrorPadImageFilter.hxx"
#include "itkVectorResampleImageFilter.hxx"
#include "itkWrapPadImageFilter.hxx"
#include "itkBSplineDecompositionImageFilter.h"
#include "itkTileImageFilter.hxx"
#include "itkBSplineCenteredL2ResampleImageFilterBase.hxx"
#include "itkRegionOfInterestImageFilter.h"
#include "itkPermuteAxesImageFilter.hxx"
#include "itkBSplineCenteredResampleImageFilterBase.h"
#include "itkChangeInformationImageFilter.hxx"
#include "itkCropImageFilter.hxx"
#include "itkBSplineL2ResampleImageFilterBase.hxx"
#include "itkVectorResampleImageFilter.h"
#include "itkBSplineDownsampleImageFilter.hxx"
#include "itkZeroFluxNeumannPadImageFilter.hxx"
#include "itkSliceBySliceImageFilter.hxx"

int itkImageGridHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
