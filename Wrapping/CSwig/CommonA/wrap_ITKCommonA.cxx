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
    ITK_WRAP_GROUP(ITKCommonBase),
    ITK_WRAP_GROUP(ITKInterpolators),
    ITK_WRAP_GROUP(ITKRegions),
    ITK_WRAP_GROUP(itkArray),
    ITK_WRAP_GROUP(itkBinaryBallStructuringElement),
    ITK_WRAP_GROUP(itkContinuousIndex),
    ITK_WRAP_GROUP(itkDifferenceImageFilter),
    ITK_WRAP_GROUP(itkDenseFiniteDifferenceImageFilter_2D),
    ITK_WRAP_GROUP(itkDenseFiniteDifferenceImageFilter_3D),
    ITK_WRAP_GROUP(itkEventObjectGroup),
    ITK_WRAP_GROUP(itkFiniteDifferenceFunction),
    ITK_WRAP_GROUP(itkFiniteDifferenceImageFilter_2D),
    ITK_WRAP_GROUP(itkFiniteDifferenceImageFilter_3D),
    ITK_WRAP_GROUP(itkFixedArray),
    ITK_WRAP_GROUP(itkFunctionBase),
    ITK_WRAP_GROUP(itkImage_2D),
    ITK_WRAP_GROUP(itkImage_3D),
    ITK_WRAP_GROUP(itkImageSource),
    ITK_WRAP_GROUP(itkImageConstIterator),
    ITK_WRAP_GROUP(itkImageRegionIterator),
    ITK_WRAP_GROUP(itkImageRegionConstIterator),
    ITK_WRAP_GROUP(itkImageFunction),
    ITK_WRAP_GROUP(itkImageToImageFilter_2D),
    ITK_WRAP_GROUP(itkImageToImageFilter_3D),
    ITK_WRAP_GROUP(itkInPlaceImageFilter_A),
    ITK_WRAP_GROUP(itkInPlaceImageFilter_B),
    ITK_WRAP_GROUP(itkIndex),
    ITK_WRAP_GROUP(itkLevelSet),
    ITK_WRAP_GROUP(itkNeighborhood),
    ITK_WRAP_GROUP(itkPoint),
    ITK_WRAP_GROUP(itkSize),
#ifdef ITK_TCL_WRAP
    ITK_WRAP_GROUP(ITKUtils),
#endif
#ifdef ITK_PYTHON_WRAP
    ITK_WRAP_GROUP(ITKPyUtils),
#ifdef ITK_PYTHON_NUMERICS
    ITK_WRAP_GROUP(itkPyBuffer),
#endif
#endif
    "SwigExtras",
    ITK_WRAP_GROUP(itkVector)
  };
}
#endif
