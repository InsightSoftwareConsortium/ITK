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
    ITK_WRAP_GROUP(itkExpImageFilter),
    ITK_WRAP_GROUP(itkExpNegativeImageFilter),
    ITK_WRAP_GROUP(itkGradientMagnitudeRecursiveGaussianImageFilter),
    ITK_WRAP_GROUP(itkGradientRecursiveGaussianImageFilter),
    ITK_WRAP_GROUP(itkMeanImageFilter),
    ITK_WRAP_GROUP(itkMedianImageFilter),
    ITK_WRAP_GROUP(itkMinimumMaximumImageFilter),
    ITK_WRAP_GROUP(itkNaryAddImageFilter),
    ITK_WRAP_GROUP(itkNormalizeImageFilter),
    ITK_WRAP_GROUP(itkPermuteAxesImageFilter),
    ITK_WRAP_GROUP(itkRandomImageSource),
    ITK_WRAP_GROUP(itkRecursiveGaussianImageFilter),
    ITK_WRAP_GROUP(itkRecursiveSeparableImageFilter),
    ITK_WRAP_GROUP(itkRegionOfInterestImageFilter),
    ITK_WRAP_GROUP(itkResampleImageFilter),
    ITK_WRAP_GROUP(itkRescaleIntensityImageFilter),
    ITK_WRAP_GROUP(itkShiftScaleImageFilter),
    ITK_WRAP_GROUP(itkSigmoidImageFilter),
    ITK_WRAP_GROUP(itkSmoothingRecursiveGaussianImageFilter),
    ITK_WRAP_GROUP(itkStatisticsImageFilter),
    ITK_WRAP_GROUP(itkSubtractImageFilter),
    ITK_WRAP_GROUP(itkThresholdImageFilter),
    ITK_WRAP_GROUP(itkVTKImageExport),
    ITK_WRAP_GROUP(itkVTKImageImport)
  };
}
#endif



