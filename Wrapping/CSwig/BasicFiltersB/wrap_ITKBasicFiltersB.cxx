/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_ITKBasicFiltersB.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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



