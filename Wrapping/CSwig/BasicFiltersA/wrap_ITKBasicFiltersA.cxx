/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_ITKBasicFiltersA.cxx
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
    ITK_WRAP_GROUP(itkAnisotropicDiffusionImageFilter_2D),
    ITK_WRAP_GROUP(itkAnisotropicDiffusionImageFilter_3D),
    ITK_WRAP_GROUP(itkBinaryThresholdImageFilter),
    ITK_WRAP_GROUP(itkBinaryDilateImageFilter),
    ITK_WRAP_GROUP(itkBinaryErodeImageFilter),
    ITK_WRAP_GROUP(itkCannyEdgeDetectionImageFilter),
    ITK_WRAP_GROUP(itkCastImageFilter_2D),
    ITK_WRAP_GROUP(itkCastImageFilter_3D),
    ITK_WRAP_GROUP(itkChangeInformationImageFilter),
    ITK_WRAP_GROUP(itkConfidenceConnectedImageFilter),
    ITK_WRAP_GROUP(itkConnectedThresholdImageFilter),
    ITK_WRAP_GROUP(itkCurvatureAnisotropicDiffusionImageFilter),
    ITK_WRAP_GROUP(itkDanielssonDistanceMapImageFilter),
    ITK_WRAP_GROUP(itkExtractImageFilter),
    ITK_WRAP_GROUP(itkFastMarchingImageFilter),
    ITK_WRAP_GROUP(itkFlipImageFilter),
    ITK_WRAP_GROUP(itkGradientAnisotropicDiffusionImageFilter),
    ITK_WRAP_GROUP(itkGradientMagnitudeImageFilter),
    ITK_WRAP_GROUP(itkGrayscaleDilateImageFilter),
    ITK_WRAP_GROUP(itkGrayscaleErodeImageFilter),
    ITK_WRAP_GROUP(itkIsolatedConnectedImageFilter),
    ITK_WRAP_GROUP(itkImportImageFilter),
    ITK_WRAP_GROUP(itkLaplacianImageFilter),
    ITK_WRAP_GROUP(itkMinimumMaximumImageCalculator),
    ITK_WRAP_GROUP(itkNeighborhoodConnectedImageFilter),
    ITK_WRAP_GROUP(itkSobelEdgeDetectionImageFilter),
    ITK_WRAP_GROUP(itkTernaryMagnitudeImageFilter)
  };
}
#endif
