/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBasicFiltersTests4.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

// this file defines the itkBasicFiltersTest for the test driver
// and all it expects is that you have a function called RegisterTests
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST( itkClosingByReconstructionImageFilterTest );
  REGISTER_TEST( itkGetAverageSliceImageFilterTest );
  REGISTER_TEST( itkGrayscaleGeodesicErodeDilateImageFilterTest );
  REGISTER_TEST( itkHessian3DToVesselnessMeasureImageFilterTest );
  REGISTER_TEST( itkInvertIntensityImageFilterTest );
  REGISTER_TEST( itkMaskNeighborhoodOperatorImageFilterTest );
  REGISTER_TEST( itkMatrixIndexSelectionImageFilterTest );
  REGISTER_TEST( itkModulusImageFilterTest );
  REGISTER_TEST( itkMorphologicalGradientImageFilterTest );
  REGISTER_TEST( itkNotImageFilterTest );
  REGISTER_TEST( itkOpeningByReconstructionImageFilterTest );
  REGISTER_TEST( itkParametricSpaceToImageSpaceMeshFilterTest );
  REGISTER_TEST( itkSignedMaurerDistanceMapImageFilterTest );
  REGISTER_TEST( itkVectorIndexSelectionCastImageFilterTest );
  REGISTER_TEST( itkVectorConnectedComponentImageFilterTest );
}

