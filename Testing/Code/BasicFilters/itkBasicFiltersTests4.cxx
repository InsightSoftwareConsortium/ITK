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
  REGISTER_TEST( itkParametricSpaceToImageSpaceMeshFilterTest );
  REGISTER_TEST( itkGrayscaleGeodesicDilateImageFilterTest );
  REGISTER_TEST( itkGrayscaleGeodesicErodeImageFilterTest );
  REGISTER_TEST( itkGetAverageSliceImageFilterTest );
  REGISTER_TEST( itkClosingByReconstructionImageFilterTest );
  REGISTER_TEST( itkOpeningByReconstructionImageFilterTest );
  REGISTER_TEST( itkWhiteTopHatImageFilterTest );
  REGISTER_TEST( itkBlackTopHatImageFilterTest );
}

