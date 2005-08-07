/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBasicFiltersTests1.cxx
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
  REGISTER_TEST(itkNarrowBandTest );
  REGISTER_TEST(itkNarrowBandImageFilterBaseTest );
  REGISTER_TEST(itkNaryAddImageFilterTest );
  REGISTER_TEST(itkNaryMaximumImageFilterTest );
  REGISTER_TEST(itkNeighborhoodConnectedImageFilterTest  );
  REGISTER_TEST(itkNeighborhoodOperatorImageFilterTest );
  REGISTER_TEST(itkNoiseImageFilterTest );
  REGISTER_TEST(itkNonThreadedShrinkImageTest );
  REGISTER_TEST(itkNormalizeImageFilterTest  );
  REGISTER_TEST(itkObjectMorphologyImageFilterTest );
  REGISTER_TEST(itkOrImageFilterTest );
  REGISTER_TEST(itkOrientImageFilterTest);
  REGISTER_TEST(itkOrientImageFilterTest2);
  REGISTER_TEST(itkParallelSparseFieldLevelSetImageFilterTest );
  REGISTER_TEST(itkPasteImageFilterTest );
  REGISTER_TEST(itkPathToChainCodePathFilterTest );
  REGISTER_TEST(itkPathToImageFilterTest );
  REGISTER_TEST(itkPromoteDimensionImageTest );
  REGISTER_TEST(itkPermuteAxesImageFilterTest );
  REGISTER_TEST(itkRGBToVectorAdaptImageFilterTest );
  REGISTER_TEST(itkRGBToLuminanceImageFilterAndAdaptorTest );
  REGISTER_TEST(itkRecursiveGaussianImageFiltersTest );
  REGISTER_TEST(itkRecursiveGaussianImageFiltersOnTensorsTest );
  REGISTER_TEST(itkReflectImageFilterTest );
  REGISTER_TEST(itkReflectiveImageRegionIteratorTest );
  REGISTER_TEST(itkRegionOfInterestImageFilterTest );
  REGISTER_TEST(itkRelabelComponentImageFilterTest );
  REGISTER_TEST(itkRemoveBoundaryObjectsTest );
  REGISTER_TEST(itkRemoveBoundaryObjectsTest2 );
  REGISTER_TEST(itkResampleImageTest );
  REGISTER_TEST(itkResamplePhasedArray3DSpecialCoordinatesImageTest );
  REGISTER_TEST(itkRescaleIntensityImageFilterTest );
}
