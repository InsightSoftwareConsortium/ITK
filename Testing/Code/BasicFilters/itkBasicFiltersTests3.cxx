/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBasicFiltersTests3.cxx
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
  REGISTER_TEST(itkCheckerBoardImageFilterTest );
  REGISTER_TEST(itkHessianRecursiveGaussianFilterTest );
  REGISTER_TEST(itkNormalizedCorrelationImageFilterTest );
  REGISTER_TEST(itkTensorFractionalAnisotropyImageFilterTest );
  REGISTER_TEST(itkTensorRelativeAnisotropyImageFilterTest );
  REGISTER_TEST(itkVectorAnisotropicDiffusionImageFilterTest );
  REGISTER_TEST(itkVectorConfidenceConnectedImageFilterTest );
  REGISTER_TEST(itkVectorExpandImageFilterTest );
  REGISTER_TEST(itkVectorGradientMagnitudeImageFilterTest1 );
  REGISTER_TEST(itkVectorGradientMagnitudeImageFilterTest2 );
  REGISTER_TEST(itkVectorNeighborhoodOperatorImageFilterTest );
  REGISTER_TEST(itkVectorResampleImageFilterTest );
  REGISTER_TEST(itkVectorRescaleIntensityImageFilterTest );
  REGISTER_TEST(itkVotingBinaryHoleFillingImageFilterTest );
  REGISTER_TEST(itkVotingBinaryImageFilterTest );
  REGISTER_TEST(itkVotingBinaryIterativeHoleFillingImageFilterTest );
  REGISTER_TEST(itkWarpImageFilterTest );
  REGISTER_TEST(itkWarpMeshFilterTest );
  REGISTER_TEST(itkWarpVectorImageFilterTest );
  REGISTER_TEST(itkWeightedAddImageFilterTest);
  REGISTER_TEST(itkWrapPadImageTest );
  REGISTER_TEST(itkXorImageFilterTest );
  REGISTER_TEST(itkZeroCrossingBasedEdgeDetectionImageFilterTest );
  REGISTER_TEST(itkZeroCrossingImageFilterTest );
}

