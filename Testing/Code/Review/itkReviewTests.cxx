/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReviewTests.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
// this file defines the itkBasicFiltersTest for the test driver
// and all it expects is that you have a function called RegisterTests

#include "vnl/vnl_sample.h"
#include "itkTestMain.h"

void RegisterTests()
{
  vnl_sample_reseed(8775070);
  REGISTER_TEST(itkChiSquareDistributionTest);
  REGISTER_TEST(itkGaussianDistributionTest);
  REGISTER_TEST(itkTDistributionTest);
  REGISTER_TEST(itkFlatStructuringElementTest);
  REGISTER_TEST(itkLabelOverlayImageFilterTest);

  REGISTER_TEST(itkQuadEdgeTest1);
  REGISTER_TEST(itkQuadEdgeMeshAddFaceTest1);
  REGISTER_TEST(itkQuadEdgeMeshAddFaceTest2);
  REGISTER_TEST(itkQuadEdgeMeshBasicLayerTest);
  REGISTER_TEST(itkQuadEdgeMeshDeleteEdgeTest);
  REGISTER_TEST(itkQuadEdgeMeshFrontIteratorTest);
  REGISTER_TEST(itkQuadEdgeMeshPointTest1);
  REGISTER_TEST(itkQuadEdgeMeshTest1);
  REGISTER_TEST(itkQuadEdgeMeshTest2);
  REGISTER_TEST(itkQuadEdgeMeshTest3);

  REGISTER_TEST(itkContourExtractor2DImageFilterTest);
 
  REGISTER_TEST(itkSumProjectionImageFilterTest);
  REGISTER_TEST(itkMaximumProjectionImageFilterTest);
  REGISTER_TEST(itkMaximumProjectionImageFilterTest2);
  REGISTER_TEST(itkMaximumProjectionImageFilterTest3);
  REGISTER_TEST(itkMeanProjectionImageFilterTest);
  REGISTER_TEST(itkMedianProjectionImageFilterTest);
  REGISTER_TEST(itkMinimumProjectionImageFilterTest);
  REGISTER_TEST(itkStandardDeviationProjectionImageFilterTest);
  REGISTER_TEST(itkBinaryProjectionImageFilterTest);
  REGISTER_TEST(itkProjectionImageFilterTest);

  REGISTER_TEST(itkValuedRegionalMinimaImageFilterTest);
  REGISTER_TEST(itkValuedRegionalMaximaImageFilterTest);
  REGISTER_TEST(itkRegionalMaximaImageFilterTest);
  REGISTER_TEST(itkRegionalMaximaImageFilterTest2);
  REGISTER_TEST(itkRegionalMinimaImageFilterTest);
  REGISTER_TEST(itkRegionalMinimaImageFilterTest2);

  REGISTER_TEST(itkNeuralNetworkIOTest);
 
  REGISTER_TEST(itkConformalFlatteningMeshFilterTest);

  REGISTER_TEST(itkVTKPolyDataReaderTest);
}
