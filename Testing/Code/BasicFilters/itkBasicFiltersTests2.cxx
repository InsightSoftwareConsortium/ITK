/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBasicFiltersTests2.cxx
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
  REGISTER_TEST(itkBasicFiltersPrintTest );
  REGISTER_TEST(itkBasicFiltersPrintTest2 );
  REGISTER_TEST(itkComplexToImaginaryFilterAndAdaptorTest );
  REGISTER_TEST(itkComplexToRealFilterAndAdaptorTest );
  REGISTER_TEST(itkComplexToModulusFilterAndAdaptorTest );
  REGISTER_TEST(itkComplexToPhaseFilterAndAdaptorTest );
  REGISTER_TEST(itkConstrainedValueAdditionImageFilterTest );
  REGISTER_TEST(itkConstrainedValueDifferenceImageFilterTest );
  REGISTER_TEST(itkContourDirectedMeanDistanceImageFilterTest );
  REGISTER_TEST(itkContourMeanDistanceImageFilterTest );
  REGISTER_TEST(itkDeformationFieldJacobianDeterminantFilterTest );
  REGISTER_TEST(itkGrayscaleMorphologicalClosingImageFilterTest );
  REGISTER_TEST(itkGrayscaleMorphologicalOpeningImageFilterTest );
  REGISTER_TEST(itkJoinSeriesImageFilterPrintTest);
  REGISTER_TEST(itkJoinSeriesImageFilterTest );
  REGISTER_TEST(itkShiftScaleImageFilterTest );
  REGISTER_TEST(itkShiftScaleInPlaceImageFilterTest );
  REGISTER_TEST(itkShrinkImageTest );
  REGISTER_TEST(itkSigmoidImageFilterTest );
  REGISTER_TEST(itkSignedDanielssonDistanceMapImageFilterTest );
  REGISTER_TEST(itkSimilarityIndexImageFilterTest );
  REGISTER_TEST(itkSimpleContourExtractorImageFilterTest );
  REGISTER_TEST(itkSimplexMeshAdaptTopologyFilterTest);
  REGISTER_TEST(itkSimplexMeshToTriangleMeshFilterTest);
  REGISTER_TEST(itkSinImageFilterAndAdaptorTest );
  REGISTER_TEST(itkSmoothingRecursiveGaussianImageFilterTest );
  REGISTER_TEST(itkSobelEdgeDetectionImageFilterTest );
  REGISTER_TEST(itkSparseFieldFourthOrderLevelSetImageFilterTest );
  REGISTER_TEST(itkSparseFieldLayerTest);
  REGISTER_TEST(itkSpatialFunctionImageEvaluatorFilterTest );
  REGISTER_TEST(itkSpatialObjectToImageFilterTest );
  REGISTER_TEST(itkSpatialObjectToImageStatisticsCalculatorTest );
  REGISTER_TEST(itkSpatialObjectToPointSetFilterTest );
  REGISTER_TEST(itkSqrtImageFilterAndAdaptorTest );
  REGISTER_TEST(itkSquareImageFilterTest );
  REGISTER_TEST(itkSquaredDifferenceImageFilterTest );
  REGISTER_TEST(itkStatisticsImageFilterTest );
  REGISTER_TEST(itkStreamingImageFilterTest );
  REGISTER_TEST(itkStreamingImageFilterTest2 );
  REGISTER_TEST(itkSubtractImageFilterTest );
  REGISTER_TEST(itkTanImageFilterAndAdaptorTest );
  REGISTER_TEST(itkTernaryMagnitudeImageFilterTest );
  REGISTER_TEST(itkThresholdImageFilterTest );
  REGISTER_TEST(itkThresholdLabelerImageFilterTest );
  REGISTER_TEST(itkTileImageFilterTest );
  REGISTER_TEST(itkTobogganImageFilterTest );
  REGISTER_TEST(itkTopHatImageFilterTest );
  REGISTER_TEST(itkTransformMeshFilterTest );
  REGISTER_TEST(itkTriangleMeshToSimplexMeshFilter2Test);
  REGISTER_TEST(itkTriangleMeshToSimplexMeshFilterTest);
  REGISTER_TEST(itkTwoOutputExampleImageFilterTest );
}

