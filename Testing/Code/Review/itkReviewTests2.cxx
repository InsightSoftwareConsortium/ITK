/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReviewTests2.cxx
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
#include "itkConfigure.h"

void RegisterTests()
{
  vnl_sample_reseed(8775070);

  // NOTE: Please do not add more tests to this test driver. The Borland
  // compiler sometimes has problems linking large executables. Add new
  // tests to itkReviewTests3.cxx
  //
  REGISTER_TEST(itkDirectFourierReconstructionImageToImageFilterTest);
  REGISTER_TEST(itkBSplineDeformableTransformInitializerTest1);
  REGISTER_TEST(itkBSplineDeformableTransformInitializerTest2);
  REGISTER_TEST(itkHeavisideStepFunctionTest1);
  REGISTER_TEST(itkLabelImageToLabelMapFilterTest);
  REGISTER_TEST(itkLabelMapFilterTest);
  REGISTER_TEST(itkLabelMapTest);
  REGISTER_TEST(itkLabelMapToLabelImageFilterTest);
  REGISTER_TEST(itkLabelObjectLineComparatorTest);
  REGISTER_TEST(itkLabelObjectLineTest);
  REGISTER_TEST(itkLabelObjectTest);
  REGISTER_TEST(itkMultiphaseFiniteDifferenceImageFilterTest);
  REGISTER_TEST(itkMultiphaseDenseFiniteDifferenceImageFilterTest);
  REGISTER_TEST(itkMultiphaseSparseFiniteDifferenceImageFilterTest);
  REGISTER_TEST(itkQuadEdgeMeshCleanFilterTest);
  REGISTER_TEST(itkQuadEdgem_NoPointTest);
  REGISTER_TEST(itkQuadEdgeTest1);
  REGISTER_TEST(itkGeometricalQuadEdgeTest1);
  REGISTER_TEST(itkQuadEdgeMeshAddFaceTest1);
  REGISTER_TEST(itkQuadEdgeMeshAddFaceTest2);
  REGISTER_TEST(itkQuadEdgeMeshBasicLayerTest);
  REGISTER_TEST(itkQuadEdgeMeshDeleteEdgeTest);
  REGISTER_TEST(itkQuadEdgeMeshFrontIteratorTest);
  REGISTER_TEST(itkQuadEdgeMeshIteratorTest);
  REGISTER_TEST(itkQuadEdgeMeshPointTest1);
  REGISTER_TEST(itkQuadEdgeMeshTest1);
  REGISTER_TEST(itkQuadEdgeMeshTest2);
  REGISTER_TEST(itkQuadEdgeMeshTest3);
  REGISTER_TEST(itkDynamicQuadEdgeMeshTest);
  REGISTER_TEST(itkQuadEdgeMeshPolygonCellTest);
  REGISTER_TEST(itkQuadEdgeMeshCellInterfaceTest);
  REGISTER_TEST(itkQuadEdgeMeshEulerOperatorCreateCenterVertexTest);
  REGISTER_TEST(itkQuadEdgeMeshEulerOperatorDeleteCenterVertexTest);
  REGISTER_TEST(itkQuadEdgeMeshEulerOperatorFlipTest);
  REGISTER_TEST(itkQuadEdgeMeshEulerOperatorJoinFacetTest);
  REGISTER_TEST(itkQuadEdgeMeshEulerOperatorJoinVertexTest);
  REGISTER_TEST(itkQuadEdgeMeshEulerOperatorSplitEdgeTest);
  REGISTER_TEST(itkQuadEdgeMeshEulerOperatorSplitFaceTest);
  REGISTER_TEST(itkQuadEdgeMeshEulerOperatorSplitVertexTest);
  REGISTER_TEST(itkAutomaticTopologyQuadEdgeMeshSourceTest);
  REGISTER_TEST(itkRegularSphereQuadEdgeMeshSourceTest);
  REGISTER_TEST(itkQuadEdgeMeshCountingCellsTest);
  REGISTER_TEST(itkBinaryMask3DQuadEdgeMeshSourceTest);
  REGISTER_TEST(itkContourExtractor2DImageFilterTest);
  REGISTER_TEST(itkQuadEdgeMeshDeletePointAndReorderIDsTest);
  REGISTER_TEST(itkTriangleHelperTest);
  REGISTER_TEST(itkCrossHelperTest);
  REGISTER_TEST(itkQuadEdgeMeshLinearParameterizationTest);
  REGISTER_TEST(itkPriorityQueueTest);
  REGISTER_TEST(itkQuadEdgeMeshSquaredEdgeLengthDecimationTest);
  REGISTER_TEST(itkQuadEdgeMeshQuadricDecimationTest);
  REGISTER_TEST(itkQuadEdgeMeshGaussianCurvatureTest);
  REGISTER_TEST(itkQuadEdgeMeshMeanCurvatureTest);
  REGISTER_TEST(itkQuadEdgeMeshMaxCurvatureTest);
  REGISTER_TEST(itkQuadEdgeMeshMinCurvatureTest);
  REGISTER_TEST(itkQuadEdgeMeshDelaunayConformingFilterTest);
  REGISTER_TEST(itkQuadEdgeMeshNormalFilterTest);
  REGISTER_TEST(itkQuadEdgeMeshSmoothingTest);
  REGISTER_TEST(itkQuadEdgeMeshScalarDataVTKPolyDataWriterTest1);
  REGISTER_TEST(itkAtanRegularizedHeavisideStepFunctionTest1);
  REGISTER_TEST(itkRegionBasedLevelSetFunctionTest);
  REGISTER_TEST(itkScalarRegionBasedLevelSetFunctionTest);
  REGISTER_TEST(itkScalarToRGBColormapImageFilterTest);
  REGISTER_TEST(itkStochasticFractalDimensionImageFilterTest);
  REGISTER_TEST(itkSinRegularizedHeavisideStepFunctionTest1);

  // NOTE: Please do not add more tests to this test driver. The Borland
  // compiler sometimes has problems linking large executables. Add new
  // tests to itkReviewTests3.cxx
  //
}
