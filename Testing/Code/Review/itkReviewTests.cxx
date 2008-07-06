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
#include "itkConfigure.h"

void RegisterTests()
{
  vnl_sample_reseed(8775070);
  REGISTER_TEST(itkFlatStructuringElementTest);

  REGISTER_TEST(itkLabelToRGBImageFilterTest);
  REGISTER_TEST(itkLabelOverlayImageFilterTest);

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
  REGISTER_TEST(itkQuadEdgeMeshIteratorTest);
  REGISTER_TEST(itkDynamicQuadEdgeMeshTest);
  REGISTER_TEST(itkQuadEdgeMeshPolygonCellTest);
  REGISTER_TEST(itkQuadEdgeMeshCellInterfaceTest);
  // REGISTER_TEST(itkQuadEdgeMeshEulerOperatorsTest);
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

  REGISTER_TEST(itkValuedRegionalMinimaImageFilterTest);
  REGISTER_TEST(itkValuedRegionalMaximaImageFilterTest);
  REGISTER_TEST(itkRegionalMaximaImageFilterTest);
  REGISTER_TEST(itkRegionalMaximaImageFilterTest2);
  REGISTER_TEST(itkRegionalMinimaImageFilterTest);
  REGISTER_TEST(itkRegionalMinimaImageFilterTest2);

  REGISTER_TEST(itkNeuralNetworkIOTest);

  REGISTER_TEST(itkConformalFlatteningMeshFilterTest);
  REGISTER_TEST(itkConformalFlatteningQuadEdgeMeshFilterTest);

  REGISTER_TEST(itkVTKPolyDataIOQuadEdgeMeshTest);
  REGISTER_TEST(itkVTKPolyDataReaderQuadEdgeMeshTest);
  REGISTER_TEST(itkVTKPolyDataReaderTest);
  REGISTER_TEST(itkVTKPolyDataWriterTest);

  REGISTER_TEST(itkMorphologicalWatershedImageFilterTest);
  REGISTER_TEST(itkMorphologicalWatershedFromMarkersImageFilterTest);

  REGISTER_TEST(itkBinaryMorphologicalClosingImageFilterTest);
  REGISTER_TEST(itkBinaryMorphologicalOpeningImageFilterTest);

  REGISTER_TEST(itkOptImageToImageMetricsTest);
  REGISTER_TEST(itkTimeAndMemoryProbeTest);

  REGISTER_TEST(itkBruker2DSEQImageIOTest);
  REGISTER_TEST(itkPhilipsRECImageIOTest);

  REGISTER_TEST(itkVoxBoCUBImageIOTest);

  REGISTER_TEST(itkSliceBySliceImageFilterTest);

  REGISTER_TEST(itkDiffeomorphicDemonsRegistrationFilterTest);
  REGISTER_TEST(itkDiffeomorphicDemonsRegistrationFilterTest2);

  REGISTER_TEST(itkDivideByConstantImageFilterTest);
  REGISTER_TEST(itkMultiplyByConstantImageFilterTest);
  REGISTER_TEST(itkAddConstantToImageFilterTest);
  REGISTER_TEST(itkSubtractConstantFromImageFilterTest);

  REGISTER_TEST( itkImageReadComplexWriteMagnitudeAndPhaseTest );
  REGISTER_TEST( itkImageReadMagnitudeAndPhaseWriteComplexTest );
  REGISTER_TEST( itkImageReadRealAndImaginaryWriteComplexTest );

  REGISTER_TEST( itkFFTComplexToComplexImageFilterTest01 );
  REGISTER_TEST( itkFFTComplexToComplexImageFilterTest02 );

  REGISTER_TEST( itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunctionTest );
  REGISTER_TEST( itkExponentialDeformationFieldImageFilterTest );
  REGISTER_TEST( itkFastSymmetricForcesDemonsRegistrationFilterTest );
  REGISTER_TEST( itkGridForwardWarpImageFilterTest );
  REGISTER_TEST( itkWarpJacobianDeterminantFilterTest );
  REGISTER_TEST( itkWarpHarmonicEnergyCalculatorTest );
  
  REGISTER_TEST( itkBSplineScatteredDataPointSetToImageFilterTest );
 
  REGISTER_TEST( itkKappaSigmaThresholdImageCalculatorTest );
  REGISTER_TEST( itkKappaSigmaThresholdImageFilterTest );

#ifdef ITK_USE_MINC2
  REGISTER_TEST(itkMINC2ImageIOTest);
#endif

#ifdef ITK_USE_TRANSFORM_IO_FACTORIES
  REGISTER_TEST(itkTransformFileReaderWriterTest);
#endif

}
