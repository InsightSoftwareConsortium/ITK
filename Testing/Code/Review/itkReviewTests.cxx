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
  REGISTER_TEST(itkQuadEdgeMeshEulerOperatorsTest);
  REGISTER_TEST(itkAutomaticTopologyQuadEdgeMeshSourceTest);
  REGISTER_TEST(itkRegularSphereQuadEdgeMeshSourceTest);

  REGISTER_TEST(itkContourExtractor2DImageFilterTest);
 
  REGISTER_TEST(itkValuedRegionalMinimaImageFilterTest);
  REGISTER_TEST(itkValuedRegionalMaximaImageFilterTest);
  REGISTER_TEST(itkRegionalMaximaImageFilterTest);
  REGISTER_TEST(itkRegionalMaximaImageFilterTest2);
  REGISTER_TEST(itkRegionalMinimaImageFilterTest);
  REGISTER_TEST(itkRegionalMinimaImageFilterTest2);

  REGISTER_TEST(itkNeuralNetworkIOTest);
 
  REGISTER_TEST(itkConformalFlatteningMeshFilterTest);

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

#ifdef ITK_USE_MINC2
  REGISTER_TEST(itkMINC2ImageIOTest);
#endif 

#ifdef ITK_USE_TRANSFORM_IO_FACTORIES
  REGISTER_TEST(itkTransformFileReaderWriterTest);
#endif
}
