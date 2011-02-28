/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>
#include <cstdlib>

#include "itkBinaryMorphologicalClosingImageFilter.txx"
#include "itkBinaryMorphologicalOpeningImageFilter.txx"
#include "itkConformalFlatteningMeshFilter.txx"
#include "itkContourExtractor2DImageFilter.txx"
#include "itkFlatStructuringElement.txx"
#include "itkGeometricalQuadEdge.txx"
#include "itkImageToPathFilter.txx"
#include "itkLabelOverlayFunctor.h"
#include "itkLabelOverlayImageFilter.txx"
#include "itkLabelToRGBImageFilter.txx"
#include "itkMorphologicalWatershedFromMarkersImageFilter.txx"
#include "itkMorphologicalWatershedImageFilter.txx"
#include "itkNeuralNetworkFileReader.txx"
#include "itkNeuralNetworkFileWriter.txx"
#include "itkQuadEdge.h"
#include "itkQuadEdgeMesh.txx"
#include "itkQuadEdgeMeshBoundaryEdgesMeshFunction.txx"
#include "itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction.txx"
#include "itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction.txx"
#include "itkQuadEdgeMeshEulerOperatorFlipEdgeFunction.txx"
#include "itkQuadEdgeMeshEulerOperatorJoinFacetFunction.txx"
#include "itkQuadEdgeMeshEulerOperatorJoinVertexFunction.txx"
#include "itkQuadEdgeMeshEulerOperatorSplitEdgeFunction.h"
#include "itkQuadEdgeMeshEulerOperatorSplitFacetFunction.txx"
#include "itkQuadEdgeMeshEulerOperatorSplitVertexFunction.txx"
#include "itkQuadEdgeMeshFrontIterator.txx"
#include "itkQuadEdgeMeshFunctionBase.h"
#include "itkQuadEdgeMeshLineCell.txx"
#include "itkQuadEdgeMeshMacro.h"
#include "itkQuadEdgeMeshPoint.txx"
#include "itkQuadEdgeMeshPolygonCell.txx"
#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.txx"
#include "itkQuadEdgeMeshTopologyChecker.txx"
#include "itkQuadEdgeMeshTraits.h"
#include "itkQuadEdgeMeshZipMeshFunction.txx"
#include "itkRegionalMaximaImageFilter.txx"
#include "itkRegionalMinimaImageFilter.txx"
#include "itkVTKPolyDataReader.txx"
#include "itkVTKPolyDataWriter.txx"
#include "itkValuedRegionalExtremaImageFilter.txx"
#include "itkValuedRegionalMaximaImageFilter.h"
#include "itkValuedRegionalMinimaImageFilter.h"

#include "itkKernelImageFilter.h"
#include "itkMovingHistogramDilateImageFilter.h"
#include "itkMovingHistogramErodeImageFilter.h"
#include "itkMovingHistogramMorphologicalGradientImageFilter.h"
#include "itkBasicDilateImageFilter.h"
#include "itkBasicErodeImageFilter.h"
#include "itkAnchorCloseImageFilter.h"
#include "itkAnchorDilateImageFilter.h"
#include "itkAnchorErodeDilateImageFilter.txx"
#include "itkAnchorErodeDilateLine.txx"
#include "itkAnchorErodeImageFilter.h"
#include "itkAnchorOpenCloseImageFilter.txx"
#include "itkAnchorOpenCloseLine.txx"
#include "itkAnchorOpenImageFilter.h"
#include "itkAnchorUtilities.h"
#include "itkAnchorUtilities.txx"
#include "itkBresenhamLine.txx"
#include "itkSharedMorphologyUtilities.txx"
#include "itkVanHerkGilWermanDilateImageFilter.h"
#include "itkVanHerkGilWermanErodeDilateImageFilter.txx"
#include "itkVanHerkGilWermanErodeImageFilter.h"
#include "itkVanHerkGilWermanUtilities.h"
#include "itkVanHerkGilWermanUtilities.txx"

#include "itkBoxUtilities.h"
#include "itkBoxMeanImageFilter.h"
#include "itkBoxMeanImageFilter.txx"
#include "itkBoxSigmaImageFilter.h"
#include "itkBoxSigmaImageFilter.txx"

#include "itkRankImageFilter.txx"
#include "itkMaskedRankImageFilter.h"
#include "itkMaskedRankImageFilter.txx"
#include "itkMiniPipelineSeparableImageFilter.txx"
#include "itkFastApproximateRankImageFilter.h"

#include "itkBinaryContourImageFilter.h"
#include "itkLabelContourImageFilter.h"

#include "itkFFTShiftImageFilter.h"

#include "itkConvolutionImageFilter.h"

#include "itkHessianToObjectnessMeasureImageFilter.h"
#include "itkMultiScaleHessianBasedMeasureImageFilter.h"


int itkReviewHeaderTest ( int , char ** )
{

  return EXIT_SUCCESS;
}
