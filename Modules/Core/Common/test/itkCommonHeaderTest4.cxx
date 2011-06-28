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

#include "itkQuadraticEdgeCell.hxx"
#include "itkQuadraticTriangleCell.hxx"
#include "itkQuadraticTriangleCellTopology.h"
#include "itkQuadrilateralCell.hxx"
#include "itkQuadrilateralCellTopology.h"
#include "itkRealTimeClock.h"
#include "itkRealTimeInterval.h"
#include "itkRealTimeStamp.h"
#include "itkRegion.h"
#include "itkResourceProbe.hxx"
#include "itkResourceProbesCollectorBase.hxx"
#include "itkRGBAPixel.hxx"
#include "itkRGBPixel.hxx"
#include "itkRootTreeIterator.h"
#include "itkShapedNeighborhoodIterator.hxx"
#include "itkSimpleDataObjectDecorator.hxx"
#include "itkSimpleFastMutexLock.h"
#include "itkSimpleFilterWatcher.h"
#include "itkSize.h"
#include "itkSliceIterator.h"
#include "itkSmapsFileParser.hxx"
#include "itkSmartPointer.h"
#include "itkSmartPointerForwardReference.hxx"
#include "itkSobelOperator.hxx"
#include "itkSparseFieldLayer.hxx"
#include "itkSparseImage.hxx"
#include "itkSpatialFunction.hxx"
#include "itkSpatialOrientation.h"
#include "itkSpatialOrientationAdapter.h"
#include "itkSpecialCoordinatesImage.hxx"
#include "itkSphereSpatialFunction.hxx"
#include "itkStdStreamLogOutput.h"
#include "itkSTLConstContainerAdaptor.h"
#include "itkSTLContainerAdaptor.h"
#include "itkStreamingImageFilter.hxx"
#include "itkStructHashFunction.h"
#include "itkSymmetricEigenAnalysis.hxx"
#include "itkSymmetricEllipsoidInteriorExteriorSpatialFunction.hxx"
#include "itkSymmetricSecondRankTensor.hxx"
#include "itkTestingMacros.h"
#include "itkTetrahedronCell.hxx"
#include "itkTetrahedronCellTopology.h"
#include "itkTextOutput.h"
#include "itkThreadLogger.h"
#include "itkThreadSupport.h"
#include "itkTimeProbe.h"
#include "itkTimeProbesCollectorBase.h"
#include "itkTimeStamp.h"
#include "itkTorusInteriorExteriorSpatialFunction.hxx"
#include "itkTreeChangeEvent.h"
#include "itkTreeContainer.hxx"
#include "itkTreeContainerBase.h"
#include "itkTreeIteratorBase.hxx"
#include "itkTreeIteratorClone.h"
#include "itkTreeNode.hxx"
#include "itkTriangleCell.hxx"
#include "itkTriangleCellTopology.h"
#include "itkTriangleHelper.hxx"

int itkCommonHeaderTest4 ( int , char * [] )
{

  return EXIT_SUCCESS;
}
