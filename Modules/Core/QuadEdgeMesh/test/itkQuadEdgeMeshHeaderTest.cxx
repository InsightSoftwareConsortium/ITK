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

#include "itkGeometricalQuadEdge.txx"
#include "itkQuadEdge.h"
#include "itkQuadEdgeCellTraitsInfo.h"
#include "itkQuadEdgeMesh.txx"
#include "itkQuadEdgeMeshBaseIterator.h"
#include "itkQuadEdgeMeshBoundaryEdgesMeshFunction.txx"
#include "itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction.txx"
#include "itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction.txx"
#include "itkQuadEdgeMeshEulerOperatorFlipEdgeFunction.txx"
#include "itkQuadEdgeMeshEulerOperatorJoinFacetFunction.txx"
#include "itkQuadEdgeMeshEulerOperatorJoinVertexFunction.txx"
#include "itkQuadEdgeMeshEulerOperatorSplitEdgeFunction.h"
#include "itkQuadEdgeMeshEulerOperatorSplitFacetFunction.txx"
#include "itkQuadEdgeMeshEulerOperatorSplitVertexFunction.txx"
#include "itkQuadEdgeMeshExtendedTraits.h"
#include "itkQuadEdgeMeshFrontIterator.txx"
#include "itkQuadEdgeMeshFunctionBase.h"
#include "itkQuadEdgeMeshLineCell.txx"
#include "itkQuadEdgeMeshMacro.h"
#include "itkQuadEdgeMeshPoint.txx"
#include "itkQuadEdgeMeshPolygonCell.txx"
#include "itkQuadEdgeMeshScalarDataVTKPolyDataWriter.txx"
#include "itkQuadEdgeMeshTopologyChecker.txx"
#include "itkQuadEdgeMeshTraits.h"
#include "itkQuadEdgeMeshZipMeshFunction.txx"
#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.h"
#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.txx"


int itkQuadEdgeMeshHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
