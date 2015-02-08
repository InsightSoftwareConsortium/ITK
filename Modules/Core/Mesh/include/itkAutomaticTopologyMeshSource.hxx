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
#ifndef itkAutomaticTopologyMeshSource_hxx
#define itkAutomaticTopologyMeshSource_hxx

// For debugging.
#include <iostream>
#include <algorithm>

#include "itkAutomaticTopologyMeshSource.h"
#include "itkNumericTraits.h"

namespace itk
{
template< typename TOutputMesh >
AutomaticTopologyMeshSource< TOutputMesh >
::AutomaticTopologyMeshSource()
{
  m_OutputMesh = TOutputMesh::New();

  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput( 0, m_OutputMesh.GetPointer() );

  this->ReleaseDataBeforeUpdateFlagOff(); // Prevents destruction of the current
                                          // mesh output
}

template< typename TOutputMesh >
AutomaticTopologyMeshSource< TOutputMesh >
::~AutomaticTopologyMeshSource()
{}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddPoint(const PointType & p0)
{
  IdentifierType   nextNewPointID = m_OutputMesh->GetNumberOfPoints();
  IdentifierType & pointIDPlusOne = m_PointsHashTable[p0];
  IdentifierType   pointID;

  if ( pointIDPlusOne != 0 )
    {
    pointID = pointIDPlusOne - 1;
    }
  else
    {
    pointID = nextNewPointID;
    pointIDPlusOne = pointID + 1;
    m_OutputMesh->SetPoint(pointID, p0);
    }
  return pointID;
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddPoint(const CoordinateType *p0)
{
  PointType    newPoint;
  unsigned int i;

  for ( i = 0; i < PointDimension; i++ )
    {
    newPoint[i] = p0[i];
    }
  return AddPoint(newPoint);
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddPoint(CoordinateType x0, CoordinateType x1, CoordinateType x2,
           CoordinateType x3, CoordinateType x4, CoordinateType x5)
{
  CoordinateType p0[] = { x0, x1, x2, x3, x4, x5 };
  PointType      newPoint;
  unsigned int   i;
  unsigned int   end = ( PointDimension < 6 ? PointDimension : 6 );

  for ( i = 0; i < end; i++ )
    {
    newPoint[i] = p0[i];
    }
  for (; i < PointDimension; i++ )
    {
    newPoint[i] = 0;
    }
  return AddPoint(newPoint);
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddVertex(const IdentifierArrayType & pointIDs)
{
  // pointIDs is an array with one element; this is for consistency.

  // m_PointsHashTable[ foo ] is set to 0 if foo is not found, but I
  // want the initial identifier to be 0.
  IdentifierType *cellIDPlusOne = &m_CellsHashTable[pointIDs];
  IdentifierType  cellID;

  if ( *cellIDPlusOne != 0 )
    {
    cellID = *cellIDPlusOne - 1;
    }
  else
    {
    // Choose the ID and store it in its hash location (cellIDPlusOne)
    cellID = m_OutputMesh->GetNumberOfCells();
    *cellIDPlusOne = cellID + 1;

    // Construct the cell.
    CellAutoPointer newCell(new VertexCell, true);
    newCell->SetPointId(0, pointIDs[0]);

    // Add the cell to the mesh.
    m_OutputMesh->SetCell(cellID, newCell);

    m_OutputMesh->SetBoundaryAssignment(0, cellID, 0, cellID);
    }
  return cellID;
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddLine(const IdentifierArrayType & pointIDs)
{
  // Check to see if the cell is already referenced in the hash table.
  IdentifierType *cellIDPlusOne = &m_CellsHashTable[pointIDs];
  IdentifierType  cellID;

  if ( *cellIDPlusOne != 0 )
    {
    cellID = *cellIDPlusOne - 1;
    }
  else
    {
    const IdentifierType pointIdsEnd = 2;

    // Construct the cell.
    CellAutoPointer newCell(new LineCell, true);

    // Add the points and vertices, keeping track of the vertex IDs.
    IdentifierArrayType vertexArray(pointIdsEnd);
    IdentifierType      i;
    for ( i = 0; i < pointIdsEnd; i++ )
      {
      IdentifierType pointID = pointIDs[i];
      vertexArray[i] = AddVertex(pointID);
      newCell->SetPointId(i, pointID);
      }

    // Choose the ID and store it in its hash location (cellIDPlusOne)
    cellID = m_OutputMesh->GetNumberOfCells();
    *cellIDPlusOne = cellID + 1;

    // Add the cell to the mesh.
    m_OutputMesh->SetCell(cellID, newCell);

    // Set the boundaries for the new cell.

    for ( i = 0; i < pointIdsEnd; i++ )
      {
      IdentifierType boundaryID = vertexArray[i];
      m_OutputMesh->SetBoundaryAssignment(0, cellID, i, boundaryID);
      }
    }

  return cellID;
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddTriangle(const IdentifierArrayType & pointIDs)
{
  // Check to see if the cell is already referenced in the hash table.
  IdentifierType *cellIDPlusOne = &m_CellsHashTable[pointIDs];
  IdentifierType  cellID;

  if ( *cellIDPlusOne != 0 )
    {
    cellID = *cellIDPlusOne - 1;
    }
  else
    {
    // Create and add a new cell.

    const IdentifierType pointIdsEnd = 3;
    const IdentifierType lineIdsEnd = 3;
    IdentifierType       i;

    // Construct the cell.
    CellAutoPointer newCell(new TriangleCell, true);

    // Add the points and vertices, keeping track of the vertex IDs.
    IdentifierArrayType vertexArray(pointIdsEnd);
    for ( i = 0; i < pointIdsEnd; i++ )
      {
      IdentifierType pointID = pointIDs[i];
      vertexArray[i] = AddVertex(pointID);
      newCell->SetPointId(i, pointID);
      }

    // Add the edges, keeping track of edge IDs.
    IdentifierArrayType lineArray(lineIdsEnd);
    for ( i = 0; i < lineIdsEnd; i++ )
      {
      lineArray[i] = AddLine(pointIDs[i], pointIDs[( i + 1 ) % pointIdsEnd]);
      }

    // Choose the ID and store it in its hash location (cellIDPlusOne)
    cellID = m_OutputMesh->GetNumberOfCells();
    *cellIDPlusOne = cellID + 1;

    // Add the cell to the mesh.
    m_OutputMesh->SetCell(cellID, newCell);

    // Set the boundaries for the new cell.

    for ( i = 0; i < pointIdsEnd; i++ )
      {
      m_OutputMesh->SetBoundaryAssignment(0, cellID, i, vertexArray[i]);
      }

    for ( i = 0; i < lineIdsEnd; i++ )
      {
      m_OutputMesh->SetBoundaryAssignment(1, cellID, i, lineArray[i]);
      }
    }

  return cellID;
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddQuadrilateral(const IdentifierArrayType & pointIDs)
{
  // Check to see if the cell is already referenced in the hash table.
  IdentifierType *cellIDPlusOne = &m_CellsHashTable[pointIDs];
  IdentifierType  cellID;

  if ( *cellIDPlusOne != 0 )
    {
    cellID = *cellIDPlusOne - 1;
    }
  else
    {
    // Create and add a new cell.

    const IdentifierType pointIdsEnd = 4;
    const IdentifierType lineIdsEnd = 4;
    IdentifierType       i;

    // Construct the cell.
    CellAutoPointer newCell(new QuadrilateralCell, true);

    // Add the points and vertices, keeping track of the vertex IDs.
    IdentifierArrayType vertexArray(pointIdsEnd);
    for ( i = 0; i < pointIdsEnd; i++ )
      {
      IdentifierType pointID = pointIDs[i];
      vertexArray[i] = AddVertex(pointID);
      newCell->SetPointId(i, pointID);
      }

    // Add the edges, keeping track of edge IDs.
    IdentifierArrayType lineArray(lineIdsEnd);
    lineArray[0] = AddLine(pointIDs[0], pointIDs[1]);
    lineArray[1] = AddLine(pointIDs[2], pointIDs[3]);
    lineArray[2] = AddLine(pointIDs[0], pointIDs[2]);
    lineArray[3] = AddLine(pointIDs[1], pointIDs[3]);

    // Choose the ID and store it in its hash location (cellIDPlusOne)
    cellID = m_OutputMesh->GetNumberOfCells();
    *cellIDPlusOne = cellID + 1;

    // Add the cell to the mesh.
    m_OutputMesh->SetCell(cellID, newCell);

    // Set the boundaries for the new cell.

    for ( i = 0; i < pointIdsEnd; i++ )
      {
      m_OutputMesh->SetBoundaryAssignment(0, cellID, i, vertexArray[i]);
      }

    for ( i = 0; i < lineIdsEnd; i++ )
      {
      m_OutputMesh->SetBoundaryAssignment(1, cellID, i, lineArray[i]);
      }
    }
  return cellID;
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddTetrahedron(const IdentifierArrayType & pointIDs)
{
  // Check to see if the cell is already referenced in the hash table.
  IdentifierType *cellIDPlusOne = &m_CellsHashTable[pointIDs];
  IdentifierType  cellID;

  if ( *cellIDPlusOne != 0 )
    {
    cellID = *cellIDPlusOne - 1;
    }
  else
    {
    // Create and add a new cell.

    const IdentifierType pointIdsEnd = 4;
    const IdentifierType lineIdsEnd = 6;
    const IdentifierType faceIdsEnd = 4;
    IdentifierType       i;

    // Construct the cell.
    CellAutoPointer newCell(new TetrahedronCell, true);

    // Add the points and vertices, keeping track of the vertex IDs.
    IdentifierArrayType vertexArray(pointIdsEnd);
    for ( i = 0; i < pointIdsEnd; i++ )
      {
      IdentifierType pointID = pointIDs[i];
      vertexArray[i] = AddVertex(pointID);
      newCell->SetPointId(i, pointID);
      }

    // Add the edges, keeping track of edge IDs.
    IdentifierArrayType lineArray(lineIdsEnd);
    lineArray[0] = AddLine(pointIDs[0], pointIDs[1]);
    lineArray[1] = AddLine(pointIDs[0], pointIDs[2]);
    lineArray[2] = AddLine(pointIDs[0], pointIDs[3]);
    lineArray[3] = AddLine(pointIDs[1], pointIDs[2]);
    lineArray[4] = AddLine(pointIDs[1], pointIDs[3]);
    lineArray[5] = AddLine(pointIDs[2], pointIDs[3]);

    // Add the faces, keeping track of face IDs.
    IdentifierArrayType faceArray(faceIdsEnd);
    faceArray[0] = AddTriangle(pointIDs[0], pointIDs[1], pointIDs[2]);
    faceArray[1] = AddTriangle(pointIDs[0], pointIDs[1], pointIDs[3]);
    faceArray[2] = AddTriangle(pointIDs[0], pointIDs[2], pointIDs[3]);
    faceArray[3] = AddTriangle(pointIDs[1], pointIDs[2], pointIDs[3]);

    // Choose the ID and store it in its hash location (cellIDPlusOne)
    cellID = m_OutputMesh->GetNumberOfCells();
    *cellIDPlusOne = cellID + 1;

    // Add the cell to the mesh.
    m_OutputMesh->SetCell(cellID, newCell);

    // Set the boundaries for the new cell.

    for ( i = 0; i < pointIdsEnd; i++ )
      {
      m_OutputMesh->SetBoundaryAssignment(0, cellID, i, vertexArray[i]);
      }

    for ( i = 0; i < lineIdsEnd; i++ )
      {
      m_OutputMesh->SetBoundaryAssignment(1, cellID, i, lineArray[i]);
      }

    for ( i = 0; i < faceIdsEnd; i++ )
      {
      m_OutputMesh->SetBoundaryAssignment(2, cellID, i, faceArray[i]);
      }
    }

  return cellID;
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddHexahedron(const IdentifierArrayType & pointIDs)
{
  // Check to see if the cell is already referenced in the hash table.
  IdentifierType *cellIDPlusOne = &m_CellsHashTable[pointIDs];
  IdentifierType  cellID;

  if ( *cellIDPlusOne != 0 )
    {
    cellID = *cellIDPlusOne - 1;
    }
  else
    {
    // Create and add a new cell.

    const IdentifierType pointIdsEnd = 8;
    const IdentifierType lineIdsEnd = 12;
    const IdentifierType faceIdsEnd = 6;
    IdentifierType       i;

    // Construct the cell.
    CellAutoPointer newCell(new HexahedronCell, true);

    // Add the points and vertices, keeping track of the vertex IDs.
    IdentifierArrayType vertexArray(pointIdsEnd);
    for ( i = 0; i < pointIdsEnd; i++ )
      {
      IdentifierType pointID = pointIDs[i];
      vertexArray[i] = AddVertex(pointID);
      newCell->SetPointId(i, pointID);
      }

    // Add the edges, keeping track of edge IDs.
    IdentifierArrayType lineArray(lineIdsEnd);
    lineArray[0]  = AddLine(pointIDs[0], pointIDs[1]);
    lineArray[1]  = AddLine(pointIDs[2], pointIDs[3]);
    lineArray[2]  = AddLine(pointIDs[4], pointIDs[5]);
    lineArray[3]  = AddLine(pointIDs[6], pointIDs[7]);
    lineArray[4]  = AddLine(pointIDs[0], pointIDs[2]);
    lineArray[5]  = AddLine(pointIDs[1], pointIDs[3]);
    lineArray[6]  = AddLine(pointIDs[4], pointIDs[6]);
    lineArray[7]  = AddLine(pointIDs[5], pointIDs[7]);
    lineArray[8]  = AddLine(pointIDs[0], pointIDs[4]);
    lineArray[9]  = AddLine(pointIDs[1], pointIDs[5]);
    lineArray[10] = AddLine(pointIDs[2], pointIDs[6]);
    lineArray[11] = AddLine(pointIDs[3], pointIDs[7]);

    // Add the faces, keeping track of face IDs.
    IdentifierArrayType faceArray(faceIdsEnd);
    faceArray[0] = AddQuadrilateral(pointIDs[0], pointIDs[1], pointIDs[2], pointIDs[3]);
    faceArray[1] = AddQuadrilateral(pointIDs[4], pointIDs[5], pointIDs[6], pointIDs[7]);
    faceArray[2] = AddQuadrilateral(pointIDs[0], pointIDs[1], pointIDs[4], pointIDs[5]);
    faceArray[3] = AddQuadrilateral(pointIDs[2], pointIDs[3], pointIDs[6], pointIDs[7]);
    faceArray[4] = AddQuadrilateral(pointIDs[0], pointIDs[2], pointIDs[4], pointIDs[6]);
    faceArray[5] = AddQuadrilateral(pointIDs[1], pointIDs[3], pointIDs[5], pointIDs[7]);

    // Choose the ID and store it in its hash location (cellIDPlusOne)
    cellID = m_OutputMesh->GetNumberOfCells();
    *cellIDPlusOne = cellID + 1;

    // Add the cell to the mesh.
    m_OutputMesh->SetCell(cellID, newCell);

    // Set the boundaries for the new cell.

    for ( i = 0; i < pointIdsEnd; i++ )
      {
      m_OutputMesh->SetBoundaryAssignment(0, cellID, i, vertexArray[i]);
      }

    for ( i = 0; i < lineIdsEnd; i++ )
      {
      m_OutputMesh->SetBoundaryAssignment(1, cellID, i, lineArray[i]);
      }

    for ( i = 0; i < faceIdsEnd; i++ )
      {
      m_OutputMesh->SetBoundaryAssignment(2, cellID, i, faceArray[i]);
      }
    }

  return cellID;
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddVertex(IdentifierType pointId0)
{
  Array< IdentifierType > pointIDs(1);
  pointIDs[0] = pointId0;
  return AddVertex(pointIDs);
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddLine(IdentifierType pointId0, IdentifierType pointId1)
{
  Array< IdentifierType > pointIDs(2);
  pointIDs[0] = pointId0;
  pointIDs[1] = pointId1;
  return AddLine(pointIDs);
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddTriangle(IdentifierType pointId0, IdentifierType pointId1,
              IdentifierType pointId2)
{
  Array< IdentifierType > pointIDs(3);
  pointIDs[0] = pointId0;
  pointIDs[1] = pointId1;
  pointIDs[2] = pointId2;
  return AddTriangle(pointIDs);
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddQuadrilateral(IdentifierType pointId0, IdentifierType pointId1,
                   IdentifierType pointId2, IdentifierType pointId3)
{
  Array< IdentifierType > pointIDs(4);
  pointIDs[0] = pointId0;
  pointIDs[1] = pointId1;
  pointIDs[2] = pointId2;
  pointIDs[3] = pointId3;
  return AddQuadrilateral(pointIDs);
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddTetrahedron(IdentifierType pointId0, IdentifierType pointId1,
                 IdentifierType pointId2, IdentifierType pointId3)
{
  Array< IdentifierType > pointIDs(4);
  pointIDs[0] = pointId0;
  pointIDs[1] = pointId1;
  pointIDs[2] = pointId2;
  pointIDs[3] = pointId3;
  return AddTetrahedron(pointIDs);
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddHexahedron(IdentifierType pointId0, IdentifierType pointId1,
                IdentifierType pointId2, IdentifierType pointId3,
                IdentifierType pointId4, IdentifierType pointId5,
                IdentifierType pointId6, IdentifierType pointId7)
{
  Array< IdentifierType > pointIDs(8);
  pointIDs[0] = pointId0;
  pointIDs[1] = pointId1;
  pointIDs[2] = pointId2;
  pointIDs[3] = pointId3;
  pointIDs[4] = pointId4;
  pointIDs[5] = pointId5;
  pointIDs[6] = pointId6;
  pointIDs[7] = pointId7;
  return AddHexahedron(pointIDs);
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddVertex(const PointType & p0)
{
  IdentifierType pointId = AddPoint(p0);

  return AddVertex(pointId);
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddLine(const PointType & p0, const PointType & p1)
{
  Array< IdentifierType > pointIDs(2);
  pointIDs[0] = AddPoint(p0);
  pointIDs[1] = AddPoint(p1);
  return AddLine(pointIDs);
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddTriangle(const PointType & p0, const PointType & p1,
              const PointType & p2)
{
  Array< IdentifierType > pointIDs(3);
  pointIDs[0] = AddPoint(p0);
  pointIDs[1] = AddPoint(p1);
  pointIDs[2] = AddPoint(p2);
  return AddTriangle(pointIDs);
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddQuadrilateral(const PointType & p0, const PointType & p1,
                   const PointType & p2, const PointType & p3)
{
  Array< IdentifierType > pointIDs(4);
  pointIDs[0] = AddPoint(p0);
  pointIDs[1] = AddPoint(p1);
  pointIDs[2] = AddPoint(p2);
  pointIDs[3] = AddPoint(p3);
  return AddQuadrilateral(pointIDs);
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddTetrahedron(const PointType & p0, const PointType & p1,
                 const PointType & p2, const PointType & p3)
{
  Array< IdentifierType > pointIDs(4);
  pointIDs[0] = AddPoint(p0);
  pointIDs[1] = AddPoint(p1);
  pointIDs[2] = AddPoint(p2);
  pointIDs[3] = AddPoint(p3);
  return AddTetrahedron(pointIDs);
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddHexahedron(
  const PointType & p0, const PointType & p1, const PointType & p2,
  const PointType & p3, const PointType & p4, const PointType & p5,
  const PointType & p6, const PointType & p7)
{
  Array< IdentifierType > pointIDs(8);
  pointIDs[0] = AddPoint(p0);
  pointIDs[1] = AddPoint(p1);
  pointIDs[2] = AddPoint(p2);
  pointIDs[3] = AddPoint(p3);
  pointIDs[4] = AddPoint(p4);
  pointIDs[5] = AddPoint(p5);
  pointIDs[6] = AddPoint(p6);
  pointIDs[7] = AddPoint(p7);
  return AddHexahedron(pointIDs);
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddVertex(const CoordinateType *p0)
{
  Array< IdentifierType > pointIDs(1);
  pointIDs[0] = AddPoint(p0);
  return AddVertex(pointIDs);
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddLine(const CoordinateType *p0,
          const CoordinateType *p1)
{
  Array< IdentifierType > pointIDs(2);
  pointIDs[0] = AddPoint(p0);
  pointIDs[1] = AddPoint(p1);
  return AddLine(pointIDs);
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddTriangle(const CoordinateType *p0,
              const CoordinateType *p1,
              const CoordinateType *p2)
{
  Array< IdentifierType > pointIDs(3);
  pointIDs[0] = AddPoint(p0);
  pointIDs[1] = AddPoint(p1);
  pointIDs[2] = AddPoint(p2);
  return AddTriangle(pointIDs);
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddQuadrilateral(const CoordinateType *p0,
                   const CoordinateType *p1,
                   const CoordinateType *p2,
                   const CoordinateType *p3)
{
  Array< IdentifierType > pointIDs(4);
  pointIDs[0] = AddPoint(p0);
  pointIDs[1] = AddPoint(p1);
  pointIDs[2] = AddPoint(p2);
  pointIDs[3] = AddPoint(p3);
  return AddQuadrilateral(pointIDs);
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddTetrahedron(const CoordinateType *p0,
                 const CoordinateType *p1,
                 const CoordinateType *p2,
                 const CoordinateType *p3)
{
  Array< IdentifierType > pointIDs(4);
  pointIDs[0] = AddPoint(p0);
  pointIDs[1] = AddPoint(p1);
  pointIDs[2] = AddPoint(p2);
  pointIDs[3] = AddPoint(p3);
  return AddTetrahedron(pointIDs);
}

template< typename TOutputMesh >
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddHexahedron(const CoordinateType *p0,
                const CoordinateType *p1,
                const CoordinateType *p2,
                const CoordinateType *p3,
                const CoordinateType *p4,
                const CoordinateType *p5,
                const CoordinateType *p6,
                const CoordinateType *p7)
{
  Array< IdentifierType > pointIDs(8);
  pointIDs[0] = AddPoint(p0);
  pointIDs[1] = AddPoint(p1);
  pointIDs[2] = AddPoint(p2);
  pointIDs[3] = AddPoint(p3);
  pointIDs[4] = AddPoint(p4);
  pointIDs[5] = AddPoint(p5);
  pointIDs[6] = AddPoint(p6);
  pointIDs[7] = AddPoint(p7);
  return AddHexahedron(pointIDs);
}
} /** end namespace itk. */

#endif
