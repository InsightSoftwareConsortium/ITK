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
#ifndef itkSimplexMesh_hxx
#define itkSimplexMesh_hxx

#include "itkSimplexMesh.h"

#include "itkProcessObject.h"
#include <algorithm>

#include "vxl_version.h"
#if VXL_VERSION_DATE_FULL > 20040406
#include "vnl/vnl_cross.h"
#define itk_cross_3d vnl_cross_3d
#else
#define itk_cross_3d cross_3d
#endif

namespace itk
{
/**
 * A protected default constructor allows the New() routine to create an
 * instance of SimplexMesh.   All the containers are initialized to empty
 * containers.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::SimplexMesh():m_LastCellId (0)
{
  m_GeometryData = GeometryMapType::New();
}

/**
 * Mesh Destructor takes care of releasing the memory of Cells
 * and CellBoundaries objects for which normal pointers are
 * stored.
 */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::~SimplexMesh()
{
  itkDebugMacro("Mesh Destructor ");

  GeometryMapPointer  geometryMap = this->GetGeometryData();
  GeometryMapIterator pointDataIterator = geometryMap->Begin();
  GeometryMapIterator pointDataEnd = geometryMap->End();

  while ( pointDataIterator != pointDataEnd )
    {
    SimplexMeshGeometry *geometry = pointDataIterator->Value();
    delete geometry;
    pointDataIterator++;
    }
  // clear the map
  geometryMap->Initialize();
  this->ReleaseCellsMemory();
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::CopyInformation(const DataObject *data)
{
  const Superclass *mesh;

  mesh = dynamic_cast< const Superclass * >( data );

  if ( mesh == ITK_NULLPTR )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::Mesh::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Superclass * ).name() );
    }

  this->m_MaximumNumberOfRegions = mesh->GetMaximumNumberOfRegions();
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::SetBarycentricCoordinates(PointIdentifier idx, PointType value)
{
  SimplexMeshGeometry *geometry = m_GeometryData->GetElement(idx);

  geometry->eps = value;
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
typename SimplexMesh< TPixelType, VDimension, TMeshTraits >::PointType
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::GetBarycentricCoordinates(PointIdentifier idx) const
{
  return m_GeometryData->GetElement(idx)->eps;
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::SetReferenceMetrics(PointIdentifier idx, PointType value)
{
  SimplexMeshGeometry *geometry = m_GeometryData->GetElement(idx);

  geometry->referenceMetrics = value;
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
typename SimplexMesh< TPixelType, VDimension, TMeshTraits >::PointType
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::GetReferenceMetrics(PointIdentifier idx) const
{
  return m_GeometryData->GetElement(idx)->referenceMetrics;
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::SetPhi(PointIdentifier idx, double value)
{
  SimplexMeshGeometry *geometry = m_GeometryData->GetElement(idx);

  geometry->phi = value;
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
double
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::GetPhi(PointIdentifier idx) const
{
  PointType            test;

  this->GetPoint(idx, &test);

  return m_GeometryData->GetElement(idx)->phi;
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::SetMeanCurvature(PointIdentifier idx, double value)
{
  SimplexMeshGeometry *geometry = m_GeometryData->GetElement(idx);

  geometry->meanCurvature = value;
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
double
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::GetMeanCurvature(PointIdentifier idx) const
{
  return m_GeometryData->GetElement(idx)->meanCurvature;
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::SetRadius(PointIdentifier idx, double value)
{
  SimplexMeshGeometry *geometry = m_GeometryData->GetElement(idx);

  geometry->circleRadius = value;
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
double
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::GetRadius(PointIdentifier idx) const
{
  return m_GeometryData->GetElement(idx)->circleRadius;
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::SetDistance(PointIdentifier idx, double value)
{
  SimplexMeshGeometry *geometry = m_GeometryData->GetElement(idx);

  geometry->distance = value;
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
double
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::GetDistance(PointIdentifier idx) const
{
  return m_GeometryData->GetElement(idx)->distance;
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
typename SimplexMesh< TPixelType, VDimension, TMeshTraits >::CellIdentifier
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::AddEdge(PointIdentifier startPointId, PointIdentifier endPointId)
{
  CellAutoPointer NewCellPointer(new LineType, true);
  CellIdentifier  edgeId = m_LastCellId;

  NewCellPointer->SetPointId(0, startPointId);
  NewCellPointer->SetPointId(1, endPointId);

  this->SetCell(edgeId, NewCellPointer);
  m_LastCellId++;
  return edgeId;
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
typename SimplexMesh< TPixelType, VDimension, TMeshTraits >::CellIdentifier
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::AddFace(CellAutoPointer & cellPointer)
{
  this->SetCell(m_LastCellId, cellPointer);
  m_LastCellId++;
  return m_LastCellId - 1;
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
typename SimplexMesh< TPixelType, VDimension, TMeshTraits >::CellIdentifier
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::ReplaceFace(CellIdentifier replaceIndex, CellAutoPointer & cellPointer)
{
  // Release previous cell, if any.
  // See documentation of Mesh::SetCell().
  CellAutoPointer cellToDelete;
  this->GetCell(replaceIndex, cellToDelete);
  cellToDelete.TakeOwnership();

  // Now place the new cell and its cell data.
  this->SetCell(replaceIndex, cellPointer);
  this->SetCellData(replaceIndex, (PixelType)1.0);
  return replaceIndex;
}

/* PrintSelf. */
template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);

  os << indent << "LastCellId = " << m_LastCellId << std::endl;

  GeometryMapPointer  geometryMap = this->GetGeometryData();
  os << indent << "GeometryData: " << geometryMap << std::endl;
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::SetGeometryData(PointIdentifier pointId, SimplexMeshGeometry *geometryData)
{
  SimplexMeshGeometry* oldGeometryData;
  if( m_GeometryData->GetElementIfIndexExists(pointId, &oldGeometryData) )
    {
    delete oldGeometryData;
    }
  m_GeometryData->InsertElement(pointId, geometryData);
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
typename SimplexMesh< TPixelType, VDimension, TMeshTraits >::IndexArray
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::GetNeighbors(PointIdentifier idx) const
{
  return m_GeometryData->GetElement(idx)->neighborIndices;
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
typename SimplexMesh< TPixelType, VDimension, TMeshTraits >::NeighborListType *
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::GetNeighbors(PointIdentifier idx, unsigned int radius, NeighborListType *list) const
{
  if ( list == ITK_NULLPTR )
    {
    list = new NeighborListType();
    IndexArray neighborArray = GetNeighbors(idx);
    list->push_back(neighborArray[0]);
    list->push_back(neighborArray[1]);
    list->push_back(neighborArray[2]);

    if ( radius > 0 )
      {
      list = GetNeighbors(neighborArray[0], radius - 1, list);
      list = GetNeighbors(neighborArray[1], radius - 1, list);
      list = GetNeighbors(neighborArray[2], radius - 1, list);
      }
    NeighborListType::iterator it = std::find(list->begin(), list->end(), idx);
    if ( it != list->end() ) { list->erase(it); }

    return list;
    }
  else
    {
    IndexArray neighborArray = GetNeighbors(idx);

    NeighborListType::iterator foundIt1 = std::find(list->begin(), list->end(), neighborArray[0]);
    NeighborListType::iterator foundIt2 = std::find(list->begin(), list->end(), neighborArray[1]);
    NeighborListType::iterator foundIt3 = std::find(list->begin(), list->end(), neighborArray[2]);
    NeighborListType::iterator endIt = list->end();
    bool                       found1 = false, found2 = false, found3 = false;

    if ( foundIt1 != endIt ) { found1 = true; }
    if ( foundIt2 != endIt ) { found2 = true; }
    if ( foundIt3 != endIt ) { found3 = true; }

    if ( !found1 ) { list->push_back(neighborArray[0]); }
    if ( !found2 ) { list->push_back(neighborArray[1]); }
    if ( !found3 ) { list->push_back(neighborArray[2]); }

    if ( radius == 0 )
      {
      return list;
      }
    else
      {
      list = GetNeighbors(neighborArray[0], radius - 1, list);
      list = GetNeighbors(neighborArray[1], radius - 1, list);
      list = GetNeighbors(neighborArray[2], radius - 1, list);
      return list;
      }
    }
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::AddNeighbor(PointIdentifier pointIdx, PointIdentifier neighborIdx)
{
  SimplexMeshGeometry *data = m_GeometryData->GetElement(pointIdx);

  for ( int i = 0; i < 3; i++ )
    {
    if ( data->neighborIndices[i] == ( (PointIdentifier)NumericTraits< PointIdentifier >::max() ) )
      {
      data->neighborIndices[i] = neighborIdx;
      break;
      }
    }
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::ReplaceNeighbor(PointIdentifier pointIdx, PointIdentifier oldIdx, PointIdentifier newIdx)
{
  SimplexMeshGeometry *data = m_GeometryData->GetElement(pointIdx);

  for ( int i = 0; i < 3; i++ )
    {
    if ( data->neighborIndices[i] == oldIdx )
      {
      data->neighborIndices[i] = newIdx;
      }
    }
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
void
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::SwapNeighbors(PointIdentifier pointIdx, PointIdentifier firstIdx, PointIdentifier secondIdx)
{
  SimplexMeshGeometry *data = m_GeometryData->GetElement(pointIdx);
  int                  i;
  int                  firstFound = -1;
  int                  secondFound = -1;

  for ( i = 0; i < 3; i++ )
    {
    if ( data->neighborIndices[i] == firstIdx )
      {
      firstFound = i;
      }
    else if ( data->neighborIndices[i] == secondIdx )
      {
      secondFound = i;
      }
    }
  if ( firstFound == -1 || secondFound == -1 )
    {
    itkExceptionMacro("first and second not found");
    }
  data->neighborIndices[firstFound] = secondIdx;
  data->neighborIndices[secondFound] = firstIdx;
}

template< typename TPixelType, unsigned int VDimension, typename TMeshTraits >
typename SimplexMesh< TPixelType, VDimension, TMeshTraits >::CovariantVectorType
SimplexMesh< TPixelType, VDimension, TMeshTraits >
::ComputeNormal(PointIdentifier idx) const
{
  PointType p, n1, n2, n3;

  p.Fill(0);
  n1.Fill(0);
  n2.Fill(0);
  n3.Fill(0);

  IndexArray neighbors = this->GetNeighbors(idx);
  this->GetPoint(idx, &p);
  this->GetPoint(neighbors[0], &n1);
  this->GetPoint(neighbors[1], &n2);
  this->GetPoint(neighbors[2], &n3);

  // compute normals
  CovariantVectorType normal;
  normal.Fill(0.0);
  CovariantVectorType z;
  z.SetVnlVector( itk_cross_3d( ( n2 - n1 ).GetVnlVector(), ( n3 - n1 ).GetVnlVector() ) );
  z.Normalize();
  normal += z;

  return normal;
}
} // end namespace itk

#endif
