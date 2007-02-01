/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkSimplexMesh.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSimplexMesh_txx
#define _itkSimplexMesh_txx

#include "itkSimplexMesh.h"

#include "itkObjectFactory.h"
#include "itkProcessObject.h"
#include <algorithm>

#include <vxl_version.h>
#if VXL_VERSION_DATE_FULL > 20040406
# include <vnl/vnl_cross.h>
# define itk_cross_3d vnl_cross_3d
#else
# define itk_cross_3d cross_3d
#endif

namespace itk
{

/**
 * A protected default constructor allows the New() routine to create an
 * instance of SimplexMesh.   All the containers are initialized to empty
 * containers.
 */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::SimplexMesh() : m_LastCellId ( 0 ) 
{
  m_GeometryData = GeometryMapType::New();
}



/**
 * Mesh Destructor takes care of releasing the memory of Cells
 * and CellBoundaries objects for which normal pointers are
 * stored.
 */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::~SimplexMesh()
{
  itkDebugMacro("Mesh Destructor ");

  GeometryMapPointer  geometryMap = this->GetGeometryData();
  GeometryMapIterator pointDataIterator = geometryMap->Begin();
  GeometryMapIterator pointDataEnd = geometryMap->End();

  while (pointDataIterator != pointDataEnd)
    {
    SimplexMeshGeometry* geometry = pointDataIterator->Value();
    if( geometry )
      {
      delete geometry;
      }
    pointDataIterator++;
    }
  // clear the map
  geometryMap->Initialize();
  this->ReleaseCellsMemory();
}


template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void 
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::CopyInformation(const DataObject *data)
{
  const Superclass *mesh;

  mesh = dynamic_cast<const Superclass*>(data);

  if (mesh)
    {
    this->m_MaximumNumberOfRegions = mesh->GetMaximumNumberOfRegions();
    }
  else
    {
    // pointer could not be cast back down
    itkExceptionMacro(<< "itk::Mesh::CopyInformation() cannot cast "
                      << typeid(data).name() << " to "
                      << typeid(Superclass*).name() );
    }


}


template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void 
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::SetBarycentricCoordinates(unsigned long idx, PointType value)
{
  SimplexMeshGeometry* geometry = m_GeometryData->GetElement(idx);
  geometry->eps = value;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
typename SimplexMesh<TPixelType, VDimension, TMeshTraits>::PointType
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::GetBarycentricCoordinates(unsigned long idx) const
{
  return m_GeometryData->GetElement(idx)->eps;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void 
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::SetReferenceMetrics(unsigned long idx, PointType value)
{
  SimplexMeshGeometry* geometry = m_GeometryData->GetElement(idx);
  geometry->referenceMetrics = value;
}


template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
typename SimplexMesh<TPixelType, VDimension, TMeshTraits>::PointType
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::GetReferenceMetrics(unsigned long idx) const
{
  return m_GeometryData->GetElement(idx)->referenceMetrics;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void 
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::SetPhi(unsigned long idx, double value)
{
  SimplexMeshGeometry* geometry = m_GeometryData->GetElement(idx);
  geometry->phi = value;
}


template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
double
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::GetPhi(unsigned long idx) const
{
  SimplexMeshGeometry* geometry = m_GeometryData->GetElement(idx);
  PointType test;
  this->GetPoint(idx, &test);

  return m_GeometryData->GetElement(idx)->phi;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void 
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::SetMeanCurvature(unsigned long idx, double value)
{
  SimplexMeshGeometry* geometry = m_GeometryData->GetElement(idx);
  geometry->meanCurvature = value;
}


template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
double 
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::GetMeanCurvature(unsigned long idx) const
{
  return m_GeometryData->GetElement(idx)->meanCurvature;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void 
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::SetRadius(unsigned long idx, double value)
{
  SimplexMeshGeometry* geometry = m_GeometryData->GetElement(idx);
  geometry->circleRadius = value;
}


template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
double
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::GetRadius(unsigned long idx) const
{
  return m_GeometryData->GetElement(idx)->circleRadius;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void 
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::SetDistance(unsigned long idx, double value)
{
  SimplexMeshGeometry* geometry = m_GeometryData->GetElement(idx);
  geometry->distance = value;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
double
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::GetDistance(unsigned long idx) const
{
  return m_GeometryData->GetElement(idx)->distance;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
unsigned long 
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::AddEdge(unsigned long startPointId, unsigned long endPointId)
{ 
  CellAutoPointer NewCellPointer;
  unsigned long edgeId = m_LastCellId;

  NewCellPointer.TakeOwnership( new LineType );
  NewCellPointer->SetPointId( 0, startPointId );
  NewCellPointer->SetPointId( 1, endPointId );

  this->SetCell( edgeId, NewCellPointer );
  m_LastCellId++;
  return edgeId;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
unsigned long 
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::AddFace(CellAutoPointer &cellPointer)
{
  this->SetCell( m_LastCellId , cellPointer );
  m_LastCellId++;
  return m_LastCellId-1;      
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
unsigned long 
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::ReplaceFace(unsigned long replaceIndex, CellAutoPointer &cellPointer)
{
  this->GetCells()->DeleteIndex( replaceIndex );
  this->SetCell( replaceIndex , cellPointer );
  this->SetCellData( replaceIndex , (PixelType) 1.0 );
  return replaceIndex;      
}

/* PrintSelf. */
template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void 
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os,indent);

  os << indent << "LastCellId = " << m_LastCellId << std::endl;

  CellsContainerPointer cells = this->GetCells();
  CellsContainerIterator cellIt = cells->Begin();

  os << indent << "Cells Point Ids:" << std::endl;
  while ( cellIt != cells->End() )
    {
    os << indent << "cell id: " << cellIt->Index() << ", point ids: ";
    CellType *nextCell = cellIt->Value();
    typename CellType::PointIdIterator pointIt = nextCell->PointIdsBegin() ;
    while (pointIt != nextCell->PointIdsEnd() ) { os << *pointIt++ << "-"; }
    os << std::endl;
    cellIt++;
    }

  PointsContainerConstIterator pointsIt = this->GetPoints()->Begin();
  os << indent << "Point locations:" << std::endl;
  while ( pointsIt != this->GetPoints()->End() )
    {
    os << indent << "pt index:" << pointsIt->Index() << " , coords: " << pointsIt->Value() << std::endl;
    pointsIt++;
    }

  GeometryMapPointer geometryMap = this->GetGeometryData();
  GeometryMapIterator pointDataIterator = geometryMap->Begin();
  GeometryMapIterator pointDataEnd = geometryMap->End();

  while (pointDataIterator != pointDataEnd)
    {
    SimplexMeshGeometry* geometry = pointDataIterator->Value();
    os << indent << "Mesh Geometry Data for point:"<< pointDataIterator->Index() << std::endl;
    os << indent << "Direct Neighbors indices: " 
       << geometry->neighborIndices[0] << ", "
       << geometry->neighborIndices[1] << ", " 
       << geometry->neighborIndices[2] << std::endl;
    pointDataIterator++;
    }
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void 
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::SetGeometryData(unsigned long pointId, SimplexMeshGeometry* geometryData)
{
  m_GeometryData->InsertElement(pointId, geometryData);
}


template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
typename SimplexMesh<TPixelType, VDimension, TMeshTraits>::IndexArray 
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::GetNeighbors(unsigned long idx) const
{
  return m_GeometryData->GetElement(idx)->neighborIndices;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
typename SimplexMesh<TPixelType, VDimension, TMeshTraits>::NeighborListType*  
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::GetNeighbors(unsigned long idx, unsigned int radius, NeighborListType* list ) const
{
  if (list == NULL)
    {
    list = new NeighborListType();
    IndexArray neighborArray = GetNeighbors(idx);
    list->push_back(neighborArray[0]);
    list->push_back(neighborArray[1]);
    list->push_back(neighborArray[2]);

    if(radius>0)
      {
      list = GetNeighbors(neighborArray[0], radius-1, list);
      list = GetNeighbors(neighborArray[1], radius-1, list);
      list = GetNeighbors(neighborArray[2], radius-1, list);
      }
    NeighborListType::iterator it = std::find( list->begin(),list->end(),idx );
    if (it != list->end()) list->erase(it);

    return list;
    }
  else 
    {
    IndexArray neighborArray = GetNeighbors(idx);

    NeighborListType::iterator foundIt1 = std::find( list->begin(),list->end(),neighborArray[0] );
    NeighborListType::iterator foundIt2 = std::find( list->begin(),list->end(),neighborArray[1] );
    NeighborListType::iterator foundIt3 = std::find( list->begin(),list->end(),neighborArray[2] );
    NeighborListType::iterator endIt = list->end();
    bool found1=false, found2=false, found3=false;

    if (foundIt1 != endIt) found1 =true;
    if (foundIt2 != endIt) found2 = true;
    if (foundIt3 != endIt) found3 = true;

    if (!found1) list->push_back(neighborArray[0]);
    if (!found2) list->push_back(neighborArray[1]);
    if (!found3) list->push_back(neighborArray[2]);

    if (radius == 0) 
      {
      return list;
      }
    else
      {
      list = GetNeighbors(neighborArray[0], radius-1, list);
      list = GetNeighbors(neighborArray[1], radius-1, list);
      list = GetNeighbors(neighborArray[2], radius-1, list);
      return list;
      }
    }
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::AddNeighbor(unsigned long pointIdx, unsigned long neighborIdx)
{
  SimplexMeshGeometry* data = m_GeometryData->GetElement(pointIdx);

  for (int i = 0; i < 3; i++)
    {
    if (data->neighborIndices[i] == ((unsigned long)NumericTraits<unsigned long>::max() ))
      {
      data->neighborIndices[i] = neighborIdx;
      break;
      }
    }
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void 
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::ReplaceNeighbor(unsigned long pointIdx, unsigned long oldIdx,unsigned long newIdx)
{
  SimplexMeshGeometry* data = m_GeometryData->GetElement(pointIdx);

  for (int i = 0; i < 3;i++)
    {
    if (data->neighborIndices[i] == oldIdx)
      {
      data->neighborIndices[i] = newIdx;
      }
    }
}


template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
void 
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::SwapNeighbors(unsigned long pointIdx, unsigned long firstIdx,unsigned long secondIdx)
{
  SimplexMeshGeometry* data = m_GeometryData->GetElement(pointIdx);
  int i;
  int firstFound = -1;
  int secondFound = -1;

  for (i = 0; i < 3;i++)
    {
    if (data->neighborIndices[i] == firstIdx)
      {
      firstFound = i;
      }
    else if (data->neighborIndices[i] == secondIdx)
      {
      secondFound = i;
      }
    }
  if(firstFound == -1 || secondFound == -1)
    {
    itkExceptionMacro("first and second not found");
    }
  data->neighborIndices[firstFound] = secondIdx;
  data->neighborIndices[secondFound] = firstIdx;
}

template <typename TPixelType, unsigned int VDimension, typename TMeshTraits>
typename SimplexMesh<TPixelType, VDimension, TMeshTraits>::CovariantVectorType 
SimplexMesh<TPixelType, VDimension, TMeshTraits>
::ComputeNormal(unsigned long idx ) const
{
  PointType p,n1,n2,n3;

  IndexArray neighbors = this->GetNeighbors( idx );  
  this->GetPoint(idx,&p);
  this->GetPoint(neighbors[0],&n1);
  this->GetPoint(neighbors[1],&n2);
  this->GetPoint(neighbors[2],&n3);

  // compute normals
  CovariantVectorType normal;
  normal.Fill(0.0);
  CovariantVectorType z;
  z.SetVnlVector( itk_cross_3d((n2-n1).GetVnlVector() , (n3-n1).GetVnlVector()) );
  z.Normalize();
  normal += z;

  return normal;
}

} // end namespace itk

#endif
