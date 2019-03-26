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
#ifndef itkPolyData_hxx
#define itkPolyData_hxx

#include "itkPolyData.h"

namespace itk
{

template< typename TPixelType >
PolyData< TPixelType >
::PolyData():
  m_PointsContainer(nullptr),
  m_PointDataContainer(nullptr),
  m_CellDataContainer(nullptr)
{
}


template< typename TPixelType >
void
PolyData< TPixelType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Number Of Points: "
     << this->GetNumberOfPoints()  << std::endl;
  os << indent << "Point Data Container pointer: "
     << ( ( this->m_PointDataContainer ) ?  this->m_PointDataContainer.GetPointer() : nullptr ) << std::endl;
  os << indent << "Size of Point Data Container: "
     << ( ( this->m_PointDataContainer ) ?  this->m_PointDataContainer->Size() : 0 ) << std::endl;
  os << indent << "Cell Data Container pointer: "
     << ( ( m_CellDataContainer ) ?  m_CellDataContainer.GetPointer() : nullptr ) << std::endl;
  os << indent << "Size of Cell Data Container: "
     << ( ( m_CellDataContainer ) ?  m_CellDataContainer->Size() : 0 ) << std::endl;
}


template< typename TPixelType >
void
PolyData< TPixelType >
::SetPoints(PointsContainer *points)
{
  itkDebugMacro("setting Points container to " << points);
  if ( m_PointsContainer != points )
    {
    m_PointsContainer = points;
    this->Modified();
    }
}


template< typename TPixelType >
typename PolyData< TPixelType >::PointsContainer *
PolyData< TPixelType >
::GetPoints()
{
  itkDebugMacro("Starting GetPoints()");
  if ( !m_PointsContainer )
    {
    this->SetPoints( PointsContainer::New() );
    }
  itkDebugMacro("returning Points container of " << m_PointsContainer);
  return m_PointsContainer;
}


template< typename TPixelType >
const typename PolyData< TPixelType >::PointsContainer *
PolyData< TPixelType >
::GetPoints() const
{
  itkDebugMacro("returning Points container of " << m_PointsContainer);
  return m_PointsContainer.GetPointer();
}


template< typename TPixelType >
void
PolyData< TPixelType >
::SetPointData(PointDataContainer *pointData)
{
  itkDebugMacro("setting PointData container to " << pointData);
  if ( m_PointDataContainer != pointData )
    {
    m_PointDataContainer = pointData;
    this->Modified();
    }
}


template< typename TPixelType >
typename PolyData< TPixelType >::PointDataContainer *
PolyData< TPixelType >
::GetPointData()
{
  if ( !m_PointDataContainer )
    {
    this->SetPointData( PointDataContainer::New() );
    }
  itkDebugMacro("returning PointData container of " << m_PointDataContainer);
  return m_PointDataContainer;
}


template< typename TPixelType >
const typename PolyData< TPixelType >::PointDataContainer *
PolyData< TPixelType >
::GetPointData() const
{
  itkDebugMacro("returning PointData container of "
                << m_PointDataContainer);
  return m_PointDataContainer.GetPointer();
}


template< typename TPixelType >
void
PolyData< TPixelType >
::SetPoint(PointIdentifier ptId, PointType point)
{
  /**
   * Make sure a points container exists.
   */
  if ( !m_PointsContainer )
    {
    this->SetPoints( PointsContainer::New() );
    }

  /**
   * Insert the point into the container with the given identifier.
   */
  m_PointsContainer->InsertElement(ptId, point);
}


template< typename TPixelType >
bool
PolyData< TPixelType >
::GetPoint(PointIdentifier ptId, PointType *point) const
{
  /**
   * If the points container doesn't exist, then the point doesn't either.
   */
  if ( !m_PointsContainer )
    {
    return false;
    }

  /**
   * Ask the container if the point identifier exists.
   */
  return m_PointsContainer->GetElementIfIndexExists(ptId, point);
}


template< typename TPixelType >
typename PolyData< TPixelType >::PointType
PolyData< TPixelType >
::GetPoint(PointIdentifier ptId) const
{
  /**
   * If the points container doesn't exist, then the point doesn't either.
   */
  if ( !m_PointsContainer )
    {
    itkExceptionMacro("Point container doesn't exist.");
    }

  /**
   * Ask the container if the point identifier exists.
   */
  PointType point;
  bool exist = m_PointsContainer->GetElementIfIndexExists(ptId, &point);
  if( ! exist )
    {
    itkExceptionMacro("Point id doesn't exist: " << ptId);
    }
  return point;
}


template< typename TPixelType >
void
PolyData< TPixelType >
::SetPointData(PointIdentifier ptId, PixelType data)
{
  /**
   * Make sure a point data container exists.
   */
  if ( !m_PointDataContainer )
    {
    this->SetPointData( PointDataContainer::New() );
    }

  /**
   * Insert the point data into the container with the given identifier.
   */
  m_PointDataContainer->InsertElement(ptId, data);
}


template< typename TPixelType >
bool
PolyData< TPixelType >
::GetPointData(PointIdentifier ptId, PixelType *data) const
{
  /**
   * If the point data container doesn't exist, then the point data doesn't
   * either.
   */
  if ( !m_PointDataContainer )
    {
    return false;
    }

  /**
   * Ask the container if the point identifier exists.
   */
  return m_PointDataContainer->GetElementIfIndexExists(ptId, data);
}


template< typename TPixelType >
typename PolyData< TPixelType >::PointIdentifier
PolyData< TPixelType >
::GetNumberOfPoints() const
{
  if ( m_PointsContainer )
    {
    return m_PointsContainer->Size();
    }
  return 0;
}


template< typename TPixelType >
void
PolyData< TPixelType >
::SetCellData(CellDataContainer *cellData)
{
  itkDebugMacro("setting CellData container to " << cellData);
  if ( m_CellDataContainer != cellData )
    {
    m_CellDataContainer = cellData;
    this->Modified();
    }
}


template< typename TPixelType >
typename PolyData< TPixelType >::CellDataContainer *
PolyData< TPixelType >
::GetCellData()
{
  itkDebugMacro("returning CellData container of "
                << m_CellDataContainer);
  return m_CellDataContainer;
}


template< typename TPixelType >
const typename PolyData< TPixelType >::CellDataContainer *
PolyData< TPixelType >
::GetCellData() const
{
  itkDebugMacro("returning CellData container of "
                << m_CellDataContainer);
  return m_CellDataContainer;
}


template< typename TPixelType >
void
PolyData< TPixelType >
::SetCellData(CellIdentifier cellId, PixelType data)
{
  /**
   * Assign data to a cell identifier.  If a spot for the cell identifier
   * does not exist, it will be created automatically.  There is no check if
   * a cell with the same identifier exists.
   */

  /**
   * Make sure a cell data container exists.
   */
  if ( !m_CellDataContainer )
    {
    this->SetCellData( CellDataContainer::New() );
    }

  /**
   * Insert the cell data into the container with the given identifier.
   */
  m_CellDataContainer->InsertElement(cellId, data);
}


template< typename TPixelType >
bool
PolyData< TPixelType >
::GetCellData(CellIdentifier cellId, PixelType *data) const
{
  /**
   * Check if cell data exists for a given cell identifier.  If a spot for
   * the cell identifier exists, "data" is set, and true is returned.
   * Otherwise, false is returned, and "data" is not modified.
   * If "data" is nullptr, then it is never set, but the existence of the cell
   * data is still returned.
   */

  /**
   * If the cell data container doesn't exist, then the cell data doesn't
   * either.
   */
  if ( !m_CellDataContainer )
    {
    return false;
    }

  /**
   * Ask the container if the cell identifier exists.
   */
  return m_CellDataContainer->GetElementIfIndexExists(cellId, data);
}


template< typename TPixelType >
void
PolyData< TPixelType >
::Initialize()
{
  Superclass::Initialize();

  m_PointsContainer = nullptr;
  m_PointDataContainer = nullptr;
  m_CellDataContainer = nullptr;
}

} // end namespace itk

#endif // itkPolyData_hxx
