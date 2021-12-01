/*=========================================================================
 *
 *  Copyright NumFOCUS
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

template< typename TPixelType, typename TCellPixel >
PolyData< TPixelType, TCellPixel >
::PolyData():
  m_PointsContainer(nullptr),
  m_VerticesContainer(nullptr),
  m_LinesContainer(nullptr),
  m_PolygonsContainer(nullptr),
  m_TriangleStripsContainer(nullptr),
  m_PointDataContainer(nullptr),
  m_CellDataContainer(nullptr)
{
}


template< typename TPixelType, typename TCellPixel >
void
PolyData< TPixelType, TCellPixel >
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


template< typename TPixelType, typename TCellPixel >
void
PolyData< TPixelType, TCellPixel >
::SetPoints(PointsContainer *points)
{
  itkDebugMacro("setting Points container to " << points);
  if ( m_PointsContainer != points )
    {
    m_PointsContainer = points;
    this->Modified();
    }
}


template< typename TPixelType, typename TCellPixel >
auto
PolyData< TPixelType, TCellPixel >
::GetPoints() -> PointsContainer *
{
  itkDebugMacro("Starting GetPoints()");
  if ( !m_PointsContainer )
    {
    this->SetPoints( PointsContainer::New() );
    }
  itkDebugMacro("returning Points container of " << m_PointsContainer);
  return m_PointsContainer;
}


template< typename TPixelType, typename TCellPixel >
auto
PolyData< TPixelType, TCellPixel >
::GetPoints() const -> const PointsContainer *
{
  itkDebugMacro("returning Points container of " << m_PointsContainer);
  return m_PointsContainer.GetPointer();
}


template< typename TPixelType, typename TCellPixel >
void
PolyData< TPixelType, TCellPixel >
::SetVertices(CellsContainer *vertices)
{
  itkDebugMacro("setting Vertices container to " << vertices);
  if ( m_VerticesContainer != vertices )
    {
    m_VerticesContainer = vertices;
    this->Modified();
    }
}


template< typename TPixelType, typename TCellPixel >
auto
PolyData< TPixelType, TCellPixel >
::GetVertices() -> CellsContainer *
{
  itkDebugMacro("Starting GetVertices()");
  if ( !m_VerticesContainer ) {
    this->SetVertices( CellsContainer::New() );
    }
  itkDebugMacro("returning Vertices container of " << m_VerticesContainer);
  return m_VerticesContainer.GetPointer();
}


template< typename TPixelType, typename TCellPixel >
auto
PolyData< TPixelType, TCellPixel >
::GetVertices() const -> const CellsContainer *
{
  itkDebugMacro("returning Vertices container of " << m_VerticesContainer);
  return m_VerticesContainer.GetPointer();
}


template< typename TPixelType, typename TCellPixel >
void
PolyData< TPixelType, TCellPixel >
::SetLines(CellsContainer *lines)
{
  itkDebugMacro("setting Lines container to " << lines);
  if ( m_LinesContainer != lines )
    {
    m_LinesContainer = lines;
    this->Modified();
    }
}


template< typename TPixelType, typename TCellPixel >
auto
PolyData< TPixelType, TCellPixel >
::GetLines() -> CellsContainer *
{
  itkDebugMacro("Starting GetLines()");
  if ( !m_LinesContainer ) {
    this->SetLines( CellsContainer::New() );
    }
  itkDebugMacro("returning Lines container of " << m_LinesContainer);
  return m_LinesContainer.GetPointer();
}


template< typename TPixelType, typename TCellPixel >
auto
PolyData< TPixelType, TCellPixel >
::GetLines() const -> const CellsContainer *
{
  itkDebugMacro("returning Lines container of " << m_LinesContainer);
  return m_LinesContainer.GetPointer();
}


template< typename TPixelType, typename TCellPixel >
void
PolyData< TPixelType, TCellPixel >
::SetPolygons(CellsContainer *polygons)
{
  itkDebugMacro("setting Polygons container to " << polygons);
  if ( m_PolygonsContainer != polygons )
    {
    m_PolygonsContainer = polygons;
    this->Modified();
    }
}


template< typename TPixelType, typename TCellPixel >
auto
PolyData< TPixelType, TCellPixel >
::GetPolygons() -> CellsContainer *
{
  itkDebugMacro("Starting GetPolygons()");
  if ( !m_PolygonsContainer ) {
    this->SetPolygons( CellsContainer::New() );
    }
  itkDebugMacro("returning Polygons container of " << m_PolygonsContainer);
  return m_PolygonsContainer.GetPointer();
}


template< typename TPixelType, typename TCellPixel >
auto
PolyData< TPixelType, TCellPixel >
::GetPolygons() const -> const CellsContainer *
{
  itkDebugMacro("returning Polygons container of " << m_PolygonsContainer);
  return m_PolygonsContainer.GetPointer();
}


template< typename TPixelType, typename TCellPixel >
void
PolyData< TPixelType, TCellPixel >
::SetTriangleStrips(CellsContainer *polygons)
{
  itkDebugMacro("setting TriangleStrips container to " << polygons);
  if ( m_TriangleStripsContainer != polygons )
    {
    m_TriangleStripsContainer = polygons;
    this->Modified();
    }
}


template< typename TPixelType, typename TCellPixel >
auto
PolyData< TPixelType, TCellPixel >
::GetTriangleStrips() -> CellsContainer *
{
  itkDebugMacro("Starting GetTriangleStrips()");
  if ( !m_TriangleStripsContainer ) {
    this->SetTriangleStrips( CellsContainer::New() );
    }
  itkDebugMacro("returning TriangleStrips container of " << m_TriangleStripsContainer);
  return m_TriangleStripsContainer.GetPointer();
}


template< typename TPixelType, typename TCellPixel >
auto
PolyData< TPixelType, TCellPixel >
::GetTriangleStrips() const -> const CellsContainer *
{
  itkDebugMacro("returning TriangleStrips container of " << m_TriangleStripsContainer);
  return m_TriangleStripsContainer.GetPointer();
}


template< typename TPixelType, typename TCellPixel >
void
PolyData< TPixelType, TCellPixel >
::SetPointData(PointDataContainer *pointData)
{
  itkDebugMacro("setting PointData container to " << pointData);
  if ( m_PointDataContainer != pointData )
    {
    m_PointDataContainer = pointData;
    this->Modified();
    }
}


template< typename TPixelType, typename TCellPixel >
auto
PolyData< TPixelType, TCellPixel >
::GetPointData() -> PointDataContainer *
{
  if ( !m_PointDataContainer )
    {
    this->SetPointData( PointDataContainer::New() );
    }
  itkDebugMacro("returning PointData container of " << m_PointDataContainer);
  return m_PointDataContainer;
}


template< typename TPixelType, typename TCellPixel >
auto
PolyData< TPixelType, TCellPixel >
::GetPointData() const -> const PointDataContainer *
{
  itkDebugMacro("returning PointData container of "
                << m_PointDataContainer);
  return m_PointDataContainer.GetPointer();
}


template< typename TPixelType, typename TCellPixel >
void
PolyData< TPixelType, TCellPixel >
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


template< typename TPixelType, typename TCellPixel >
bool
PolyData< TPixelType, TCellPixel >
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


template< typename TPixelType, typename TCellPixel >
auto
PolyData< TPixelType, TCellPixel >
::GetPoint(PointIdentifier ptId) const -> PointType
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


template< typename TPixelType, typename TCellPixel >
void
PolyData< TPixelType, TCellPixel >
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


template< typename TPixelType, typename TCellPixel >
bool
PolyData< TPixelType, TCellPixel >
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


template< typename TPixelType, typename TCellPixel >
auto
PolyData< TPixelType, TCellPixel >
::GetNumberOfPoints() const -> PointIdentifier
{
  if ( m_PointsContainer )
    {
    return m_PointsContainer->Size();
    }
  return 0;
}


template< typename TPixelType, typename TCellPixel >
void
PolyData< TPixelType, TCellPixel >
::SetCellData(CellDataContainer *cellData)
{
  itkDebugMacro("setting CellData container to " << cellData);
  if ( m_CellDataContainer != cellData )
    {
    m_CellDataContainer = cellData;
    this->Modified();
    }
}


template< typename TPixelType, typename TCellPixel >
auto
PolyData< TPixelType, TCellPixel >
::GetCellData() -> CellDataContainer *
{
  itkDebugMacro("returning CellData container of "
                << m_CellDataContainer);
  return m_CellDataContainer;
}


template< typename TPixelType, typename TCellPixel >
auto
PolyData< TPixelType, TCellPixel >
::GetCellData() const -> const CellDataContainer *
{
  itkDebugMacro("returning CellData container of "
                << m_CellDataContainer);
  return m_CellDataContainer;
}


template< typename TPixelType, typename TCellPixel >
void
PolyData< TPixelType, TCellPixel >
::SetCellData(CellIdentifier cellId, TCellPixel data)
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


template< typename TPixelType, typename TCellPixel >
bool
PolyData< TPixelType, TCellPixel >
::GetCellData(CellIdentifier cellId, TCellPixel *data) const
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


template< typename TPixelType, typename TCellPixel >
void
PolyData< TPixelType, TCellPixel >
::Initialize()
{
  Superclass::Initialize();

  m_PointsContainer = nullptr;
  m_PointDataContainer = nullptr;
  m_CellDataContainer = nullptr;
}

} // end namespace itk

#endif // itkPolyData_hxx
