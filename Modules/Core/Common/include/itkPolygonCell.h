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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkPolygonCell_h
#define itkPolygonCell_h

#include "itkLineCell.h"
#include "itkPoint.h"
#include <vector>
#include <deque>

namespace itk
{
/** \class PolygonCell
 *  \brief Represents a polygon in a Mesh.
 *
 * PolygonCell represents a polygon for a Mesh.
 *  the points of the polygon can be dynamically changed.
 *
 * \tparam TPixelType The type associated with a point, cell, or boundary
 * for use in storing its data.
 *
 * \tparam TCellTraits Type information of mesh containing cell.
 *
 * \ingroup MeshObjects
 * \ingroup ITKCommon
 */
template< typename TCellInterface >
class ITK_TEMPLATE_EXPORT PolygonCell:public TCellInterface
{
public:
  /** Standard class typedefs. */
  itkCellCommonTypedefs(PolygonCell);
  itkCellInheritedTypedefs(TCellInterface);

  /** Standard part of every itk Object. */
  itkTypeMacro(PolygonCell, CellInterface);

  /** Save some template parameter information. */
  itkStaticConstMacro(CellDimension, unsigned int, 2);

  /** The type of boundary for this triangle's vertices. */
  typedef VertexCell< TCellInterface >         VertexType;
  typedef typename VertexType::SelfAutoPointer VertexAutoPointer;

  /** The type of boundary for this triangle's edges. */
  typedef LineCell< TCellInterface >         EdgeType;
  typedef typename EdgeType::SelfAutoPointer EdgeAutoPointer;

  typedef FixedArray< int, 2 >   EdgeInfo;
  typedef std::deque< EdgeInfo > EdgeInfoDQ;

  /** Need to add POLYGON_CELL into CellInterface. */
  itkCellVisitMacro(Superclass::POLYGON_CELL);

  /** Implement the standard CellInterface. */
  virtual CellGeometry GetType(void) const ITK_OVERRIDE
  { return Superclass::POLYGON_CELL; }
  virtual void MakeCopy(CellAutoPointer &) const ITK_OVERRIDE;

  virtual unsigned int GetDimension(void) const ITK_OVERRIDE;

  virtual unsigned int GetNumberOfPoints(void) const ITK_OVERRIDE;

  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension) const ITK_OVERRIDE;

  virtual bool GetBoundaryFeature(int dimension, CellFeatureIdentifier, CellAutoPointer &) ITK_OVERRIDE;

  virtual void SetPointIds(PointIdConstIterator first) ITK_OVERRIDE;

  virtual void SetPointIds(PointIdConstIterator first,
                           PointIdConstIterator last) ITK_OVERRIDE;

  void AddPointId(PointIdentifier);
  void RemovePointId(PointIdentifier);
  void SetPointIds(int dummy, int num, PointIdConstIterator first);

  void BuildEdges();

  void ClearPoints();

  virtual void SetPointId(int localId, PointIdentifier) ITK_OVERRIDE;
  virtual PointIdIterator      PointIdsBegin(void) ITK_OVERRIDE;

  virtual PointIdConstIterator PointIdsBegin(void) const ITK_OVERRIDE;

  virtual PointIdIterator      PointIdsEnd(void) ITK_OVERRIDE;

  virtual PointIdConstIterator PointIdsEnd(void) const ITK_OVERRIDE;

  /** Polygon-specific interface. */
  virtual CellFeatureCount GetNumberOfVertices() const;

  virtual CellFeatureCount GetNumberOfEdges() const;

  virtual bool GetVertex(CellFeatureIdentifier, VertexAutoPointer &);
  virtual bool GetEdge(CellFeatureIdentifier, EdgeAutoPointer &);

  /** Constructor and destructor */
  PolygonCell() {}
  PolygonCell(PointIdentifier NumberOfPoints)
  {
    for ( PointIdentifier i = 0; i < NumberOfPoints; i++ )
      {
      m_PointIds.push_back( NumericTraits< PointIdentifier >::max() );
      }
    this->BuildEdges();
  }

  ~PolygonCell() ITK_OVERRIDE {}

protected:
  std::vector< EdgeInfo >        m_Edges;
  std::vector< PointIdentifier > m_PointIds;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PolygonCell);
};
} //end namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPolygonCell.hxx"
#endif

#endif
