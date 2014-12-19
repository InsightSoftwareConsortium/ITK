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
class PolygonCell:public TCellInterface
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
  virtual CellGeometry GetType(void) const
  { return Superclass::POLYGON_CELL; }
  virtual void MakeCopy(CellAutoPointer &) const;

  virtual unsigned int GetDimension(void) const;

  virtual unsigned int GetNumberOfPoints(void) const;

  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension) const;

  virtual bool GetBoundaryFeature(int dimension, CellFeatureIdentifier, CellAutoPointer &);

  virtual void SetPointIds(PointIdConstIterator first);

  virtual void SetPointIds(PointIdConstIterator first,
                           PointIdConstIterator last);

  void AddPointId(PointIdentifier);
  void RemovePointId(PointIdentifier);
  void SetPointIds(int dummy, int num, PointIdConstIterator first);

  void BuildEdges(void);

  void ClearPoints(void);

  virtual void SetPointId(int localId, PointIdentifier);
  virtual PointIdIterator      PointIdsBegin(void);

  virtual PointIdConstIterator PointIdsBegin(void) const;

  virtual PointIdIterator      PointIdsEnd(void);

  virtual PointIdConstIterator PointIdsEnd(void) const;

  /** Polygon-specific interface. */
  virtual CellFeatureCount GetNumberOfVertices(void) const;

  virtual CellFeatureCount GetNumberOfEdges(void) const;

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

  ~PolygonCell() {}

protected:
  std::vector< EdgeInfo >        m_Edges;
  std::vector< PointIdentifier > m_PointIds;

private:
  PolygonCell(const Self &);    //purposely not implemented
  void operator=(const Self &); //purposely not implemented
};
} //end namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPolygonCell.hxx"
#endif

#endif
