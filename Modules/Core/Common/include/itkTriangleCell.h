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
#ifndef itkTriangleCell_h
#define itkTriangleCell_h

#include "itkLineCell.h"
#include "itkTriangleCellTopology.h"

#include <vector>

namespace itk
{
/** \class TriangleCell
 * TriangleCell represents a triangle for a Mesh.
 *
 * Template parameters for TriangleCell:
 *
 * TPixelType =
 *     The type associated with a point, cell, or boundary for use in storing
 *     its data.
 *
 * TCellTraits =
 *     Type information of mesh containing cell.
 *
 * \ingroup MeshObjects
 * \ingroup ITKCommon
 */

template< typename TCellInterface >
class ITK_TEMPLATE_EXPORT TriangleCell:
  public TCellInterface, private TriangleCellTopology
{
public:
  /** Standard class typedefs. */
  itkCellCommonTypedefs(TriangleCell);
  itkCellInheritedTypedefs(TCellInterface);

  /** Standard part of every itk Object. */
  itkTypeMacro(TriangleCell, CellInterface);

  /** The type of boundary for this triangle's vertices. */
  typedef VertexCell< TCellInterface >         VertexType;
  typedef typename VertexType::SelfAutoPointer VertexAutoPointer;

  /** The type of boundary for this triangle's edges. */
  typedef LineCell< TCellInterface >         EdgeType;
  typedef typename EdgeType::SelfAutoPointer EdgeAutoPointer;

  /** Triangle-specific topology numbers. */
  itkStaticConstMacro(NumberOfPoints, unsigned int, 3);
  itkStaticConstMacro(NumberOfVertices, unsigned int, 3);
  itkStaticConstMacro(NumberOfEdges, unsigned int, 3);
  itkStaticConstMacro(CellDimension, unsigned int, 2);

  /** Implement the standard CellInterface. */
  virtual CellGeometry GetType(void) const ITK_OVERRIDE
  { return Superclass::TRIANGLE_CELL; }
  virtual void MakeCopy(CellAutoPointer &) const ITK_OVERRIDE;

  virtual unsigned int GetDimension(void) const ITK_OVERRIDE;

  virtual unsigned int GetNumberOfPoints(void) const ITK_OVERRIDE;

  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension) const ITK_OVERRIDE;

  virtual bool GetBoundaryFeature(int dimension, CellFeatureIdentifier,
                                  CellAutoPointer &) ITK_OVERRIDE;
  virtual void SetPointIds(PointIdConstIterator first) ITK_OVERRIDE;

  virtual void SetPointIds(PointIdConstIterator first,
                           PointIdConstIterator last) ITK_OVERRIDE;

  virtual void SetPointId(int localId, PointIdentifier) ITK_OVERRIDE;
  virtual PointIdIterator      PointIdsBegin(void) ITK_OVERRIDE;

  virtual PointIdConstIterator PointIdsBegin(void) const ITK_OVERRIDE;

  virtual PointIdIterator      PointIdsEnd(void) ITK_OVERRIDE;

  virtual PointIdConstIterator PointIdsEnd(void) const ITK_OVERRIDE;

  /** Triangle-specific interface. */
  virtual CellFeatureCount GetNumberOfVertices() const;

  virtual CellFeatureCount GetNumberOfEdges() const;

  virtual bool GetVertex(CellFeatureIdentifier, VertexAutoPointer &);
  virtual bool GetEdge(CellFeatureIdentifier, EdgeAutoPointer &);

  virtual bool EvaluatePosition(CoordRepType *,
                                PointsContainer *,
                                CoordRepType *,
                                CoordRepType[],
                                double *,
                                InterpolationWeightType *) ITK_OVERRIDE;

  /** Cell visitor interface. */
  itkCellVisitMacro(Superclass::TRIANGLE_CELL);

  /** \brief Compute Area to a TriangleCell given a PointsContainer.  */
  CoordRepType ComputeArea(PointsContainer *);

  PointType ComputeBarycenter(CoordRepType *,
                              PointsContainer *);

  PointType ComputeCenterOfGravity(PointsContainer *);

  PointType ComputeCircumCenter(PointsContainer *);

public:
  TriangleCell():
    m_PointIds( NumberOfPoints, NumericTraits< PointIdentifier >::max() )
  {}
  ~TriangleCell() ITK_OVERRIDE {}

protected:
  /** Store the number of points needed for a triangle. */
  std::vector< PointIdentifier > m_PointIds;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(TriangleCell);

  /** Computes the SQUARED distance between a point and a line segment defined
   * by two other points */
  double DistanceToLine(PointType x, PointType p1, PointType p2,
                        double & t, CoordRepType *closestPoint);

  double DistanceToLine(PointType x, PointType p1, PointType p2,
                        double & t, PointType & closestPoint);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTriangleCell.hxx"
#endif

#endif
