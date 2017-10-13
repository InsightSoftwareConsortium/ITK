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
#ifndef itkQuadraticTriangleCell_h
#define itkQuadraticTriangleCell_h

#include "itkQuadraticEdgeCell.h"
#include "itkQuadraticTriangleCellTopology.h"

namespace itk
{
/** \class QuadraticTriangleCell
 *  \brief Represents a second order triangular patch for a Mesh.
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
class ITK_TEMPLATE_EXPORT QuadraticTriangleCell:public TCellInterface, private QuadraticTriangleCellTopology
{
public:
  /** Standard class typedefs. */
  itkCellCommonTypedefs(QuadraticTriangleCell);
  itkCellInheritedTypedefs(TCellInterface);

  /** Standard part of every itk Object. */
  itkTypeMacro(QuadraticTriangleCell, CellInterface);

  /** The type of boundary for this triangle's vertices. */
  typedef VertexCell< TCellInterface >         VertexType;
  typedef typename VertexType::SelfAutoPointer VertexAutoPointer;

  /** The type of boundary for this triangle's edges. */
  typedef QuadraticEdgeCell< TCellInterface > EdgeType;
  typedef typename EdgeType::SelfAutoPointer  EdgeAutoPointer;

  /** Triangle-specific topology numbers. */
  itkStaticConstMacro(NumberOfPoints, unsigned int, 6);
  itkStaticConstMacro(NumberOfVertices, unsigned int, 3);
  itkStaticConstMacro(NumberOfEdges, unsigned int, 3);
  itkStaticConstMacro(CellDimension, unsigned int, 2);

  /** Implement the standard CellInterface. */
  virtual CellGeometry GetType(void) const ITK_OVERRIDE
  { return Superclass::QUADRATIC_TRIANGLE_CELL; }
  virtual void MakeCopy(CellAutoPointer &) const ITK_OVERRIDE;

  virtual unsigned int GetDimension(void) const ITK_OVERRIDE;

  virtual unsigned int GetNumberOfPoints(void) const ITK_OVERRIDE;

  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension) const ITK_OVERRIDE;

  virtual bool GetBoundaryFeature(int dimension, CellFeatureIdentifier, CellAutoPointer &) ITK_OVERRIDE;
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

  /** Cell visitor interface. */
  itkCellVisitMacro(Superclass::QUADRATIC_TRIANGLE_CELL);

  /** Given the parametric coordinates of a point in the cell
   *  determine the value of its Shape Functions
   *  returned through an itkArray<InterpolationWeightType>).  */
  virtual void EvaluateShapeFunctions(
    const ParametricCoordArrayType & parametricCoordinates,
    ShapeFunctionsArrayType  & weights) const ITK_OVERRIDE;

public:
  QuadraticTriangleCell()
  {
    for ( PointIdentifier i = 0; i < itkGetStaticConstMacro(NumberOfPoints); i++ )
      {
      m_PointIds[i] = NumericTraits< PointIdentifier >::max();
      }
  }

  ~QuadraticTriangleCell() ITK_OVERRIDE {}

protected:
  /** Store the number of points needed for a triangle. */
  PointIdentifier m_PointIds[NumberOfPoints];

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuadraticTriangleCell);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuadraticTriangleCell.hxx"
#endif

#endif
