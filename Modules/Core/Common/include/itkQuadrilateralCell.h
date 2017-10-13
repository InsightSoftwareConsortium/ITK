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
#ifndef itkQuadrilateralCell_h
#define itkQuadrilateralCell_h

#include "itkLineCell.h"
#include "itkQuadrilateralCellTopology.h"

namespace itk
{
/** \class QuadrilateralCell
 *  \brief Represents a quadrilateral for a Mesh.
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
class ITK_TEMPLATE_EXPORT QuadrilateralCell:public TCellInterface, private QuadrilateralCellTopology
{
public:
  /** Standard class typedefs. */
  itkCellCommonTypedefs(QuadrilateralCell);
  itkCellInheritedTypedefs(TCellInterface);

  /** Standard part of every itk Object. */
  itkTypeMacro(QuadrilateralCell, CellInterface);

  /** The type of boundary for this triangle's vertices. */
  typedef VertexCell< TCellInterface >         VertexType;
  typedef typename VertexType::SelfAutoPointer VertexAutoPointer;

  /** The type of boundary for this triangle's edges. */
  typedef LineCell< TCellInterface >         EdgeType;
  typedef typename EdgeType::SelfAutoPointer EdgeAutoPointer;

  /** Quadrilateral-specific topology numbers. */
  itkStaticConstMacro(NumberOfPoints, unsigned int, 4);
  itkStaticConstMacro(NumberOfVertices, unsigned int, 4);
  itkStaticConstMacro(NumberOfEdges, unsigned int, 4);
  itkStaticConstMacro(CellDimension, unsigned int, 2);
  itkStaticConstMacro(NumberOfDerivatives, unsigned int, 8);

  /** Implement the standard CellInterface. */
  virtual CellGeometry GetType(void) const ITK_OVERRIDE
  { return Superclass::QUADRILATERAL_CELL; }
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

  /** Quadrilateral-specific interface. */
  virtual CellFeatureCount GetNumberOfVertices() const;

  virtual CellFeatureCount GetNumberOfEdges() const;

  virtual bool GetVertex(CellFeatureIdentifier, VertexAutoPointer &);
  virtual bool GetEdge(CellFeatureIdentifier, EdgeAutoPointer &);

  /** Evaluate the position inside the cell */
  virtual bool EvaluatePosition(CoordRepType * position,
                                PointsContainer * points,
                                CoordRepType * closestPoint,
                                CoordRepType[CellDimension],
                                double * dist2,
                                InterpolationWeightType * weight) ITK_OVERRIDE;

  /** Visitor interface */
  itkCellVisitMacro(Superclass::QUADRILATERAL_CELL);

  /** Constructor and destructor */
  QuadrilateralCell()
  {
    for ( PointIdentifier i = 0; i < itkGetStaticConstMacro(NumberOfPoints); i++ )
      {
      m_PointIds[i] = NumericTraits< PointIdentifier >::max();
      }
  }

  ~QuadrilateralCell() ITK_OVERRIDE {}

protected:
  /** Store the number of points needed for a quadrilateral. */
  PointIdentifier m_PointIds[NumberOfPoints];

  void InterpolationDerivs(const CoordRepType pointCoords[CellDimension], CoordRepType derivs[NumberOfDerivatives]);
  void InterpolationFunctions(const CoordRepType pointCoords[CellDimension], InterpolationWeightType weights[NumberOfPoints]);
  void EvaluateLocation(int &itkNotUsed(subId), const PointsContainer * points, const CoordRepType pointCoords[PointDimension],
                        CoordRepType x[PointDimension], InterpolationWeightType * weights);

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuadrilateralCell);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuadrilateralCell.hxx"
#endif

#endif
