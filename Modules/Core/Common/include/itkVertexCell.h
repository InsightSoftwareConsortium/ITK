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
#ifndef __itkVertexCell_h
#define __itkVertexCell_h

#include "itkCellInterface.h"
#include "itkNumericTraits.h"

namespace itk
{
/** \class VertexCell
 *  \brief Represents a single vertex for a Mesh.
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
class VertexCell:public TCellInterface
{
public:
  /** Standard class typedefs. */
  itkCellCommonTypedefs(VertexCell);
  itkCellInheritedTypedefs(TCellInterface);

  /** Standard part of every itk Object. */
  itkTypeMacro(VertexCell, CellInterface);

  /** Vertex-specific topology numbers. */
  itkStaticConstMacro(NumberOfPoints, unsigned int, 1);
  itkStaticConstMacro(CellDimension, unsigned int, 0);

  /** Implement the standard CellInterface. */
  virtual CellGeometry GetType(void) const
  { return Superclass::VERTEX_CELL; }
  virtual void MakeCopy(CellAutoPointer &) const;

  virtual unsigned int GetDimension(void) const;

  virtual unsigned int GetNumberOfPoints(void) const;

  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension) const;

  virtual bool GetBoundaryFeature(int dimension, CellFeatureIdentifier,
                                  CellAutoPointer &);
  virtual void SetPointIds(PointIdConstIterator first);

  virtual void SetPointIds(PointIdConstIterator first,
                           PointIdConstIterator last);

  virtual void SetPointId(int localId, PointIdentifier);
  virtual PointIdIterator      PointIdsBegin(void);

  virtual PointIdConstIterator PointIdsBegin(void) const;

  virtual PointIdIterator      PointIdsEnd(void);

  virtual PointIdConstIterator PointIdsEnd(void) const;

  /** Vertex-specific interface. */
  virtual void SetPointId(PointIdentifier);
  virtual PointIdentifier GetPointId(void);

  /** Cell visitor interface */
  itkCellVisitMacro(Superclass::VERTEX_CELL);

  /** Evaluate the position of a given point */
  virtual bool EvaluatePosition(CoordRepType *,
                                PointsContainer *,
                                CoordRepType *,
                                CoordRepType[],
                                double *,
                                InterpolationWeightType *);

public:
  VertexCell()
  {
    for ( PointIdentifier i = 0; i < itkGetStaticConstMacro(NumberOfPoints); i++ )
      {
      m_PointIds[i] = NumericTraits< PointIdentifier >::max();
      }
  }

  ~VertexCell() {}

protected:
  /**
   * Store the number of points needed for a vertex.
   */
  PointIdentifier m_PointIds[NumberOfPoints];

private:
  VertexCell(const Self &);     //purposely not implemented
  void operator=(const Self &); //purposely not implemented
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVertexCell.hxx"
#endif

#endif
