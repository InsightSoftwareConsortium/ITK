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
#ifndef itkLineCell_h
#define itkLineCell_h

#include "itkVertexCell.h"

namespace itk
{
/** \class LineCell
 *  \brief Represents a line segment for a Mesh.
 *
 * LineCell represents a line segment for a Mesh.
 *
 * Template parameters for LineCell:
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
class LineCell:public TCellInterface
{
public:
  /** Standard class typedefs. */
  itkCellCommonTypedefs(LineCell);
  itkCellInheritedTypedefs(TCellInterface);

  /** Standard part of every itk Object. */
  itkTypeMacro(LineCell, CellInterface);

  /** The type of boundary for this lines's vertices. */
  typedef VertexCell< TCellInterface >         VertexType;
  typedef typename VertexType::SelfAutoPointer VertexAutoPointer;

  /** Line-specific topology numbers. */
  enum { NumberOfPoints   = 2,
         NumberOfVertices = 2,
         CellDimension    = 1 };

  /** Implement the standard CellInterface. */
  virtual CellGeometry GetType(void) const
  { return Superclass::LINE_CELL; }
  virtual void MakeCopy(CellAutoPointer &) const;

  virtual unsigned int GetDimension(void) const;

  virtual unsigned int GetNumberOfPoints(void) const;

  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension) const;

  virtual bool GetBoundaryFeature(int dimension, CellFeatureIdentifier, CellAutoPointer &);
  virtual void SetPointIds(PointIdConstIterator first);

  virtual void SetPointIds(PointIdConstIterator first,
                           PointIdConstIterator last);

  virtual void SetPointId(int localId, PointIdentifier);
  virtual PointIdIterator      PointIdsBegin(void);

  virtual PointIdConstIterator PointIdsBegin(void) const;

  virtual PointIdIterator      PointIdsEnd(void);

  virtual PointIdConstIterator PointIdsEnd(void) const;

  /** Line-specific interface. */
  virtual CellFeatureCount GetNumberOfVertices(void) const;

  virtual bool GetVertex(CellFeatureIdentifier, VertexAutoPointer &);

  /** Visitor interface */
  itkCellVisitMacro(Superclass::LINE_CELL);

  LineCell()
  {
    for ( unsigned int i = 0; i < itkGetStaticConstMacro(NumberOfPoints); i++ )
      {
      m_PointIds[i] = NumericTraits< PointIdentifier >::max();
      }
  }

  ~LineCell() {}

protected:
  /** Store number of points needed for a line segment. */
  PointIdentifier m_PointIds[NumberOfPoints];

private:
  LineCell(const Self &);       //purposely not implemented
  void operator=(const Self &); //purposely not implemented
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLineCell.hxx"
#endif

#endif
