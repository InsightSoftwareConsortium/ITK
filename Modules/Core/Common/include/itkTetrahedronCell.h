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
#ifndef itkTetrahedronCell_h
#define itkTetrahedronCell_h

#include "itkTriangleCell.h"
#include "itkTetrahedronCellTopology.h"

namespace itk
{
/** \class TetrahedronCell
 *  \brief TetrahedronCell represents a tetrahedron for a Mesh.
 *
 * \tparam TPixelType The type associated with a point, cell, or boundary
 * for use in storing its data.
 *
 * \tparam TCellTraits Type information of mesh containing cell.
 * \ingroup MeshObjects
 * \ingroup ITKCommon
 */
template< typename TCellInterface >
class ITK_TEMPLATE_EXPORT TetrahedronCell:public TCellInterface, private TetrahedronCellTopology
{
public:
  /** Standard class typedefa. */
  itkCellCommonTypedefs(TetrahedronCell);
  itkCellInheritedTypedefs(TCellInterface);

  /** Standard part of every itk Object. */
  itkTypeMacro(TetrahedronCell, CellInterface);

  /** The type of boundary for this triangle's vertices. */
  typedef VertexCell< TCellInterface >         VertexType;
  typedef typename VertexType::SelfAutoPointer VertexAutoPointer;

  /** The type of boundary for this triangle's edges. */
  typedef LineCell< TCellInterface >         EdgeType;
  typedef typename EdgeType::SelfAutoPointer EdgeAutoPointer;

  /** The type of boundary for this hexahedron's faces. */
  typedef TriangleCell< TCellInterface >     FaceType;
  typedef typename FaceType::SelfAutoPointer FaceAutoPointer;

  /** Tetrahedron-specific topology numbers. */
  itkStaticConstMacro(NumberOfPoints, unsigned int, 4);
  itkStaticConstMacro(NumberOfVertices, unsigned int, 4);
  itkStaticConstMacro(NumberOfEdges, unsigned int, 6);
  itkStaticConstMacro(NumberOfFaces, unsigned int, 4);
  itkStaticConstMacro(CellDimension, unsigned int, 3);

  /** Implement the standard CellInterface. */
  virtual CellGeometry GetType(void) const ITK_OVERRIDE
  { return Superclass::TETRAHEDRON_CELL; }
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

  /** Tetrahedron-specific interface. */
  virtual CellFeatureCount GetNumberOfVertices() const;

  virtual CellFeatureCount GetNumberOfEdges() const;

  virtual CellFeatureCount GetNumberOfFaces() const;

  virtual bool GetVertex(CellFeatureIdentifier, VertexAutoPointer &);
  virtual bool GetEdge(CellFeatureIdentifier, EdgeAutoPointer &);
  virtual bool GetFace(CellFeatureIdentifier, FaceAutoPointer &);

  /** Visitor interface. */
  itkCellVisitMacro(Superclass::TETRAHEDRON_CELL);

  virtual bool EvaluatePosition(CoordRepType *,
                                PointsContainer *,
                                CoordRepType *,
                                CoordRepType[],
                                double *,
                                InterpolationWeightType *) ITK_OVERRIDE;

public:
  TetrahedronCell()
  {
    for ( PointIdentifier i = 0; i < itkGetStaticConstMacro(NumberOfPoints); i++ )
      {
      m_PointIds[i] = NumericTraits< PointIdentifier >::max();
      }
  }

  ~TetrahedronCell() ITK_OVERRIDE {}

protected:
  /** Store the number of points needed for a tetrahedron. */
  PointIdentifier m_PointIds[NumberOfPoints];

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(TetrahedronCell);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTetrahedronCell.hxx"
#endif

#endif
