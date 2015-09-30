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

#ifndef itkCellAreaTriangleCellSubdivisionCriterion_h
#define itkCellAreaTriangleCellSubdivisionCriterion_h

#include "itkQuadEdgeMeshSubdivisionCriterion.h"
#include "itkObjectFactory.h"
#include "itkTriangleHelper.h"
#include "itkNumericTraits.h"


namespace itk
{
/**
 *\class CellAreaTriangleCellSubdivisionCriterion
 *\brief
 *\ingroup SubdivisionQuadEdgeMeshFilter
 */
template <typename TSubdivisionFilter>
class CellAreaTriangleCellSubdivisionCriterion : public QuadEdgeMeshSubdivisionCriterion<TSubdivisionFilter>
{
public:
  typedef CellAreaTriangleCellSubdivisionCriterion             Self;
  typedef QuadEdgeMeshSubdivisionCriterion<TSubdivisionFilter> Superclass;
  typedef SmartPointer<Self>                                   Pointer;
  typedef SmartPointer<const Self>                             ConstPointer;

  typedef typename Superclass::MeshType                     MeshType;
  typedef typename Superclass::MeshPointer                  MeshPointer;
  typedef typename Superclass::MeshConstPointer             MeshConstPointer;
  typedef typename Superclass::PointsContainerPointer       PointsContainerPointer;
  typedef typename Superclass::PointsContainerConstIterator PointsContainerConstIterator;
  typedef typename Superclass::PointsContainerIterator      PointsContainerIterator;
  typedef typename Superclass::CellsContainer               CellsContainer;
  typedef typename Superclass::CellsContainerPointer        CellsContainerPointer;
  typedef typename Superclass::CellsContainerIterator       CellsContainerIterator;
  typedef typename Superclass::CellsContainerConstIterator  CellsContainerConstIterator;
  typedef typename Superclass::PointType                    PointType;
  typedef typename Superclass::CoordRepType                 CoordRepType;
  typedef typename Superclass::PointIdentifier              PointIdentifier;
  typedef typename Superclass::CellIdentifier               CellIdentifier;
  typedef typename Superclass::CellType                     CellType;
  typedef typename Superclass::QEType                       QEType;
  typedef typename Superclass::PointIdIterator              PointIdIterator;
  typedef typename Superclass::SubdivisionCellContainer     SubdivisionCellContainer;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(CellAreaTriangleCellSubdivisionCriterion, QuadEdgeMeshTriangleCellSubdivisionCriterion);
  itkNewMacro(Self);

  virtual void
  Compute(MeshType * mesh, SubdivisionCellContainer & cellIds) ITK_OVERRIDE;

  itkGetConstMacro(MaximumArea, CoordRepType);
  itkSetMacro(MaximumArea, CoordRepType);

protected:
  CellAreaTriangleCellSubdivisionCriterion() { m_MaximumArea = NumericTraits<CoordRepType>::max(); }
  ~CellAreaTriangleCellSubdivisionCriterion() {}

private:
  CoordRepType m_MaximumArea;
};

} // namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCellAreaTriangleCellSubdivisionCriterion.hxx"
#endif

#endif
