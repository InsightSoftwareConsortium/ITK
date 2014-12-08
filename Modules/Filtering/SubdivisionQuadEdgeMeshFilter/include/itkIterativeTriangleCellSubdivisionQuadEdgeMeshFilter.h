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

#ifndef __itkIterativeTriangleCellSubdivisionQuadEdgeMeshFilter_h
#define __itkIterativeTriangleCellSubdivisionQuadEdgeMeshFilter_h

#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.h"

namespace itk
{
/**
 * \class IterativeTriangleCellSubdivisionQuadEdgeMeshFilter
 *
 * \brief FIXME
 * \ingroup itkSubdivisionQuadEdgeMeshFilter
 */
template <typename TInputMesh, typename TCellSubdivisionFilter>
class IterativeTriangleCellSubdivisionQuadEdgeMeshFilter
  : public QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, typename TCellSubdivisionFilter::OutputMeshType>
{
public:
  typedef IterativeTriangleCellSubdivisionQuadEdgeMeshFilter                                            Self;
  typedef QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, typename TCellSubdivisionFilter::OutputMeshType> Superclass;
  typedef SmartPointer<Self>                                                                            Pointer;
  typedef SmartPointer<const Self>                                                                      ConstPointer;

  typedef TCellSubdivisionFilter                      CellSubdivisionFilterType;
  typedef typename CellSubdivisionFilterType::Pointer CellSubdivisionFilterPointer;

  typedef TInputMesh                      InputMeshType;
  typedef typename InputMeshType::Pointer InputMeshPointer;

  typedef typename CellSubdivisionFilterType::OutputMeshType OutputMeshType;
  typedef typename OutputMeshType::Pointer                   OutputMeshPointer;

  typedef typename CellSubdivisionFilterType::OutputCellIdentifier     OutputCellIdentifier;
  typedef typename CellSubdivisionFilterType::SubdivisionCellContainer SubdivisionCellContainer;
  typedef
    typename CellSubdivisionFilterType::SubdivisionCellContainerConstIterator SubdivisionCellContainerConstIterator;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(IterativeTriangleCellSubdivisionQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);
  itkNewMacro(Self);

#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro(SameTypeCheck,
                  (Concept::SameType<typename CellSubdivisionFilterType::InputMeshType,
                                     typename CellSubdivisionFilterType::OutputMeshType>));
#endif

  itkSetMacro(ResolutionLevels, unsigned int);
  itkGetConstMacro(ResolutionLevels, unsigned int);
  itkGetConstReferenceMacro(CellsToBeSubdivided, SubdivisionCellContainer);

  void
  SetCellsToBeSubdivided(const SubdivisionCellContainer & cellIdList);
  void
  AddSubdividedCellId(OutputCellIdentifier cellId);

protected:
  IterativeTriangleCellSubdivisionQuadEdgeMeshFilter();

  virtual ~IterativeTriangleCellSubdivisionQuadEdgeMeshFilter() {}

  virtual void
  GenerateData();

  void
  PrintSelf(std::ostream & os, Indent indent) const;

  CellSubdivisionFilterPointer m_CellSubdivisionFilter;
  SubdivisionCellContainer     m_CellsToBeSubdivided;
  unsigned int                 m_ResolutionLevels;

private:
  IterativeTriangleCellSubdivisionQuadEdgeMeshFilter(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkIterativeTriangleCellSubdivisionQuadEdgeMeshFilter.hxx"
#endif

#endif
