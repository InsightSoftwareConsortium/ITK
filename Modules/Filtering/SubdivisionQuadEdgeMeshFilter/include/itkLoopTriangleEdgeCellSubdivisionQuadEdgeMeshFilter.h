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

#ifndef __itkLoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilter_h
#define __itkLoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilter_h

#include "itkTriangleEdgeCellSubdivisionQuadEdgeMeshFilter.h"

namespace itk
{
/**
 * \class LoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilter
 *
 * \brief FIXME     Add documentation here
 * \ingroup itkSubdivisionQuadEdgeMeshFilter
 */
template <typename TInputMesh, typename TOutputMesh>
class LoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilter
  : public TriangleEdgeCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>
{
public:
  typedef LoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilter                      Self;
  typedef TriangleEdgeCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh> Superclass;
  typedef SmartPointer<Self>                                                     Pointer;
  typedef SmartPointer<const Self>                                               ConstPointer;

  typedef typename Superclass::InputMeshType                     InputMeshType;
  typedef typename Superclass::InputMeshPointer                  InputMeshPointer;
  typedef typename Superclass::InputMeshConstPointer             InputMeshConstPointer;
  typedef typename Superclass::InputPointsContainer              InputPointsContainer;
  typedef typename Superclass::InputPointsContainerPointer       InputPointsContainerPointer;
  typedef typename Superclass::InputPointsContainerConstIterator InputPointsContainerConstIterator;
  typedef typename Superclass::InputPointsContainerIterator      InputPointsContainerIterator;
  typedef typename Superclass::InputCellsContainer               InputCellsContainer;
  typedef typename Superclass::InputCellsContainerPointer        InputCellsContainerPointer;
  typedef typename Superclass::InputCellsContainerIterator       InputCellsContainerIterator;
  typedef typename Superclass::InputCellsContainerConstIterator  InputCellsContainerConstIterator;
  typedef typename Superclass::InputPointType                    InputPointType;
  typedef typename Superclass::InputCoordRepType                 InputCoordType;
  typedef typename Superclass::InputPointIdentifier              InputPointIdentifier;
  typedef typename Superclass::InputCellIdentifier               InputCellIdentifier;
  typedef typename Superclass::InputCellType                     InputCellType;
  typedef typename Superclass::InputQEType                       InputQEType;
  typedef typename Superclass::InputMeshTraits                   InputMeshTraits;
  typedef typename Superclass::InputPointIdIterator              InputPointIdIterator;

  typedef typename Superclass::OutputMeshType                OutputMeshType;
  typedef typename Superclass::OutputMeshPointer             OutputMeshPointer;
  typedef typename Superclass::OutputPointsContainerPointer  OutputPointsContainerPointer;
  typedef typename Superclass::OutputPointsContainerIterator OutputPointsContainerIterator;
  typedef typename Superclass::OutputPointType               OutputPointType;
  typedef typename Superclass::OutputVectorType              OutputVectorType;
  typedef typename Superclass::OutputCoordType               OutputCoordType;
  typedef typename Superclass::OutputPointIdentifier         OutputPointIdentifier;
  typedef typename Superclass::OutputCellIdentifier          OutputCellIdentifier;
  typedef typename Superclass::OutputCellType                OutputCellType;
  typedef typename Superclass::OutputQEType                  OutputQEType;
  typedef typename Superclass::OutputMeshTraits              OutputMeshTraits;
  typedef typename Superclass::OutputPointIdIterator         OutputPointIdIterator;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(LoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilter, TriangleEdgeCellSubdivisionQuadEdgeMeshFilter);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

protected:
  LoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilter() {}
  virtual ~LoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilter() {}

  virtual void
  AddNewEdgePoints(InputQEType * edge);

  virtual void
  CopyInputMeshToOutputMeshPoints();

  virtual void
  AverageOriginOfEdge(InputQEType * edge, const InputPointsContainer * points);

private:
  LoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilter(const Self &); // purposely not implement
  void
  operator=(const Self &); // purposely not implement
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilter.hxx"
#endif

#endif
