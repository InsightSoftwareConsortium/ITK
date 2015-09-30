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

#ifndef itkSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter_h
#define itkSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter_h

#include "itkTriangleCellSubdivisionQuadEdgeMeshFilter.h"

namespace itk
{
/**
 * \class SquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter
 *
 * \brief FIXME     Add documentation here
 * \ingroup SubdivisionQuadEdgeMeshFilter
 */
template <class TInputMesh, class TOutputMesh>
class SquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter
  : public TriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>
{
public:
  typedef SquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter               Self;
  typedef TriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh> Superclass;
  typedef SmartPointer<Self>                                                 Pointer;
  typedef SmartPointer<const Self>                                           ConstPointer;

  typedef typename Superclass::InputMeshType                    InputMeshType;
  typedef typename Superclass::InputMeshPointer                 InputMeshPointer;
  typedef typename Superclass::InputMeshConstPointer            InputMeshConstPointer;
  typedef typename Superclass::InputPointsContainerPointer      InputPointsContainerPointer;
  typedef typename Superclass::InputPointsContainerIterator     InputPointsContainerIterator;
  typedef typename Superclass::InputCellsContainer              InputCellsContainer;
  typedef typename Superclass::InputCellsContainerPointer       InputCellsContainerPointer;
  typedef typename Superclass::InputCellsContainerIterator      InputCellsContainerIterator;
  typedef typename Superclass::InputCellsContainerConstIterator InputCellsContainerConstIterator;
  typedef typename Superclass::InputPointType                   InputPointType;
  typedef typename Superclass::InputVectorType                  InputVectorType;
  typedef typename Superclass::InputCoordType                   InputCoordType;
  typedef typename Superclass::InputPointIdentifier             InputPointIdentifier;
  typedef typename Superclass::InputCellIdentifier              InputCellIdentifier;
  typedef typename Superclass::InputCellType                    InputCellType;
  typedef typename Superclass::InputQEType                      InputQEType;
  typedef typename Superclass::InputMeshTraits                  InputMeshTraits;
  typedef typename Superclass::InputPointIdIterator             InputPointIdIterator;

  typedef typename Superclass::OutputMeshType                OutputMeshType;
  typedef typename Superclass::OutputMeshPointer             OutputMeshPointer;
  typedef typename Superclass::OutputPointsContainerPointer  OutputPointsContainerPointer;
  typedef typename Superclass::OutputPointsContainerIterator OutputPointsContainerIterator;
  typedef typename Superclass::OutputCellsContainerPointer   OutputCellsContainerPointer;
  typedef typename Superclass::OutputCellsContainerIterator  OutputCellsContainerIterator;
  typedef typename Superclass::OutputPointType               OutputPointType;
  typedef typename Superclass::OutputVectorType              OutputVectorType;
  typedef typename Superclass::OutputCoordType               OutputCoordType;
  typedef typename Superclass::OutputPointIdentifier         OutputPointIdentifier;
  typedef typename Superclass::OutputCellIdentifier          OutputCellIdentifier;
  typedef typename Superclass::OutputCellType                OutputCellType;
  typedef typename Superclass::OutputQEType                  OutputQEType;
  typedef typename Superclass::OutputMeshTraits              OutputMeshTraits;
  typedef typename Superclass::OutputPointIdIterator         OutputPointIdIterator;

  typedef typename Superclass::EdgePointIdentifierContainer              EdgePointIdentifierContainer;
  typedef typename Superclass::EdgePointIdentifierContainerPointer       EdgePointIdentifierContainerPointer;
  typedef typename Superclass::EdgePointIdentifierContainerIterator      EdgePointIdentifierContainerIterator;
  typedef typename Superclass::EdgePointIdentifierContainerConstIterator EdgePointIdentifierContainerConstIterator;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(SquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter, TriangleCellSubdivisionQuadEdgeMeshFilter);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

protected:
  SquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter() {}
  virtual ~SquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter() {}

  virtual void
  AddNewCellPoints(InputCellType * cell) ITK_OVERRIDE;
  virtual void
  GenerateOutputCells() ITK_OVERRIDE;

private:
  SquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter(const Self &);
  void
  operator=(const Self &);
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter.hxx"
#endif

#endif
