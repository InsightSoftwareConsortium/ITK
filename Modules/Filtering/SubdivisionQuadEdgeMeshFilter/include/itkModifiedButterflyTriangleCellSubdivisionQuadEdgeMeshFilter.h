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

#ifndef itkModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilter_h
#define itkModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilter_h

#include "itkTriangleCellSubdivisionQuadEdgeMeshFilter.h"

namespace itk
{
/**
 * \class ModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilter
 * \brief Interpolating subdivision scheme.
 *
 * Similar to LinearTriangleCellSubdivisionQuadEdgeMeshFilter, except that new vertices created using butterfly
 * neighborhood:
 * \f[
 * NV_k = \frac{1}{2} \sum_{i=1}{2} U_k^i + \frac{1}{8} \sum_{i=1}^{2} V_k^i - \frac{1}{16} \sum_{i=1}{4} W_k^i
 * \f]
 *
 * \ingroup SubdivisionQuadEdgeMeshFilter
 */
template <typename TInputMesh, typename TOutputMesh>
class ModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilter
  : public TriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>
{
public:
  typedef ModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilter         Self;
  typedef TriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh> Superclass;
  typedef SmartPointer<Self>                                                 Pointer;
  typedef SmartPointer<const Self>                                           ConstPointer;

  typedef typename Superclass::InputMeshType                InputMeshType;
  typedef typename Superclass::InputMeshPointer             InputMeshPointer;
  typedef typename Superclass::InputMeshConstPointer        InputMeshConstPointer;
  typedef typename Superclass::InputPointsContainerPointer  InputPointsContainerPointer;
  typedef typename Superclass::InputPointsContainerIterator InputPointsContainerIterator;
  typedef typename Superclass::InputPointType               InputPointType;
  typedef typename Superclass::InputVectorType              InputVectorType;
  typedef typename Superclass::InputCoordType               InputCoordType;
  typedef typename Superclass::InputPointIdentifier         InputPointIdentifier;
  typedef typename Superclass::InputCellIdentifier          InputCellIdentifier;
  typedef typename Superclass::InputCellType                InputCellType;
  typedef typename Superclass::InputQEType                  InputQEType;
  typedef typename Superclass::InputMeshTraits              InputMeshTraits;
  typedef typename Superclass::InputPointIdIterator         InputPointIdIterator;

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

  typedef typename Superclass::EdgePointIdentifierContainer              EdgePointIdentifierContainer;
  typedef typename Superclass::EdgePointIdentifierContainerPointer       EdgePointIdentifierContainerPointer;
  typedef typename Superclass::EdgePointIdentifierContainerConstIterator EdgePointIdentifierContainerConstIterator;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(ModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilter, TriangleCellSubdivisionQuadEdgeMeshFilter);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

protected:
  ModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilter() {}
  ~ModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilter() override {}

  void
  AddNewCellPoints(InputCellType * cell) override;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilter);
};
} // namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilter.hxx"
#endif

#endif
