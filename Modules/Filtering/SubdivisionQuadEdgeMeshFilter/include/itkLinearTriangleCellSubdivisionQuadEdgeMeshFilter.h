/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#ifndef itkLinearTriangleCellSubdivisionQuadEdgeMeshFilter_h
#define itkLinearTriangleCellSubdivisionQuadEdgeMeshFilter_h

#include "itkTriangleCellSubdivisionQuadEdgeMeshFilter.h"

namespace itk
{

/** \class LinearTriangleCellSubdivisionQuadEdgeMeshFilter
 * \brief It is the simplest interpolating subdivision scheme, the new vertices
 * are defined as the middle point of each edge in a triangle.
 *
 * Each triangle is split into 4 new triangles.
 *
 * \ingroup SubdivisionQuadEdgeMeshFilter
 */
template <typename TInputMesh, typename TOutputMesh>
class LinearTriangleCellSubdivisionQuadEdgeMeshFilter
  : public TriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LinearTriangleCellSubdivisionQuadEdgeMeshFilter);

  using Self = LinearTriangleCellSubdivisionQuadEdgeMeshFilter;
  using Superclass = TriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using InputMeshType = typename Superclass::InputMeshType;
  using InputMeshPointer = typename Superclass::InputMeshPointer;
  using InputMeshConstPointer = typename Superclass::InputMeshConstPointer;
  using InputPointsContainerPointer = typename Superclass::InputPointsContainerPointer;
  using InputPointsContainerIterator = typename Superclass::InputPointsContainerIterator;
  using InputPointType = typename Superclass::InputPointType;
  using InputVectorType = typename Superclass::InputVectorType;
  using InputCoordType = typename Superclass::InputCoordType;
  using InputPointIdentifier = typename Superclass::InputPointIdentifier;
  using InputCellIdentifier = typename Superclass::InputCellIdentifier;
  using InputCellType = typename Superclass::InputCellType;
  using InputQEType = typename Superclass::InputQEType;
  using InputMeshTraits = typename Superclass::InputMeshTraits;
  using InputPointIdIterator = typename Superclass::InputPointIdIterator;

  using OutputMeshType = typename Superclass::OutputMeshType;
  using OutputMeshPointer = typename Superclass::OutputMeshPointer;
  using OutputPointsContainerPointer = typename Superclass::OutputPointsContainerPointer;
  using OutputPointsContainerIterator = typename Superclass::OutputPointsContainerIterator;
  using OutputPointType = typename Superclass::OutputPointType;
  using OutputVectorType = typename Superclass::OutputVectorType;
  using OutputCoordType = typename Superclass::OutputCoordType;
  using OutputPointIdentifier = typename Superclass::OutputPointIdentifier;
  using OutputCellIdentifier = typename Superclass::OutputCellIdentifier;
  using OutputCellType = typename Superclass::OutputCellType;
  using OutputQEType = typename Superclass::OutputQEType;
  using OutputMeshTraits = typename Superclass::OutputMeshTraits;
  using OutputPointIdIterator = typename Superclass::OutputPointIdIterator;

  using EdgePointIdentifierContainer = typename Superclass::EdgePointIdentifierContainer;
  using EdgePointIdentifierContainerPointer = typename Superclass::EdgePointIdentifierContainerPointer;
  using EdgePointIdentifierContainerConstIterator = typename Superclass::EdgePointIdentifierContainerConstIterator;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(LinearTriangleCellSubdivisionQuadEdgeMeshFilter, TriangleCellSubdivisionQuadEdgeMeshFilter);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

protected:
  LinearTriangleCellSubdivisionQuadEdgeMeshFilter() = default;
  ~LinearTriangleCellSubdivisionQuadEdgeMeshFilter() override = default;

  void
  AddNewCellPoints(InputCellType * cell) override;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLinearTriangleCellSubdivisionQuadEdgeMeshFilter.hxx"
#endif

#endif
