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

#ifndef itkLoopTriangleCellSubdivisionQuadEdgeMeshFilter_h
#define itkLoopTriangleCellSubdivisionQuadEdgeMeshFilter_h

#include "itkTriangleCellSubdivisionQuadEdgeMeshFilter.h"

namespace itk
{
/**
 * \class LoopTriangleCellSubdivisionQuadEdgeMeshFilter
 * \brief Subdivide a triangular surface QuadEdgeMesh using Loop Subdivision
 *
 * The Loop subdivision scheme is a simple approximating face-split scheme for
 * triangular meshes. The new points defined as:
 * \f[
 * NV_k = \frac{3}{8} \sum_{i=1}^{2} U_k^i + \frac{1}{8} \sum_{i=1}^{2} V_k^i
 * \f]
 *
 * where \f$NV_k\f$ is the new inserted points, \f$U_k\f$ are the two vertices
 * of edge \f$k\f$, \f$V_k\f$ are two neighborhood vertices. In addition, the
 * original vertices are smoothed, for each vertex in the original mesh
 * \f[
 * OV_k = \left( 1- N \cdot B \right) \cdot OV_k + \beta \cdot \sum_{i=1}^{N} V_i
 * \f]
 *
 * where \f$N\f$ denotes the number of vertices of first ring neighborhood
 * points. \f$OVk\f$ is the original vertex. \f$V_i\f$ are first ring
 * neighborhood points. The weighting \f$\beta\f$  defined as
 *
 * \f[
 * \beta = \frac{1}{N} \left( \frac{5}{8} - \left( \frac{3}{8} + \frac{1}{4} \cdot \cos^2\left(\frac{2\pi}{N}
 * \right)\right) \right) \f]
 *
 * \ingroup SubdivisionQuadEdgeMeshFilter
 */
template <typename TInputMesh, typename TOutputMesh>
class LoopTriangleCellSubdivisionQuadEdgeMeshFilter
  : public TriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LoopTriangleCellSubdivisionQuadEdgeMeshFilter);

  using Self = LoopTriangleCellSubdivisionQuadEdgeMeshFilter;
  using Superclass = TriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using InputMeshType = typename Superclass::InputMeshType;
  using InputMeshPointer = typename Superclass::InputMeshPointer;
  using InputMeshConstPointer = typename Superclass::InputMeshConstPointer;
  using InputPointsContainer = typename Superclass::InputPointsContainer;
  using InputPointsContainerPointer = typename Superclass::InputPointsContainerPointer;
  using InputPointsContainerIterator = typename Superclass::InputPointsContainerIterator;
  using InputPointsContainerConstIterator = typename Superclass::InputPointsContainerConstIterator;
  using InputCellsContainer = typename Superclass::InputCellsContainer;
  using InputCellsContainerPointer = typename Superclass::InputCellsContainerPointer;
  using InputCellsContainerIterator = typename Superclass::InputCellsContainerIterator;
  using InputCellsContainerConstIterator = typename Superclass::InputCellsContainerConstIterator;
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
  using SubdivisionCellContainerConstIterator = typename Superclass::SubdivisionCellContainerConstIterator;

  using EdgePointIdentifierContainer = typename Superclass::EdgePointIdentifierContainer;
  using EdgePointIdentifierContainerPointer = typename Superclass::EdgePointIdentifierContainerPointer;
  using EdgePointIdentifierContainerConstIterator = typename Superclass::EdgePointIdentifierContainerConstIterator;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(LoopTriangleCellSubdivisionQuadEdgeMeshFilter, TriangleCellSubdivisionQuadEdgeMeshFilter);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

protected:
  LoopTriangleCellSubdivisionQuadEdgeMeshFilter() {}
  ~LoopTriangleCellSubdivisionQuadEdgeMeshFilter() override {}

  void
  CopyInputMeshToOutputMeshPoints() override;

  void
  AddNewCellPoints(InputCellType * cell) override;

  InputPointType
  SmoothingPoint(const InputPointType & ipt, const InputPointsContainer * points);
};
} // namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLoopTriangleCellSubdivisionQuadEdgeMeshFilter.hxx"
#endif
#endif
