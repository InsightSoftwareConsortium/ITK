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
 * \right)\right) \right)
 * \f]
 *
 * \ingroup SubdivisionQuadEdgeMeshFilter
 */
template <typename TInputMesh, typename TOutputMesh>
class LoopTriangleCellSubdivisionQuadEdgeMeshFilter
  : public TriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>
{
public:
  typedef LoopTriangleCellSubdivisionQuadEdgeMeshFilter                      Self;
  typedef TriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh> Superclass;
  typedef SmartPointer<Self>                                                 Pointer;
  typedef SmartPointer<const Self>                                           ConstPointer;

  typedef typename Superclass::InputMeshType                     InputMeshType;
  typedef typename Superclass::InputMeshPointer                  InputMeshPointer;
  typedef typename Superclass::InputMeshConstPointer             InputMeshConstPointer;
  typedef typename Superclass::InputPointsContainer              InputPointsContainer;
  typedef typename Superclass::InputPointsContainerPointer       InputPointsContainerPointer;
  typedef typename Superclass::InputPointsContainerIterator      InputPointsContainerIterator;
  typedef typename Superclass::InputPointsContainerConstIterator InputPointsContainerConstIterator;
  typedef typename Superclass::InputCellsContainer               InputCellsContainer;
  typedef typename Superclass::InputCellsContainerPointer        InputCellsContainerPointer;
  typedef typename Superclass::InputCellsContainerIterator       InputCellsContainerIterator;
  typedef typename Superclass::InputCellsContainerConstIterator  InputCellsContainerConstIterator;
  typedef typename Superclass::InputPointType                    InputPointType;
  typedef typename Superclass::InputVectorType                   InputVectorType;
  typedef typename Superclass::InputCoordType                    InputCoordType;
  typedef typename Superclass::InputPointIdentifier              InputPointIdentifier;
  typedef typename Superclass::InputCellIdentifier               InputCellIdentifier;
  typedef typename Superclass::InputCellType                     InputCellType;
  typedef typename Superclass::InputQEType                       InputQEType;
  typedef typename Superclass::InputMeshTraits                   InputMeshTraits;
  typedef typename Superclass::InputPointIdIterator              InputPointIdIterator;

  typedef typename Superclass::OutputMeshType                        OutputMeshType;
  typedef typename Superclass::OutputMeshPointer                     OutputMeshPointer;
  typedef typename Superclass::OutputPointsContainerPointer          OutputPointsContainerPointer;
  typedef typename Superclass::OutputPointsContainerIterator         OutputPointsContainerIterator;
  typedef typename Superclass::OutputPointType                       OutputPointType;
  typedef typename Superclass::OutputVectorType                      OutputVectorType;
  typedef typename Superclass::OutputCoordType                       OutputCoordType;
  typedef typename Superclass::OutputPointIdentifier                 OutputPointIdentifier;
  typedef typename Superclass::OutputCellIdentifier                  OutputCellIdentifier;
  typedef typename Superclass::OutputCellType                        OutputCellType;
  typedef typename Superclass::OutputQEType                          OutputQEType;
  typedef typename Superclass::OutputMeshTraits                      OutputMeshTraits;
  typedef typename Superclass::OutputPointIdIterator                 OutputPointIdIterator;
  typedef typename Superclass::SubdivisionCellContainerConstIterator SubdivisionCellContainerConstIterator;

  typedef typename Superclass::EdgePointIdentifierContainer              EdgePointIdentifierContainer;
  typedef typename Superclass::EdgePointIdentifierContainerPointer       EdgePointIdentifierContainerPointer;
  typedef typename Superclass::EdgePointIdentifierContainerConstIterator EdgePointIdentifierContainerConstIterator;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(LoopTriangleCellSubdivisionQuadEdgeMeshFilter, TriangleCellSubdivisionQuadEdgeMeshFilter);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

protected:
  LoopTriangleCellSubdivisionQuadEdgeMeshFilter() {}
  ~LoopTriangleCellSubdivisionQuadEdgeMeshFilter() ITK_OVERRIDE {}

  void
  CopyInputMeshToOutputMeshPoints() ITK_OVERRIDE;

  void
  AddNewCellPoints(InputCellType * cell) ITK_OVERRIDE;

  InputPointType
  SmoothingPoint(const InputPointType & ipt, const InputPointsContainer * points);

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LoopTriangleCellSubdivisionQuadEdgeMeshFilter);
};
} // namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLoopTriangleCellSubdivisionQuadEdgeMeshFilter.hxx"
#endif
#endif
