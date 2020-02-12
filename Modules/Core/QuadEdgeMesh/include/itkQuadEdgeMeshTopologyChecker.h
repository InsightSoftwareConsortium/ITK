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
#ifndef itkQuadEdgeMeshTopologyChecker_h
#define itkQuadEdgeMeshTopologyChecker_h

#include "itkQuadEdgeMeshBoundaryEdgesMeshFunction.h"

namespace itk
{
/**
 *\class QuadEdgeMeshTopologyChecker
 *  \brief Make some basic checks in order to verify that the considered
 *         mesh is not degenerated and correctly represents a surface
 *         with a potential boundary.
 *
 * We check that they are no isolated vertices, no isolated edges and
 * that the Euler formula is possible.
 *
 * \author Alexandre Gouaillard, Leonardo Florez-Valencia, Eric Boix
 *
 * This implementation was contributed as a paper to the Insight Journal
 * https://hdl.handle.net/1926/306
 *
 * \ingroup ITKQuadEdgeMesh
 */
template <typename TMesh>
class ITK_TEMPLATE_EXPORT QuadEdgeMeshTopologyChecker : public Object
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuadEdgeMeshTopologyChecker);

  // Standard types
  using Self = QuadEdgeMeshTopologyChecker;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using MeshType = TMesh;
  using QEPrimal = typename MeshType::QEPrimal;
  using EdgeCellType = typename MeshType::EdgeCellType;
  using CellsContainerConstIterator = typename MeshType::CellsContainerConstIterator;
  using BoundaryEdges = QuadEdgeMeshBoundaryEdgesMeshFunction<MeshType>;

  using PointIdentifier = typename MeshType::PointIdentifier;
  using CellIdentifier = typename MeshType::CellIdentifier;

public:
  itkNewMacro(Self);
  itkTypeMacro(QuadEdgeMeshTopologyChecker, Object);

  itkSetConstObjectMacro(Mesh, MeshType);

  using IdentifierType = ::itk::IdentifierType;
  using OffsetValueType = ::itk::OffsetValueType;

  itkSetMacro(ExpectedNumberOfPoints, PointIdentifier);
  itkSetMacro(ExpectedNumberOfEdges, CellIdentifier);
  itkSetMacro(ExpectedNumberOfFaces, CellIdentifier);
  itkSetMacro(ExpectedNumberOfBoundaries, CellIdentifier);
  itkSetMacro(ExpectedGenus, OffsetValueType);

  bool
  ValidateEulerCharacteristic() const;

protected:
  QuadEdgeMeshTopologyChecker();
  ~QuadEdgeMeshTopologyChecker() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  using MeshPointer = typename MeshType::ConstPointer;

  MeshPointer m_Mesh;

  PointIdentifier m_ExpectedNumberOfPoints;
  CellIdentifier  m_ExpectedNumberOfEdges;
  CellIdentifier  m_ExpectedNumberOfFaces;
  CellIdentifier  m_ExpectedNumberOfBoundaries;
  OffsetValueType m_ExpectedGenus;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkQuadEdgeMeshTopologyChecker.hxx"
#endif

#endif
