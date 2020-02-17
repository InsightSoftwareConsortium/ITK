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

#ifndef itkSubdivisionQuadEdgeMeshFilter_h
#define itkSubdivisionQuadEdgeMeshFilter_h

#include "itkConceptChecking.h"
#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.h"
#include "itkMapContainer.h"

namespace itk
{
/**
 * \class SubdivisionQuadEdgeMeshFilter
 * \brief Abstract base class for itk::QuadEdgeMesh subdivision
 *
 * Code imported from Insight Journal publication:
 *
 * Wanlin Zhu, Triangle Mesh Subdivision
 * http://hdl.handle.net/10380/3307
 *
 * \ingroup SubdivisionQuadEdgeMeshFilter
 */
template <typename TInputMesh, typename TOutputMesh>
class SubdivisionQuadEdgeMeshFilter : public QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(SubdivisionQuadEdgeMeshFilter);

  using Self = SubdivisionQuadEdgeMeshFilter;
  using Superclass = QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, TOutputMesh>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using InputMeshType = TInputMesh;
  using InputMeshPointer = typename InputMeshType::Pointer;
  using InputMeshConstPointer = typename InputMeshType::ConstPointer;
  using InputPointsContainer = typename InputMeshType::PointsContainer;
  using InputPointsContainerPointer = typename InputMeshType::PointsContainerPointer;
  using InputPointsContainerConstIterator = typename InputMeshType::PointsContainerConstIterator;
  using InputPointsContainerIterator = typename InputMeshType::PointsContainerIterator;
  using InputCellsContainer = typename InputMeshType::CellsContainer;
  using InputCellsContainerPointer = typename InputMeshType::CellsContainerPointer;
  using InputCellsContainerIterator = typename InputMeshType::CellsContainerIterator;
  using InputCellsContainerConstIterator = typename InputMeshType::CellsContainerConstIterator;
  using InputPointType = typename InputMeshType::PointType;
  using InputCoordType = typename InputMeshType::CoordRepType;
  using InputPointIdentifier = typename InputMeshType::PointIdentifier;
  using InputCellIdentifier = typename InputMeshType::CellIdentifier;
  using InputCellType = typename InputMeshType::CellType;
  using InputQEType = typename InputMeshType::QEType;
  using InputMeshTraits = typename InputMeshType::MeshTraits;
  using InputPointIdIterator = typename InputMeshType::PointIdIterator;

  using OutputMeshType = TOutputMesh;
  using OutputMeshPointer = typename OutputMeshType::Pointer;
  using OutputPointsContainerPointer = typename OutputMeshType::PointsContainerPointer;
  using OutputPointsContainerIterator = typename OutputMeshType::PointsContainerIterator;
  using OutputCellsContainer = typename OutputMeshType::CellsContainer;
  using OutputCellsContainerPointer = typename OutputMeshType::CellsContainerPointer;
  using OutputCellsContainerIterator = typename OutputMeshType::CellsContainerIterator;
  using OutputCellsContainerConstIterator = typename OutputMeshType::CellsContainerConstIterator;
  using OutputPointType = typename OutputMeshType::PointType;
  using OutputCoordType = typename OutputMeshType::CoordRepType;
  using OutputPointIdentifier = typename OutputMeshType::PointIdentifier;
  using OutputCellIdentifier = typename OutputMeshType::CellIdentifier;
  using OutputCellType = typename OutputMeshType::CellType;
  using OutputQEType = typename OutputMeshType::QEType;
  using OutputMeshTraits = typename OutputMeshType::MeshTraits;
  using OutputPointIdIterator = typename OutputMeshType::PointIdIterator;

  using EdgePointIdentifierContainer = MapContainer<InputQEType *, OutputPointIdentifier>;
  using EdgePointIdentifierContainerPointer = typename EdgePointIdentifierContainer::Pointer;
  using EdgePointIdentifierContainerIterator = typename EdgePointIdentifierContainer::Iterator;
  using EdgePointIdentifierContainerConstIterator = typename EdgePointIdentifierContainer::ConstIterator;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(SubdivisionQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);

protected:
  SubdivisionQuadEdgeMeshFilter();
  ~SubdivisionQuadEdgeMeshFilter() override = default;

  /** inheriting class should implement this method, to take care of mesh geometry (vertex' coordinates). */
  virtual void
  GenerateOutputPoints() = 0;

  /** inheriting class should implement this method, to take care of mesh connectivity (vertex' connection). */
  virtual void
  GenerateOutputCells() = 0;
  void
  GenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  EdgePointIdentifierContainerPointer m_EdgesPointIdentifier;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSubdivisionQuadEdgeMeshFilter.hxx"
#endif

#endif
