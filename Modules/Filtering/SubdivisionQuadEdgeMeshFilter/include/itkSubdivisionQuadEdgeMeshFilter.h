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
  typedef SubdivisionQuadEdgeMeshFilter                             Self;
  typedef QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, TOutputMesh> Superclass;
  typedef SmartPointer<Self>                                        Pointer;
  typedef SmartPointer<const Self>                                  ConstPointer;

  typedef TInputMesh                                           InputMeshType;
  typedef typename InputMeshType::Pointer                      InputMeshPointer;
  typedef typename InputMeshType::ConstPointer                 InputMeshConstPointer;
  typedef typename InputMeshType::PointsContainer              InputPointsContainer;
  typedef typename InputMeshType::PointsContainerPointer       InputPointsContainerPointer;
  typedef typename InputMeshType::PointsContainerConstIterator InputPointsContainerConstIterator;
  typedef typename InputMeshType::PointsContainerIterator      InputPointsContainerIterator;
  typedef typename InputMeshType::CellsContainer               InputCellsContainer;
  typedef typename InputMeshType::CellsContainerPointer        InputCellsContainerPointer;
  typedef typename InputMeshType::CellsContainerIterator       InputCellsContainerIterator;
  typedef typename InputMeshType::CellsContainerConstIterator  InputCellsContainerConstIterator;
  typedef typename InputMeshType::PointType                    InputPointType;
  typedef typename InputMeshType::CoordRepType                 InputCoordType;
  typedef typename InputMeshType::PointIdentifier              InputPointIdentifier;
  typedef typename InputMeshType::CellIdentifier               InputCellIdentifier;
  typedef typename InputMeshType::CellType                     InputCellType;
  typedef typename InputMeshType::QEType                       InputQEType;
  typedef typename InputMeshType::MeshTraits                   InputMeshTraits;
  typedef typename InputMeshType::PointIdIterator              InputPointIdIterator;

  typedef TOutputMesh                                          OutputMeshType;
  typedef typename OutputMeshType::Pointer                     OutputMeshPointer;
  typedef typename OutputMeshType::PointsContainerPointer      OutputPointsContainerPointer;
  typedef typename OutputMeshType::PointsContainerIterator     OutputPointsContainerIterator;
  typedef typename OutputMeshType::CellsContainer              OutputCellsContainer;
  typedef typename OutputMeshType::CellsContainerPointer       OutputCellsContainerPointer;
  typedef typename OutputMeshType::CellsContainerIterator      OutputCellsContainerIterator;
  typedef typename OutputMeshType::CellsContainerConstIterator OutputCellsContainerConstIterator;
  typedef typename OutputMeshType::PointType                   OutputPointType;
  typedef typename OutputMeshType::CoordRepType                OutputCoordType;
  typedef typename OutputMeshType::PointIdentifier             OutputPointIdentifier;
  typedef typename OutputMeshType::CellIdentifier              OutputCellIdentifier;
  typedef typename OutputMeshType::CellType                    OutputCellType;
  typedef typename OutputMeshType::QEType                      OutputQEType;
  typedef typename OutputMeshType::MeshTraits                  OutputMeshTraits;
  typedef typename OutputMeshType::PointIdIterator             OutputPointIdIterator;

  typedef MapContainer<InputQEType *, OutputPointIdentifier>   EdgePointIdentifierContainer;
  typedef typename EdgePointIdentifierContainer::Pointer       EdgePointIdentifierContainerPointer;
  typedef typename EdgePointIdentifierContainer::Iterator      EdgePointIdentifierContainerIterator;
  typedef typename EdgePointIdentifierContainer::ConstIterator EdgePointIdentifierContainerConstIterator;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(SubdivisionQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);

protected:
  SubdivisionQuadEdgeMeshFilter();
  ~SubdivisionQuadEdgeMeshFilter() ITK_OVERRIDE {}

  /** inheriting class should implement this method, to take care of mesh geometry (vertex' coordinates). */
  virtual void
  GenerateOutputPoints() = 0;

  /** inheriting class should implement this method, to take care of mesh connectivity (vertex' connection). */
  virtual void
  GenerateOutputCells() = 0;
  void
  GenerateData() ITK_OVERRIDE;

  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  EdgePointIdentifierContainerPointer m_EdgesPointIdentifier;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SubdivisionQuadEdgeMeshFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSubdivisionQuadEdgeMeshFilter.hxx"
#endif

#endif
