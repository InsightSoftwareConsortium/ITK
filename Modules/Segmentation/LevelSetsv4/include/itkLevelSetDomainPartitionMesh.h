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
#ifndef itkLevelSetDomainPartitionMesh_h
#define itkLevelSetDomainPartitionMesh_h

#include "itkLevelSetDomainPartitionBase.h"
#include <map>

namespace itk
{
/**
 *\class LevelSetDomainPartitionMesh
 *
 * \brief Helper class used to partition domain and efficiently compute overlap.
 *  \ingroup ITKLevelSetsv4
 */
template <typename TMesh>
class ITK_TEMPLATE_EXPORT LevelSetDomainPartitionMesh : public LevelSetDomainPartitionBase<TMesh>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetDomainPartitionMesh);

  using Self = LevelSetDomainPartitionMesh;
  using Superclass = LevelSetDomainPartitionBase<TMesh>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  static constexpr unsigned int PointDimension = TMesh::PointDimension;

  itkTypeMacro(LevelSetDomainPartitionMesh, LevelSetDomainPartitionBase);

  using MeshType = TMesh;
  using MeshPointer = typename MeshType::Pointer;
  using MeshConstPointer = typename MeshType::ConstPointer;
  using PointType = typename MeshType::PointType;
  using PointIdentifierType = typename MeshType::PointIdentifierType;

  using PointsContainerConstPointer = typename MeshType::PointsContainerConstPointer;
  using PointsContainerConstIterator = typename MeshType::PointsContainerConstIterator;

  using IdentifierListType = typename Superclass::IdentifierListType;

  using ListMeshType = std::map<PointIdentifierType, IdentifierListType>;

  itkSetObjectMacro(Mesh, MeshType);

protected:
  LevelSetDomainPartitionMesh() = default;
  virtual ~LevelSetDomainPartitionMesh() = default;

  /** Populate a list mesh with each node being a list of overlapping
   *  level set support at that pixel */
  virtual void
  PopulateListDomain();

  /** Allocate a list mesh with each node being a list of overlapping
   *  level set support at that pixel */
  void
  AllocateListDomain();

private:
  MeshPointer  m_Mesh;
  ListMeshType m_ListDomain;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetDomainPartitionMesh.hxx"
#endif

#endif
