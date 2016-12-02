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
#ifndef itkLevelSetDomainPartitionMesh_h
#define itkLevelSetDomainPartitionMesh_h

#include "itkLevelSetDomainPartitionBase.h"
#include <map>

namespace itk
{
/** \class LevelSetDomainPartitionMesh
 *
 * \brief Helper class used to partition domain and efficiently compute overlap.
 *  \ingroup ITKLevelSetsv4
 */
template< typename TMesh >
class ITK_TEMPLATE_EXPORT LevelSetDomainPartitionMesh :
  public LevelSetDomainPartitionBase< TMesh >
{
public:

  typedef LevelSetDomainPartitionMesh           Self;
  typedef LevelSetDomainPartitionBase< TMesh >  Superclass;
  typedef SmartPointer< Self >                  Pointer;
  typedef SmartPointer< const Self >            ConstPointer;

  itkStaticConstMacro( PointDimension, unsigned int, TMesh::PointDimension);

  itkTypeMacro( LevelSetDomainPartitionMesh,
                LevelSetDomainPartitionBase );

  typedef TMesh                                     MeshType;
  typedef typename MeshType::Pointer                MeshPointer;
  typedef typename MeshType::ConstPointer           MeshConstPointer;
  typedef typename MeshType::PointType              PointType;
  typedef typename MeshType::PointIdentifierType    PointIdentifierType;

  typedef typename MeshType::PointsContainerConstPointer  PointsContainerConstPointer;
  typedef typename MeshType::PointsContainerConstIterator PointsContainerConstIterator;

  typedef typename Superclass::IdentifierListType IdentifierListType;

  typedef std::map< PointIdentifierType, IdentifierListType > ListMeshType;

  itkSetObjectMacro( Mesh, MeshType );

protected:
  LevelSetDomainPartitionMesh();
  virtual ~LevelSetDomainPartitionMesh();

  /** Populate a list mesh with each node being a list of overlapping
   *  level set support at that pixel */
  virtual void PopulateListDomain();

  /** Allocate a list mesh with each node being a list of overlapping
   *  level set support at that pixel */
  void AllocateListDomain();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetDomainPartitionMesh);

  MeshPointer     m_Mesh;
  ListMeshType    m_ListDomain;
};

} //end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetDomainPartitionMesh.hxx"
#endif

#endif
