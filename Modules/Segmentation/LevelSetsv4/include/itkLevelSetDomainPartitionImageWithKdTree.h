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
#ifndef itkLevelSetDomainPartitionImageWithKdTree_h
#define itkLevelSetDomainPartitionImageWithKdTree_h

#include "itkLevelSetDomainPartitionImage.h"

#include "itkListSample.h"
#include "itkKdTreeGenerator.h"

namespace itk
{
/** \class LevelSetDomainPartitionImageWithKdTree
 *
 * \brief Helper class used to share data in the ScalarChanAndVeseLevelSetFunction.
 * \ingroup ITKLevelSetsv4
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT LevelSetDomainPartitionImageWithKdTree:
  public LevelSetDomainPartitionImage< TImage >
{
public:

  typedef LevelSetDomainPartitionImageWithKdTree  Self;
  typedef LevelSetDomainPartitionImage< TImage >  Superclass;
  typedef SmartPointer< Self >                    Pointer;
  typedef SmartPointer< const Self >              ConstPointer;

  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  itkTypeMacro( LevelSetDomainPartitionImageWithKdTree, LevelSetDomainPartitionImage);

  typedef TImage                                        ImageType;
  typedef typename Superclass::ListIndexType            ListIndexType;
  typedef typename Superclass::ListRegionType           ListRegionType;
  typedef typename Superclass::ListPointType            ListPointType;
  typedef typename Superclass::ListIteratorType         ListIteratorType;
  typedef typename Superclass::IdentifierListType       IdentifierListType;

  typedef typename ListPointType::VectorType                    CentroidVectorType;
  typedef typename Statistics::ListSample< CentroidVectorType > SampleType;
  typedef typename Statistics::KdTreeGenerator< SampleType >    TreeGeneratorType;
  typedef typename TreeGeneratorType::Pointer                   TreePointer;
  typedef typename TreeGeneratorType::KdTreeType                TreeType;
  typedef typename TreeType::Pointer                            KdTreePointer;

  /** Initialize with a precomputed kd-tree */
  itkSetObjectMacro( KdTree, TreeType );

  /** Number of neighbors level sets connected to this level set. */
  typedef unsigned int NeighborsIdType;

  /** Get/Set number of neighbors in the kd-tree leaf node */
  itkSetMacro( NumberOfNeighbors, NeighborsIdType );
  itkGetMacro( NumberOfNeighbors, NeighborsIdType );

  /** Populate a list image with each pixel being a list of overlapping
   *  level set support at that pixel */
  void PopulateListDomain() ITK_OVERRIDE;

protected:
  LevelSetDomainPartitionImageWithKdTree();
  ~LevelSetDomainPartitionImageWithKdTree() ITK_OVERRIDE;

  /** Populate a list image with each pixel being a list of overlapping
   *  level set support at that pixel */
  void PopulateDomainWithKdTree();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetDomainPartitionImageWithKdTree);

  KdTreePointer     m_KdTree;
  NeighborsIdType   m_NumberOfNeighbors;
};

} //end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetDomainPartitionImageWithKdTree.hxx"
#endif

#endif
