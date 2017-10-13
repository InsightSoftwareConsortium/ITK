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
#ifndef itkPointsLocator_h
#define itkPointsLocator_h

#include "itkObject.h"

#include "itkPoint.h"
#include "itkIntTypes.h"
#include "itkKdTree.h"
#include "itkKdTreeGenerator.h"
#include "itkVectorContainer.h"
#include "itkVectorContainerToListSampleAdaptor.h"

namespace itk
{

/** \class PointsLocator
 * \brief Accelerate geometric searches for points.
 *
 * This class accelerates the search for the closest point to a user-provided
 * point, by using constructing a Kd-Tree structure for the PointSetContainer.
 *
 * \ingroup ITKRegistrationCommon
 */
template<
  typename TPointsContainer = VectorContainer< IdentifierType, Point< float, 3 > >
  >
class ITK_TEMPLATE_EXPORT PointsLocator : public Object
{
public:
  /** Standard class typedefs. */
  typedef PointsLocator               Self;
  typedef Object                      Superclass;
  typedef SmartPointer<Self>          Pointer;
  typedef SmartPointer<const Self>    ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** Standard part of every itk Object. */
  itkTypeMacro( PointsLocator, Object );

  /** Hold on to the type information specified by the template parameters. */
  typedef TPointsContainer                            PointsContainer;
  typedef typename PointsContainer::Pointer           PointsContainerPointer;
  typedef typename PointsContainer::ConstPointer      PointsContainerConstPointer;
  typedef typename PointsContainer::ElementIdentifier PointIdentifier;
  typedef typename PointsContainer::Element           PointType;

  /** Hold on to the dimensions specified by the template parameters. */
  itkStaticConstMacro( PointDimension, unsigned int, PointType::PointDimension );

  /** Convenient typedefs. */
  typedef typename PointsContainer::ConstIterator PointsContainerConstIterator;
  typedef typename PointsContainer::Iterator      PointsContainerIterator;

  /** Type of the PointsContainer to List Adaptor. */
  typedef Statistics::VectorContainerToListSampleAdaptor
    <PointsContainer>                                   SampleAdaptorType;
  typedef typename SampleAdaptorType::Pointer           SampleAdaptorPointer;

  /** Types fo the KdTreeGenerator */
  typedef Statistics::KdTreeGenerator<SampleAdaptorType>    TreeGeneratorType;
  typedef typename TreeGeneratorType::Pointer               TreeGeneratorPointer;
  typedef typename TreeGeneratorType::KdTreeType            TreeType;
  typedef typename TreeType::ConstPointer                   TreeConstPointer;
  typedef typename TreeType::InstanceIdentifierVectorType   NeighborsIdentifierType;

  /** Set/Get the points from which the bounding box should be computed. */
  itkSetObjectMacro( Points, PointsContainer );

  /** Set/Get the points from which the bounding box should be computed. */
  itkGetModifiableObjectMacro(Points, PointsContainer );

  /** Compute the kd-tree that will facilitate the querying the points. */
  void Initialize();

  /** Find the closest point */
  PointIdentifier FindClosestPoint( const PointType &query ) const;

  /** Find the k-nearest neighbors.  Returns the point ids. */
  void Search( const PointType &, unsigned int, NeighborsIdentifierType & )
    const;

  /** Find the closest N points.  Returns the point ids. */
  void FindClosestNPoints( const PointType &, unsigned int,
    NeighborsIdentifierType & ) const;

  /** Find all the points within a specified radius.  Returns the point ids. */
  void Search( const PointType &, double, NeighborsIdentifierType & ) const;

  /** Find all the points within a specified radius.  Returns the point ids. */
  void FindPointsWithinRadius( const PointType &, double,
    NeighborsIdentifierType & ) const;

protected:
  PointsLocator();
  ~PointsLocator() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PointsLocator);

  PointsContainerPointer   m_Points;
  SampleAdaptorPointer     m_SampleAdaptor;
  TreeGeneratorPointer     m_KdTreeGenerator;
  TreeConstPointer         m_Tree;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointsLocator.hxx"
#endif

#endif
