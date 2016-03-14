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
#ifndef itkKdTree_hxx
#define itkKdTree_hxx

#include "itkKdTree.h"

namespace itk
{
namespace Statistics
{
template<typename TSample>
KdTreeNonterminalNode<TSample>
::KdTreeNonterminalNode( unsigned int partitionDimension,
                         MeasurementType partitionValue, Superclass *left, Superclass *right ) :
  m_PartitionDimension(partitionDimension),
  m_PartitionValue(partitionValue),
  m_InstanceIdentifier(0),
  m_Left(left),
  m_Right(right)
{
}

template<typename TSample>
void
KdTreeNonterminalNode<TSample>
::GetParameters( unsigned int &partitionDimension,
  MeasurementType &partitionValue ) const
{
  partitionDimension = this->m_PartitionDimension;
  partitionValue = this->m_PartitionValue;
}

template<typename TSample>
KdTreeWeightedCentroidNonterminalNode<TSample>
::KdTreeWeightedCentroidNonterminalNode( unsigned int partitionDimension,
  MeasurementType partitionValue, Superclass *left, Superclass *right,
  CentroidType & centroid, unsigned int size )
{
  this->m_PartitionDimension = partitionDimension;
  this->m_PartitionValue = partitionValue;
  this->m_Left = left;
  this->m_Right = right;
  this->m_WeightedCentroid = centroid;
  this->m_MeasurementVectorSize =
    NumericTraits<CentroidType>::GetLength( centroid );

  this->m_Centroid = this->m_WeightedCentroid / static_cast<double>( size );

  this->m_Size = size;
}

template<typename TSample>
void
KdTreeWeightedCentroidNonterminalNode<TSample>
::GetParameters( unsigned int &partitionDimension,
  MeasurementType &partitionValue) const
{
  partitionDimension = this->m_PartitionDimension;
  partitionValue = this->m_PartitionValue;
}

template<typename TSample>
KdTree<TSample>
::KdTree()
{
  this->m_EmptyTerminalNode = new KdTreeTerminalNode<TSample>();

  this->m_DistanceMetric = DistanceMetricType::New();
  this->m_Sample = ITK_NULLPTR;
  this->m_Root = ITK_NULLPTR;
  this->m_BucketSize = 16;
  this->m_MeasurementVectorSize = 0;
}

template<typename TSample>
KdTree<TSample>
::~KdTree()
{
  if( this->m_Root != ITK_NULLPTR )
    {
    this->DeleteNode( this->m_Root );
    }
  delete this->m_EmptyTerminalNode;
}

template<typename TSample>
void
KdTree<TSample>
::PrintSelf( std::ostream &os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "Input Sample: ";
  if( this->m_Sample != ITK_NULLPTR )
    {
    os << this->m_Sample << std::endl;
    }
  else
    {
    os << "not set." << std::endl;
    }
  os << indent << "Bucket Size: " << this->m_BucketSize << std::endl;
  os << indent << "Root Node: ";
  if( this->m_Root != ITK_NULLPTR )
    {
    os << this->m_Root << std::endl;
    }
  else
    {
    os << "not set." << std::endl;
    }
  os << indent << "MeasurementVectorSize: "
     << this->m_MeasurementVectorSize << std::endl;
}

template<typename TSample>
void
KdTree<TSample>
::DeleteNode( KdTreeNodeType *node )
{
  if( node->IsTerminal() )
    {
    // terminal node
    if( node == this->m_EmptyTerminalNode )
      {
      // empty node
      return;
      }
    delete node;
    return;
    }

  // non-terminal node
  if( node->Left() != ITK_NULLPTR )
    {
    this->DeleteNode( node->Left() );
    }

  if( node->Right() != ITK_NULLPTR )
    {
    this->DeleteNode( node->Right() );
    }

  delete node;
}

template<typename TSample>
void
KdTree<TSample>
::SetSample( const TSample *sample )
{
  this->m_Sample = sample;
  this->m_MeasurementVectorSize = this->m_Sample->GetMeasurementVectorSize();
  this->m_DistanceMetric->SetMeasurementVectorSize(
    this->m_MeasurementVectorSize );
  this->Modified();
}

template<typename TSample>
void
KdTree<TSample>
::SetBucketSize(unsigned int size)
{
  this->m_BucketSize = size;
}

template<typename TSample>
void
KdTree<TSample>
::Search( const MeasurementVectorType & query,
         unsigned int numberOfNeighborsRequested, InstanceIdentifierVectorType &result ) const
{
  // This function has two different signatures. The other signature, that returns the distances vector too,
  // is called here; however, its distances vector is discarded.
  std::vector<double> not_used_distances;
  this->Search(query, numberOfNeighborsRequested, result, not_used_distances);
}

template<typename TSample>
void
KdTree<TSample>
::Search( const MeasurementVectorType & query,
  unsigned int numberOfNeighborsRequested, InstanceIdentifierVectorType &result,
  std::vector<double> &distances ) const
{
  if( numberOfNeighborsRequested > this->Size() )
    {
    itkExceptionMacro( "The numberOfNeighborsRequested for the nearest "
      << "neighbor search should be less than or equal to the number of "
      << "the measurement vectors." );
    }

  /* 'distances' is the storage container used internally for the
   * NearestNeighbors class.  The 'distances' vector is modified
   * by the NearestNeighbors class.  By passing in
   * the 'distances' vector here, we can avoid unnecessary memory
   * duplications and copy operations.*/
  NearestNeighbors nearestNeighbors(distances);
  nearestNeighbors.resize( numberOfNeighborsRequested );

  MeasurementVectorType lowerBound;
  NumericTraits<MeasurementVectorType>::SetLength( lowerBound,
    this->m_MeasurementVectorSize );
  MeasurementVectorType upperBound;
  NumericTraits<MeasurementVectorType>::SetLength( upperBound,
    this->m_MeasurementVectorSize );

  for(  unsigned int d = 0; d < this->m_MeasurementVectorSize; ++d )
    {
    lowerBound[d] = static_cast< MeasurementType >( -std::sqrt(
      -static_cast< double >( NumericTraits< MeasurementType >::
      NonpositiveMin() ) ) / 2.0 );
    upperBound[d] = static_cast< MeasurementType >( std::sqrt(
      static_cast<double >( NumericTraits< MeasurementType >::max() ) / 2.0 ) );
    }
  this->NearestNeighborSearchLoop( this->m_Root, query, lowerBound, upperBound,
    nearestNeighbors );

  result = nearestNeighbors.GetNeighbors();
}

template<typename TSample>
inline int
KdTree<TSample>
::NearestNeighborSearchLoop( const KdTreeNodeType *node,
  const MeasurementVectorType &query, MeasurementVectorType &lowerBound,
  MeasurementVectorType &upperBound, NearestNeighbors &nearestNeighbors ) const
{
  unsigned int       i;
  InstanceIdentifier tempId;
  double             tempDistance;

  if( node->IsTerminal() )
    {
    // terminal node
    if( node == this->m_EmptyTerminalNode )
      {
      // empty node
      return 0;
      }

    for(  i = 0; i < node->Size(); ++i )
      {
      tempId = node->GetInstanceIdentifier(i);
      tempDistance = this->m_DistanceMetric->Evaluate( query,
        this->m_Sample->GetMeasurementVector( tempId ) );
      if( tempDistance < nearestNeighbors.GetLargestDistance() )
        {
        nearestNeighbors.ReplaceFarthestNeighbor( tempId, tempDistance );
        }
      }

    if( this->BallWithinBounds( query, lowerBound, upperBound,
      nearestNeighbors.GetLargestDistance() ) )
      {
      return 1;
      }

    return 0;
    }

  unsigned int    partitionDimension;
  MeasurementType partitionValue;
  MeasurementType tempValue;
  node->GetParameters( partitionDimension, partitionValue );

  //
  // Check the point associated with the nonterminal node
  // and potentially add it to the list of nearest neighbors
  //
  tempId = node->GetInstanceIdentifier(0);
  tempDistance = this->m_DistanceMetric->Evaluate( query,
    this->m_Sample->GetMeasurementVector( tempId ) );
  if( tempDistance < nearestNeighbors.GetLargestDistance() )
    {
    nearestNeighbors.ReplaceFarthestNeighbor( tempId, tempDistance );
    }

  //
  // Now check both child sub-trees
  //
  if( query[partitionDimension] <= partitionValue )
    {
    // search the closer child node
    tempValue = upperBound[partitionDimension];
    upperBound[partitionDimension] = partitionValue;
    if( this->NearestNeighborSearchLoop( node->Left(), query, lowerBound,
      upperBound, nearestNeighbors ) )
      {
      return 1;
      }
    upperBound[partitionDimension] = tempValue;

    // search the other node, if necessary
    tempValue = lowerBound[partitionDimension];
    lowerBound[partitionDimension] = partitionValue;
    if( this->BoundsOverlapBall( query, lowerBound, upperBound,
      nearestNeighbors.GetLargestDistance() ) )
      {
      this->NearestNeighborSearchLoop( node->Right(), query, lowerBound,
        upperBound, nearestNeighbors );
      }
    lowerBound[partitionDimension] = tempValue;
    }
  else
    {
    // search the closer child node
    tempValue = lowerBound[partitionDimension];
    lowerBound[partitionDimension] = partitionValue;
    if( this->NearestNeighborSearchLoop( node->Right(), query, lowerBound,
      upperBound, nearestNeighbors ) )
      {
      return 1;
      }
    lowerBound[partitionDimension] = tempValue;

    // search the other node, if necessary
    tempValue = upperBound[partitionDimension];
    upperBound[partitionDimension] = partitionValue;
    if( this->BoundsOverlapBall( query, lowerBound, upperBound,
      nearestNeighbors.GetLargestDistance() ) )
      {
      this->NearestNeighborSearchLoop( node->Left(), query, lowerBound,
        upperBound, nearestNeighbors );
      }
    upperBound[partitionDimension] = tempValue;
    }

  // stop or continue search
  if( this->BallWithinBounds( query, lowerBound, upperBound,
    nearestNeighbors.GetLargestDistance() ) )
    {
    return 1;
    }

  return 0;
}

template<typename TSample>
void
KdTree<TSample>
::Search( const MeasurementVectorType & query, double radius,
  InstanceIdentifierVectorType & result ) const
{
  MeasurementVectorType lowerBound;
  MeasurementVectorType upperBound;

  NumericTraits<MeasurementVectorType>::SetLength( lowerBound,
    this->m_MeasurementVectorSize );
  NumericTraits<MeasurementVectorType>::SetLength( upperBound,
    this->m_MeasurementVectorSize );

  for(  unsigned int d = 0; d < this->m_MeasurementVectorSize; ++d )
    {
    lowerBound[d] = static_cast<MeasurementType>( -std::sqrt(
      -static_cast<double>( NumericTraits<MeasurementType>::
      NonpositiveMin() ) ) / 2.0 );
    upperBound[d] = static_cast< MeasurementType >( std::sqrt(
      static_cast<double>( NumericTraits< MeasurementType >::max() ) / 2.0 ) );
    }

  result.clear();
  this->SearchLoop( this->m_Root, query, radius, lowerBound, upperBound, result );
}

template<typename TSample>
inline int
KdTree<TSample>
::SearchLoop( const KdTreeNodeType *node, const MeasurementVectorType &query,
  double radius, MeasurementVectorType &lowerBound, MeasurementVectorType
  &upperBound, InstanceIdentifierVectorType &neighbors ) const
{
  InstanceIdentifier tempId;
  double             tempDistance;

  if( node->IsTerminal() )
    {
    // terminal node
    if( node == this->m_EmptyTerminalNode )
      {
      // empty node
      return 0;
      }

    for( unsigned int i = 0; i < node->Size(); ++i )
      {
      tempId = node->GetInstanceIdentifier( i );
      tempDistance = this->m_DistanceMetric->Evaluate( query,
        this->m_Sample->GetMeasurementVector( tempId ) );
      if( tempDistance <= radius )
        {
        neighbors.push_back( tempId );
        }
      }

    if( this->BallWithinBounds( query, lowerBound, upperBound, radius ) )
      {
      return 1;
      }

    return 0;
    }
  if( node->IsTerminal() == false )
    {
    tempId = node->GetInstanceIdentifier( 0 );
    tempDistance = this->m_DistanceMetric->Evaluate( query,
      this->m_Sample->GetMeasurementVector( tempId ) );
    if( tempDistance <= radius )
      {
      neighbors.push_back( tempId );
      }
    }

  unsigned int    partitionDimension;
  MeasurementType partitionValue;
  MeasurementType tempValue;
  node->GetParameters( partitionDimension, partitionValue );

  if( query[partitionDimension] <= partitionValue )
    {
    // search the closer child node
    tempValue = upperBound[partitionDimension];
    upperBound[partitionDimension] = partitionValue;
    if( this->SearchLoop( node->Left(), query, radius, lowerBound, upperBound,
      neighbors ) )
      {
      return 1;
      }
    upperBound[partitionDimension] = tempValue;

    // search the other node, if necessary
    tempValue = lowerBound[partitionDimension];
    lowerBound[partitionDimension] = partitionValue;
    if( this->BoundsOverlapBall( query, lowerBound, upperBound, radius ) )
      {
      this->SearchLoop( node->Right(), query, radius, lowerBound, upperBound,
        neighbors );
      }
    lowerBound[partitionDimension] = tempValue;
    }
  else
    {
    // search the closer child node
    tempValue = lowerBound[partitionDimension];
    lowerBound[partitionDimension] = partitionValue;
    if( this->SearchLoop( node->Right(), query, radius, lowerBound, upperBound,
      neighbors ) )
      {
      return 1;
      }
    lowerBound[partitionDimension] = tempValue;

    // search the other node, if necessary
    tempValue = upperBound[partitionDimension];
    upperBound[partitionDimension] = partitionValue;
    if( this->BoundsOverlapBall( query, lowerBound, upperBound, radius ) )
      {
      this->SearchLoop( node->Left(), query, radius, lowerBound, upperBound,
        neighbors );
      }
    upperBound[partitionDimension] = tempValue;
    }

  // stop or continue search
  if( this->BallWithinBounds( query, lowerBound, upperBound, radius ) )
    {
    return 1;
    }

  return 0;
}

template<typename TSample>
inline bool
KdTree<TSample>
::BallWithinBounds( const MeasurementVectorType & query, MeasurementVectorType
  &lowerBound, MeasurementVectorType & upperBound, double radius ) const
{
  for( unsigned int d = 0; d < this->m_MeasurementVectorSize; ++d )
    {
    if( ( this->m_DistanceMetric->Evaluate( query[d], lowerBound[d] ) <=
      radius ) || ( this->m_DistanceMetric->Evaluate( query[d],
      upperBound[d] ) <= radius ) )
      {
      return false;
      }
    }
  return true;
}

template<typename TSample>
inline bool
KdTree<TSample>
::BoundsOverlapBall( const MeasurementVectorType &query, MeasurementVectorType
  &lowerBound, MeasurementVectorType &upperBound, double radius ) const
{
  double squaredSearchRadius = itk::Math::sqr( radius );

  double sum = 0.0;
  for( unsigned int d = 0; d < this->m_MeasurementVectorSize; ++d )
    {
    if( query[d] <= lowerBound[d] )
      {
      sum += itk::Math::sqr( this->m_DistanceMetric->Evaluate( query[d],
        lowerBound[d] ) );
      if( sum < squaredSearchRadius )
        {
        return true;
        }
      }
    else if( query[d] >= upperBound[d] )
      {
      sum += itk::Math::sqr( this->m_DistanceMetric->Evaluate( query[d],
        upperBound[d] ) );
      if( sum < squaredSearchRadius )
        {
        return true;
        }
      }
    }
  return false;
}

template<typename TSample>
void
KdTree<TSample>
::PrintTree( std::ostream & os ) const
{
  const unsigned int topLevel = 0;
  const unsigned int activeDimension = 0;

  this->PrintTree( this->m_Root, topLevel, activeDimension, os );
}

template<typename TSample>
void
KdTree<TSample>
::PrintTree( KdTreeNodeType *node, unsigned int level,
  unsigned int activeDimension, std::ostream &os ) const
{
  level++;
  if( node->IsTerminal() )
    {
    // terminal node
    if( node == this->m_EmptyTerminalNode )
      {
      // empty node
      os << "Empty node: level = " << level << std::endl;
      return;
      }
    os << "Terminal: level = " << level << " dim = " << activeDimension
       << std::endl;
    os << "          ";
    for(  unsigned int i = 0; i < node->Size(); ++i )
      {
      os << "[" << node->GetInstanceIdentifier( i ) << "] "
         << this->m_Sample->GetMeasurementVector(
         node->GetInstanceIdentifier( i ) ) << ", ";
      }
    os << std::endl;
    return;
    }

  unsigned int    partitionDimension;
  MeasurementType partitionValue;

  node->GetParameters( partitionDimension, partitionValue );
  typename KdTreeNodeType::CentroidType centroid;
  node->GetWeightedCentroid(centroid);
  os << "Nonterminal: level = " << level << std::endl;
  os << "             dim = " << partitionDimension << std::endl;
  os << "             value = " << partitionValue << std::endl;
  os << "             weighted centroid = " << centroid;
  os << "             size = " << node->Size() << std::endl;
  os << "             identifier = " << node->GetInstanceIdentifier( 0 );
  os << this->m_Sample->GetMeasurementVector( node->GetInstanceIdentifier( 0 ) )
     << std::endl;

  this->PrintTree( node->Left(),  level, partitionDimension, os );
  this->PrintTree( node->Right(), level, partitionDimension, os );
}

template<typename TSample>
void
KdTree<TSample>
::PlotTree( std::ostream & os ) const
{
  //
  // Graph header
  //
  os << "digraph G {" << std::endl;

  //
  // Recursively visit the tree and add entries for the nodes
  //
  this->PlotTree( this->m_Root, os );

  //
  // Graph footer
  //
  os << "}" << std::endl;
}

template<typename TSample>
void
KdTree<TSample>
::PlotTree( KdTreeNodeType *node, std::ostream & os ) const
{
  unsigned int    partitionDimension;
  MeasurementType partitionValue;

  node->GetParameters( partitionDimension, partitionValue );

  KdTreeNodeType *left  = node->Left();
  KdTreeNodeType *right = node->Right();

  char partitionDimensionCharSymbol = ( 'X' + partitionDimension );

  if( node->IsTerminal() )
    {
    // terminal node
    if( node != this->m_EmptyTerminalNode )
      {
      os << "\"" << node << "\" [label=\"";
      for(  unsigned int i = 0; i < node->Size(); ++i )
        {
        os << this->GetMeasurementVector( node->GetInstanceIdentifier( i ) );
        os << " ";
        }
      os << "\" ];" << std::endl;
      }
    }
  else
    {
    os << "\"" << node << "\" [label=\"";
    os << this->GetMeasurementVector( node->GetInstanceIdentifier( 0 ) );
    os << " " << partitionDimensionCharSymbol << "=" << partitionValue;
    os << "\" ];" << std::endl;
    }

  if( left &&  ( left != this->m_EmptyTerminalNode ) )
    {
    os << "\"" << node << "\" -> \"" << left << "\";" << std::endl;
    this->PlotTree( left, os );
    }

  if( right && ( right != this->m_EmptyTerminalNode ) )
    {
    os << "\"" << node << "\" -> \"" << right << "\";" << std::endl;
    this->PlotTree( right, os );
    }
}
} // end of namespace Statistics
} // end of namespace itk

#endif
