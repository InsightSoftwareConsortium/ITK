/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKdTree.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkKdTree_txx
#define __itkKdTree_txx


namespace itk{ 
  namespace Statistics{

template< class TSample >
inline void 
KdTree< TSample >
::DumpVector(MeasurementVectorType &vec)
{
  for (unsigned int i = 0 ; i < MeasurementVectorSize ; i++)
    {
      std::cout << vec[i] << "\t" ;
    }
  std::cout << std::endl ;
}

template< class TSample >
KdTreeNonterminalNode< TSample >
::KdTreeNonterminalNode(unsigned int partitionDimension,
                        MeasurementType partitionValue,
                        Superclass* left,
                        Superclass* right)
{
  m_PartitionDimension = partitionDimension ;
  m_PartitionValue = partitionValue ;
  m_Left = left ;
  m_Right = right ;
}

template< class TSample >
void
KdTreeNonterminalNode< TSample >
::GetParameters(unsigned int &partitionDimension, 
                MeasurementType &partitionValue)
{
  partitionDimension = m_PartitionDimension ;
  partitionValue = m_PartitionValue ;
}

template< class TSample >
KdTreeWeightedCenteroidNonterminalNode< TSample >
::KdTreeWeightedCenteroidNonterminalNode(unsigned int partitionDimension,
                                         MeasurementType partitionValue,
                                         Superclass* left,
                                         Superclass* right,
                                         CenteroidType &centeroid,
                                         unsigned int size)
{
  m_PartitionDimension = partitionDimension ;
  m_PartitionValue = partitionValue ;
  m_Left = left ;
  m_Right = right ;
  m_WeightedCenteroid = centeroid ;

  for (unsigned int i = 0 ; i < TSample::MeasurementVectorSize ; i++)
    {
      m_Centeroid[i] = m_WeightedCenteroid[i] / double(size) ;
    }

  m_Size = size ;
}

template< class TSample >
void
KdTreeWeightedCenteroidNonterminalNode< TSample >
::GetParameters(unsigned int &partitionDimension, 
                MeasurementType &partitionValue)
{
  partitionDimension = m_PartitionDimension ;
  partitionValue = m_PartitionValue ;
}

template< class TSample >
KdTree< TSample >
::KdTree()
{
  m_EmptyTerminalNode = 
    new KdTreeTerminalNode< TSample >() ;

  m_DistanceMetric = DistanceMetricType::New() ;
}

template< class TSample >
KdTree< TSample >
::~KdTree()
{
  DeleteNode(m_Root) ;
  delete m_EmptyTerminalNode ;
}

template< class TSample >
void
KdTree< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Source Sample: " << m_Sample << std::endl ;
  os << indent << "Bucket Size: " << m_BucketSize << std::endl ;
  os << indent << "Root Node: " << &m_Root << std::endl ;
}

template< class TSample >
void
KdTree< TSample >
::SetSample(TSample* sample)
{
  m_Sample = sample ;
}

template< class TSample >
void
KdTree< TSample >
::SetBucketSize(unsigned int size)
{
  m_BucketSize = size ;
}


template< class TSample >
void
KdTree< TSample >
::Search(MeasurementVectorType &query, unsigned int k)
{
  m_Neighbors.resize(k) ;

  MeasurementVectorType lowerBound ;
  MeasurementVectorType upperBound ;

  for (unsigned int d = 0 ; d < MeasurementVectorSize ; d++)
    {
      lowerBound[d] = NumericTraits< MeasurementType >::NonpositiveMin() ;
      upperBound[d] = NumericTraits< MeasurementType >::max() ;
    }

  m_NumberOfVisits = 0 ;
  m_StopSearch = false ;
  SearchLoop(m_Root, query, lowerBound, upperBound) ;
}

template< class TSample >
inline int
KdTree< TSample >
::SearchLoop(KdTreeNodeType* node, MeasurementVectorType &query, 
             MeasurementVectorType &lowerBound,
             MeasurementVectorType &upperBound)
{
  int i ;
  InstanceIdentifier tempId ;
  double tempDistance ;

  if ( node->IsTerminal() )
    {
      // terminal node
      if (node == m_EmptyTerminalNode)
        {
          // empty node
          return 0 ;
        }

      for (i = 0 ; i < node->Size() ; i++)
        {
          tempId = node->GetInstanceIdentifier(i) ;
          tempDistance = 
            m_DistanceMetric->
            Evaluate(query, m_Sample->GetMeasurementVector(tempId)) ;
          m_NumberOfVisits++ ;
          if (tempDistance < m_Neighbors.GetLargestDistance() )
            {
              m_Neighbors.ReplaceFarthestNeighbor(tempId, tempDistance) ;
            }
        }

      if(BallWithinBounds(query, lowerBound, upperBound))
        {
          return 1 ;
        }

      return 0;
    }


  unsigned int partitionDimension ; 
  MeasurementType partitionValue ;
  MeasurementType tempValue ;
  node->GetParameters(partitionDimension, partitionValue) ;

  if (query[partitionDimension] <= partitionValue)
    {
      // search the closer child node
      tempValue = upperBound[partitionDimension] ;
      upperBound[partitionDimension] = partitionValue ;
      if (SearchLoop(node->Left(), query, lowerBound, upperBound))
        {
          return 1 ;
        }
      upperBound[partitionDimension] = tempValue ;

      // search the other node, if necessary
      tempValue = lowerBound[partitionDimension] ;
      lowerBound[partitionDimension] = partitionValue ;
      if (BoundsOverlapBall(query, lowerBound, upperBound))
        {
          SearchLoop(node->Right(), query, lowerBound, upperBound) ;
        }
      lowerBound[partitionDimension] = tempValue ;
    }
  else
    {
      // search the closer child node
      tempValue = lowerBound[partitionDimension] ;
      lowerBound[partitionDimension] = partitionValue ;
      if (SearchLoop(node->Right(), query, lowerBound, upperBound))
        {
          return 1 ;
        }
      lowerBound[partitionDimension] = tempValue ;

      // search the other node, if necessary
      tempValue = upperBound[partitionDimension] ;
      upperBound[partitionDimension] = partitionValue ;
      if (BoundsOverlapBall(query, lowerBound, upperBound))
        {
          std::cout << "Left child overlaps." << std::endl ;
          SearchLoop(node->Left(), query, lowerBound, upperBound) ;
        }
      upperBound[partitionDimension] = tempValue ;
    }

  // stop or continue search
  if (BallWithinBounds(query, lowerBound, upperBound))
    {
      return 1 ;
    }  

  return 0 ;
}


template< class TSample >
inline bool
KdTree< TSample >
::BallWithinBounds(MeasurementVectorType &query, 
                   MeasurementVectorType &lowerBound,
                   MeasurementVectorType &upperBound)
{
  unsigned int dimension ;
  for (dimension = 0 ; dimension < MeasurementVectorSize ; dimension++)
    {
      if ((m_DistanceMetric->Evaluate(query[dimension] ,
                                      lowerBound[dimension]) <= 
           m_Neighbors.GetLargestDistance()) ||
          (m_DistanceMetric->Evaluate(query[dimension] ,
                                      upperBound[dimension]) <= 
           m_Neighbors.GetLargestDistance()))
        {
          return false ;
        }
    }
  return true ;
}

template< class TSample >
inline bool
KdTree< TSample >
::BoundsOverlapBall(MeasurementVectorType &query, 
                    MeasurementVectorType &lowerBound,
                    MeasurementVectorType &upperBound)
{
  double sum = NumericTraits< double >::Zero ;
  double temp ;
  unsigned int dimension ;
  double squaredSearchRadius = m_Neighbors.GetLargestDistance() ;
  squaredSearchRadius *= squaredSearchRadius ;
  for (dimension = 0  ; dimension < MeasurementVectorSize ; dimension++)
    {

      if (query[dimension] <= lowerBound[dimension])
        {
          temp = m_DistanceMetric->Evaluate(query[dimension], 
                                            lowerBound[dimension]) ;
          sum += temp * temp ;
          if (sum < squaredSearchRadius)
            {
              return true ;
            }
        }
      else if (query[dimension] >= upperBound[dimension])
        {
          temp = m_DistanceMetric->Evaluate(query[dimension], 
                                            upperBound[dimension]) ;
          sum += temp * temp ;
          if (sum < squaredSearchRadius)
            {
              return true ;
            }
        }
    }
  return false ;
}


template< class TSample >
void
KdTree< TSample >
::DeleteNode(KdTreeNodeType *node)
{
  if ( node->IsTerminal() )
    {
      // terminal node
      if (node == m_EmptyTerminalNode)
        {
          // empty node
          return ;
        }
      delete node ;
      return ;
    }

  // non-terminal node
  DeleteNode(node->Left()) ;
  DeleteNode(node->Right()) ;
  delete node ;
}

template< class TSample >
void
KdTree< TSample >
::PrintTree(KdTreeNodeType *node, int level, unsigned int activeDimension)
{
  level++ ;
  if ( node->IsTerminal() )
    {
      // terminal node
      if (node == m_EmptyTerminalNode)
        {
          // empty node
          std::cout << "Empty node: level = " << level << std::endl ;
          return ;
        }

      std::cout << "Terminal: level = " << level 
                << " dim = " << activeDimension<< std::endl ;
      std::cout << "          " ;
      for (int i = 0 ; i < node->Size() ; i++)
        {
          std::cout << "[" << node->GetInstanceIdentifier(i) << "] "
                    << m_Sample->GetMeasurementVector(node->GetInstanceIdentifier(i))[activeDimension] << ", " ;
        }
      std::cout << std::endl ;
      return ;
    }
  
  unsigned int partitionDimension ;
  MeasurementType partitionValue ;

  node->GetParameters(partitionDimension, partitionValue) ;
  typename KdTreeNodeType::CenteroidType centeroid ;
  node->GetWeightedCenteroid(centeroid) ;
  std::cout << "Nonterminal: level = " << level << std::endl ;
  std::cout << "             dim = " << partitionDimension << std::endl ;
  std::cout << "             value = " << partitionValue << std::endl ;
  std::cout << "             weighted centeroid = " 
            << centeroid ;
  std::cout << "             size = " << node->Size()<< std::endl ;
 
  PrintTree(node->Left(), level, partitionDimension) ;
  PrintTree(node->Right(), level, partitionDimension) ;
}


  } // end of namespace Statistics 
} // end of namespace itk

#endif










