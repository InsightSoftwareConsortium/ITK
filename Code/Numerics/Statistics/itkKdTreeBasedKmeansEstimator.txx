/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKdTreeBasedKmeansEstimator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkKdTreeBasedKmeansEstimator_txx
#define _itkKdTreeBasedKmeansEstimator_txx

#include "itkKdTreeBasedKmeansEstimator.h"
#include "itkStatisticsAlgorithm.h"

namespace itk {
namespace Statistics {

template< class TKdTree >
KdTreeBasedKmeansEstimator< TKdTree >
::KdTreeBasedKmeansEstimator()
{
  m_CenteroidPositionChangesThreshold = 0.0 ;
}

template< class TKdTree >
void
KdTreeBasedKmeansEstimator< TKdTree >
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);

  
  os << indent << "Current Iteration: "
     << m_CurrentIteration << std::endl;
  os << indent << "Maximum Iteration: "
     << m_MaximumIteration << std::endl;

  os << indent << "Sum of Centeroid Position Changes: "
     << m_CenteroidPositionChanges << std::endl;
  os << indent << "Threshold for theSum of Centeroid Position Changes: "
     << m_CenteroidPositionChangesThreshold << std::endl;

  os << indent << "Kd Tree:" << m_KdTree << std::endl ;
  os << indent << "Distance Metric: " << m_DistanceMetric << std::endl ;
  
  os << indent << "Initial Centeroids: " << std::endl ;
  int i ;
  for (i = 0 ; i < m_InitialPosition.size() ; i++)
    {
      os << indent << m_InitialPosition[i] << std::endl ;
    }

  os << indent << "Current Centeroids: " << std::endl ;
  for (i = 0 ; i < m_CurrentPosition.size() ; i++)
    {
      os << indent << m_CurrentPosition[i] << std::endl ;
    }

  os << indent << "Temp Vertex: " << m_TempVertex << std::endl ;
}


template< class TKdTree >
double 
KdTreeBasedKmeansEstimator< TKdTree >
::GetSumOfSquaredPositionChanges(ParametersType &previous, 
                                 ParametersType &current) 
{
  double temp ;
  double sum = 0.0 ;
  int i, j ;
  

  for (i = 0 ; i < previous.size() ; i++)
    {
      temp = m_DistanceMetric->Evaluate(previous[i], current[i]) ;
      sum += temp ;
    }
  return sum ;
}

template< class TKdTree >
inline int 
KdTreeBasedKmeansEstimator< TKdTree >
::GetClosestCandidate(ParameterType &measurements, 
                      std::vector< int > &validIndexes)
{

  int closest ;
  double closestDistance = NumericTraits< double >::max() ;
  double tempDistance ;
  std::vector< int >::iterator iter = validIndexes.begin() ;
  while (iter != validIndexes.end())
    {
      tempDistance = 
        m_DistanceMetric->Evaluate(m_CandidateVector[*iter].Centeroid,
                                   measurements) ;
      if (tempDistance < closestDistance)
        {
          closest = *iter ;
          closestDistance = tempDistance ;
        }
      ++iter ;
    }
  return closest ;
}

template< class TKdTree >
inline bool
KdTreeBasedKmeansEstimator< TKdTree >
::IsFarther(ParameterType &pointA,
            ParameterType &pointB,
            MeasurementVectorType &lowerBound,
            MeasurementVectorType &upperBound) 
{
  // calculates the vertex of the Cell bounded by the lowerBound
  // and the upperBound
  for (int i = 0 ; i < MeasurementVectorSize ; i++)
    {
      if ((pointA[i] - pointB[i]) < 0.0)
        {
          m_TempVertex[i] = lowerBound[i] ;
        }
      else
        {
          m_TempVertex[i] = upperBound[i] ;
        }
    }

  if (m_DistanceMetric->Evaluate(pointA, m_TempVertex) >= 
      m_DistanceMetric->Evaluate(pointB, m_TempVertex))
    {
      return true ;
    }

  return false ;
}

template< class TKdTree >
inline void
KdTreeBasedKmeansEstimator< TKdTree >
::Filter(KdTreeNodeType* node, 
         std::vector< int > validIndexes,
         MeasurementVectorType &lowerBound,
         MeasurementVectorType &upperBound)
{
  int i, j ;
  typename TKdTree::InstanceIdentifier tempId ;
  int closest ;
  ParameterType individualPoint ;
  
  if (node->GetNodeType() == KdTreeNodeType::Terminal)
    {
      // terminal node
      if (node == m_KdTree->GetEmptyTerminalNode())
        {
          // empty node
          return ;
        }

      for (i = 0 ; i < node->Size() ; i++)
        {
          tempId = node->GetInstanceIdentifier(i) ;
          this->GetPoint(individualPoint,
                         m_KdTree->GetMeasurementVector(tempId)) ;
          closest = 
            this->GetClosestCandidate(individualPoint, validIndexes) ;
          for (j = 0 ; j < MeasurementVectorSize ; j++)
            {
              m_CandidateVector[closest].WeightedCenteroid[j] +=
                individualPoint[j] ;
            }
          m_CandidateVector[closest].Size += 1 ;
        }
    }
  else
    {
      CenteroidType centeroid ; 
      CenteroidType weightedCenteroid ;
      ParameterType closestPosition ;
      node->GetWeightedCenteroid(weightedCenteroid) ;
      node->GetCenteroid(centeroid) ;

      closest = 
        this->GetClosestCandidate(centeroid, validIndexes) ;
      closestPosition = m_CandidateVector[closest].Centeroid ;
      std::vector< int >::iterator iter = validIndexes.begin() ;
      while (iter != validIndexes.end())
        {
          if (*iter != closest &&
              this->IsFarther(m_CandidateVector[*iter].Centeroid,
                              closestPosition,
                              lowerBound, upperBound))
            {
              iter = validIndexes.erase(iter) ;
              continue ;
            }

          if (iter != validIndexes.end())
            {
              ++iter ;
            }
        }

      if (validIndexes.size() == 1)
        {
          for (j = 0 ; j < MeasurementVectorSize ; j++)
            {
              m_CandidateVector[closest].WeightedCenteroid[j] += 
                weightedCenteroid[j] ;
            }
          m_CandidateVector[closest].Size += node->Size() ;
        }
      else
        {
          unsigned int partitionDimension ; 
          MeasurementType partitionValue ;
          MeasurementType tempValue ;
          node->GetParameters(partitionDimension, partitionValue) ;

          tempValue = upperBound[partitionDimension] ;
          upperBound[partitionDimension] = partitionValue ;
          this->Filter(node->Left(), validIndexes, 
                       lowerBound, upperBound) ;
          upperBound[partitionDimension] = tempValue ;

          tempValue = lowerBound[partitionDimension] ;
          lowerBound[partitionDimension] = partitionValue ;
          this->Filter(node->Right(), validIndexes,
                       lowerBound, upperBound) ;
          lowerBound[partitionDimension] = tempValue ;
        }
    }
}

template< class TKdTree >
void
KdTreeBasedKmeansEstimator< TKdTree >
::CopyParameters(ParametersType &source, ParametersType &target)
{
  int i, j ;
  for (i = 0 ; i < source.size() ; i++)
    {
      for (j = 0 ; j < MeasurementVectorSize ; j++)
        {
          target[i][j] = source[i][j] ;
        }
    }
}

template< class TKdTree >
void
KdTreeBasedKmeansEstimator< TKdTree >
::StartOptimization()
{
  int i ;
  MeasurementVectorType lowerBound ;
  MeasurementVectorType upperBound ;

  FindSampleBound<SampleType>(m_KdTree->GetSample(),
                  m_KdTree->GetSample()->Begin(), 
                  m_KdTree->GetSample()->End(),
                  lowerBound,
                  upperBound) ;

  ParametersType previousPosition ;
  previousPosition.resize(m_InitialPosition.size()) ;
  m_CurrentPosition.resize(m_InitialPosition.size()) ;
  m_DistanceMetric = EuclideanDistance< ParameterType >::New() ;
  this->CopyParameters(m_InitialPosition, m_CurrentPosition) ;
  m_CurrentIteration = 0 ;
  std::vector< int > validIndexes ;

  for (i = 0 ; i < m_InitialPosition.size() ; i++)
    {
      validIndexes.push_back(i) ;
    }

  while(true)
    {
      this->CopyParameters(m_CurrentPosition, previousPosition) ;
      m_CandidateVector.SetCenteroids(m_CurrentPosition) ;
      this->Filter(m_KdTree->GetRoot(), validIndexes,
                   lowerBound, upperBound) ;
      m_CandidateVector.UpdateCenteroids() ;
      m_CandidateVector.GetCenteroids(m_CurrentPosition) ;
      
      if(m_CurrentIteration >= m_MaximumIteration) 
        {
          break ;
        } 

      m_CenteroidPositionChanges = 
        this->GetSumOfSquaredPositionChanges(previousPosition, 
                                             m_CurrentPosition) ;
      if (m_CenteroidPositionChanges <= m_CenteroidPositionChangesThreshold)
        {
          break ;
        }

      m_CurrentIteration++ ;
    }
}

} // end of namespace Statistics
} // end namespace itk

#endif







