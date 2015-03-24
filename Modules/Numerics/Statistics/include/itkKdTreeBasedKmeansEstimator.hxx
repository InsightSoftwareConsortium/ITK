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
#ifndef itkKdTreeBasedKmeansEstimator_hxx
#define itkKdTreeBasedKmeansEstimator_hxx

#include "itkKdTreeBasedKmeansEstimator.h"
#include "itkStatisticsAlgorithm.h"

namespace itk
{
namespace Statistics
{
template< typename TKdTree >
KdTreeBasedKmeansEstimator< TKdTree >
::KdTreeBasedKmeansEstimator() :
  m_CurrentIteration(0),
  m_MaximumIteration(100),
  m_CentroidPositionChanges(0.0),
  m_CentroidPositionChangesThreshold(0.0),
  m_KdTree(ITK_NULLPTR),
  m_DistanceMetric(EuclideanDistanceMetric< ParameterType >::New()),
  m_UseClusterLabels(false),
  m_GenerateClusterLabels(false),
  m_MeasurementVectorSize(0),
  m_MembershipFunctionsObject(MembershipFunctionVectorObjectType::New())
{
  m_TempVertex.Fill(0.0);
}

template< typename TKdTree >
void
KdTreeBasedKmeansEstimator< TKdTree >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Current Iteration: "
     << this->GetCurrentIteration() << std::endl;
  os << indent << "Maximum Iteration: "
     << this->GetMaximumIteration() << std::endl;

  os << indent << "Sum of Centroid Position Changes: "
     << this->GetCentroidPositionChanges() << std::endl;
  os << indent << "Threshold for the Sum of Centroid Position Changes: "
     << this->GetCentroidPositionChangesThreshold() << std::endl;

  os << indent << "Kd Tree:";
  if ( m_KdTree.IsNotNull() )
    {
    os << this->GetKdTree() << std::endl;
    }
  else
    {
    os << "not set." << std::endl;
    }

  os << indent << "Parameters: " << this->GetParameters() << std::endl;
  os << indent << "MeasurementVectorSize: " << this->GetMeasurementVectorSize() << std::endl;
  os << indent << "UseClusterLabels: " << this->GetUseClusterLabels() << std::endl;
}

template< typename TKdTree >
double
KdTreeBasedKmeansEstimator< TKdTree >
::GetSumOfSquaredPositionChanges(InternalParametersType & previous,
                                 InternalParametersType & current)
{
  double       temp;
  double       sum = 0.0;
  unsigned int i;

  for ( i = 0; i < (unsigned int)previous.size(); i++ )
    {
    temp = m_DistanceMetric->Evaluate(previous[i], current[i]);
    sum += temp;
    }
  return sum;
}

template< typename TKdTree >
inline int
KdTreeBasedKmeansEstimator< TKdTree >
::GetClosestCandidate(ParameterType & measurements,
                      std::vector< int > & validIndexes)
{
  int    closest = 0;
  double closestDistance = NumericTraits< double >::max();
  double tempDistance;

  std::vector< int >::iterator iter = validIndexes.begin();
  while ( iter != validIndexes.end() )
    {
    tempDistance =
      m_DistanceMetric->Evaluate(m_CandidateVector[*iter].Centroid,
                                 measurements);
    if ( tempDistance < closestDistance )
      {
      closest = *iter;
      closestDistance = tempDistance;
      }
    ++iter;
    }
  return closest;
}

template< typename TKdTree >
inline bool
KdTreeBasedKmeansEstimator< TKdTree >
::IsFarther(ParameterType & pointA,
            ParameterType & pointB,
            MeasurementVectorType & lowerBound,
            MeasurementVectorType & upperBound)
{
  // calculates the vertex of the Cell bounded by the lowerBound
  // and the upperBound
  for ( unsigned int i = 0; i < m_MeasurementVectorSize; i++ )
    {
    if ( ( pointA[i] - pointB[i] ) < 0.0 )
      {
      m_TempVertex[i] = lowerBound[i];
      }
    else
      {
      m_TempVertex[i] = upperBound[i];
      }
    }

  if ( m_DistanceMetric->Evaluate(pointA, m_TempVertex) >=
       m_DistanceMetric->Evaluate(pointB, m_TempVertex) )
    {
    return true;
    }

  return false;
}

template< typename TKdTree >
inline void
KdTreeBasedKmeansEstimator< TKdTree >
::Filter(KdTreeNodeType *node,
         std::vector< int > validIndexes,
         MeasurementVectorType & lowerBound,
         MeasurementVectorType & upperBound)
{
  unsigned int i, j;

  typename TKdTree::InstanceIdentifier tempId;
  int           closest;
  ParameterType individualPoint;
  NumericTraits<ParameterType>::SetLength(individualPoint,
    this->m_MeasurementVectorSize);

  if ( node->IsTerminal() )
    {
    // terminal node
    if ( node == m_KdTree->GetEmptyTerminalNode() )
      {
      // empty node
      return;
      }

    for ( i = 0; i < (unsigned int)node->Size(); i++ )
      {
      tempId = node->GetInstanceIdentifier(i);
      this->GetPoint( individualPoint,
                      m_KdTree->GetMeasurementVector(tempId) );
      closest =
        this->GetClosestCandidate(individualPoint, validIndexes);
      for ( j = 0; j < m_MeasurementVectorSize; j++ )
        {
        m_CandidateVector[closest].WeightedCentroid[j] +=
          individualPoint[j];
        }
      m_CandidateVector[closest].Size += 1;
      if ( m_GenerateClusterLabels )
        {
        m_ClusterLabels[tempId] = closest;
        }
      }
    }
  else
    {
    CentroidType  centroid;
    CentroidType  weightedCentroid;
    ParameterType closestPosition;
    node->GetWeightedCentroid(weightedCentroid);
    node->GetCentroid(centroid);

    closest =
      this->GetClosestCandidate(centroid, validIndexes);
    closestPosition = m_CandidateVector[closest].Centroid;
    std::vector< int >::iterator iter = validIndexes.begin();

    while ( iter != validIndexes.end() )
      {
      if ( *iter != closest
           && this->IsFarther(m_CandidateVector[*iter].Centroid,
                              closestPosition,
                              lowerBound, upperBound) )
        {
        iter = validIndexes.erase(iter);
        continue;
        }

      if ( iter != validIndexes.end() )
        {
        ++iter;
        }
      }

    if ( validIndexes.size() == 1 )
      {
      for ( j = 0; j < m_MeasurementVectorSize; j++ )
        {
        m_CandidateVector[closest].WeightedCentroid[j] +=
          weightedCentroid[j];
        }
      m_CandidateVector[closest].Size += node->Size();
      if ( m_GenerateClusterLabels )
        {
        this->FillClusterLabels(node, closest);
        }
      }
    else
      {
      unsigned int    partitionDimension;
      MeasurementType partitionValue;
      MeasurementType tempValue;
      node->GetParameters(partitionDimension, partitionValue);

      tempValue = upperBound[partitionDimension];
      upperBound[partitionDimension] = partitionValue;
      this->Filter(node->Left(), validIndexes,
                   lowerBound, upperBound);
      upperBound[partitionDimension] = tempValue;

      tempValue = lowerBound[partitionDimension];
      lowerBound[partitionDimension] = partitionValue;
      this->Filter(node->Right(), validIndexes,
                   lowerBound, upperBound);
      lowerBound[partitionDimension] = tempValue;
      }
    }
}

template< typename TKdTree >
void
KdTreeBasedKmeansEstimator< TKdTree >
::FillClusterLabels(KdTreeNodeType *node, int closestIndex)
{
  unsigned int i;

  if ( node->IsTerminal() )
    {
    // terminal node
    if ( node == m_KdTree->GetEmptyTerminalNode() )
      {
      // empty node
      return;
      }

    for ( i = 0; i < (unsigned int)node->Size(); i++ )
      {
      m_ClusterLabels[node->GetInstanceIdentifier(i)] = closestIndex;
      }
    }
  else
    {
    this->FillClusterLabels(node->Left(), closestIndex);
    this->FillClusterLabels(node->Right(), closestIndex);
    }
}

template< typename TKdTree >
void
KdTreeBasedKmeansEstimator< TKdTree >
::CopyParameters(ParametersType & source, InternalParametersType & target)
{
  unsigned int i, j;
  int          index = 0;

  for ( i = 0; i < (unsigned int)( source.size() / m_MeasurementVectorSize ); i++ )
    {
    for ( j = 0; j < m_MeasurementVectorSize; j++ )
      {
      target[i][j] = source[index];
      ++index;
      }
    }
}

template< typename TKdTree >
void
KdTreeBasedKmeansEstimator< TKdTree >
::CopyParameters(InternalParametersType & source, ParametersType & target)
{
  unsigned int i, j;
  int          index = 0;

  for ( i = 0; i < (unsigned int)source.size(); i++ )
    {
    for ( j = 0; j < m_MeasurementVectorSize; j++ )
      {
      target[index] = source[i][j];
      ++index;
      }
    }
}

template< typename TKdTree >
void
KdTreeBasedKmeansEstimator< TKdTree >
::CopyParameters(InternalParametersType & source, InternalParametersType & target)
{
  unsigned int i, j;

  for ( i = 0; i < (unsigned int)source.size(); i++ )
    {
    for ( j = 0; j < m_MeasurementVectorSize; j++ )
      {
      target[i][j] = source[i][j];
      }
    }
}

template< typename TKdTree >
void
KdTreeBasedKmeansEstimator< TKdTree >
::StartOptimization()
{
  unsigned int          i;
  MeasurementVectorType lowerBound;
  MeasurementVectorType upperBound;

  NumericTraits<MeasurementVectorType>::SetLength(lowerBound, this->m_MeasurementVectorSize);
  NumericTraits<MeasurementVectorType>::SetLength(upperBound, this->m_MeasurementVectorSize);

  Algorithm::FindSampleBound< SampleType >(m_KdTree->GetSample(),
                                           m_KdTree->GetSample()->Begin(),
                                           m_KdTree->GetSample()->End(),
                                           lowerBound,
                                           upperBound);

  InternalParametersType previousPosition;
  //previousPosition.resize(m_Parameters.size() / m_MeasurementVectorSize);
  InternalParametersType currentPosition;
  //currentPosition.resize(m_Parameters.size() / m_MeasurementVectorSize);

  for ( i = 0; i < m_Parameters.size() / m_MeasurementVectorSize; i++ )
    {
    ParameterType m;
    ParameterType m1;
    NumericTraits<ParameterType>::SetLength(m, m_MeasurementVectorSize);
    NumericTraits<ParameterType>::SetLength(m1, m_MeasurementVectorSize);
    previousPosition.push_back(m);
    currentPosition.push_back(m1);
    }

  this->CopyParameters(m_Parameters, currentPosition);
  m_CurrentIteration = 0;
  std::vector< int > validIndexes;

  for ( i = 0; i < (unsigned int)( m_Parameters.size() / m_MeasurementVectorSize ); i++ )
    {
    validIndexes.push_back(i);
    }

  m_GenerateClusterLabels = false;

  while ( true )
    {
    this->CopyParameters(currentPosition, previousPosition);
    m_CandidateVector.SetCentroids(currentPosition);
    this->Filter(m_KdTree->GetRoot(), validIndexes,
                 lowerBound, upperBound);
    m_CandidateVector.UpdateCentroids();
    m_CandidateVector.GetCentroids(currentPosition);

    if ( m_CurrentIteration >= m_MaximumIteration )
      {
      break;
      }

    m_CentroidPositionChanges =
      this->GetSumOfSquaredPositionChanges(previousPosition,
                                           currentPosition);
    if ( m_CentroidPositionChanges <= m_CentroidPositionChangesThreshold )
      {
      break;
      }

    m_CurrentIteration++;
    }

  if ( m_UseClusterLabels )
    {
    m_GenerateClusterLabels = true;
    m_ClusterLabels.clear();
    m_ClusterLabels.resize( m_KdTree->GetSample()->Size() );
    for ( i = 0; i < (unsigned int)( m_Parameters.size() / m_MeasurementVectorSize ); i++ )
      {
      validIndexes.push_back(i);
      }

    this->Filter(m_KdTree->GetRoot(), validIndexes,
                 lowerBound, upperBound);
    }

  this->CopyParameters(currentPosition, m_Parameters);
}

template< typename TKdTree >
void
KdTreeBasedKmeansEstimator< TKdTree >
::SetKdTree(TKdTree *tree)
{
  m_KdTree = tree;
  m_MeasurementVectorSize = tree->GetMeasurementVectorSize();
  m_DistanceMetric->SetMeasurementVectorSize(m_MeasurementVectorSize);
  NumericTraits<ParameterType>::SetLength(m_TempVertex, m_MeasurementVectorSize);
  this->Modified();
}

template< typename TKdTree >
const TKdTree *
KdTreeBasedKmeansEstimator< TKdTree >
::GetKdTree() const
{
  return m_KdTree.GetPointer();
}

template< typename TKdTree >
const typename KdTreeBasedKmeansEstimator< TKdTree >::MembershipFunctionVectorObjectType *
KdTreeBasedKmeansEstimator< TKdTree >
::GetOutput() const
{
  //INSERT CHECKS if all the required inputs are set and optmization has been
  // run.
  unsigned int                   numberOfClasses = m_Parameters.size() / m_MeasurementVectorSize;
  MembershipFunctionVectorType & membershipFunctionsVector = m_MembershipFunctionsObject->Get();

  for ( unsigned int i = 0; i < numberOfClasses; i++ )
    {
    DistanceToCentroidMembershipFunctionPointer membershipFunction =
      DistanceToCentroidMembershipFunctionType::New();
    membershipFunction->SetMeasurementVectorSize(m_MeasurementVectorSize);
    typename DistanceToCentroidMembershipFunctionType::CentroidType centroid;
    centroid.SetSize(m_MeasurementVectorSize);
    for ( unsigned int j = 0; j < m_MeasurementVectorSize; j++ )
      {
      unsigned int parameterIndex = i * m_MeasurementVectorSize + j;
      centroid[j] = m_Parameters[parameterIndex];
      }
    membershipFunction->SetCentroid(centroid);
    membershipFunctionsVector.push_back( membershipFunction.GetPointer() );
    }

  return static_cast< const MembershipFunctionVectorObjectType * >( m_MembershipFunctionsObject );
}

template< typename TKdTree >
void
KdTreeBasedKmeansEstimator< TKdTree >
::GetPoint(ParameterType & point,
           MeasurementVectorType measurements)
{
  for ( unsigned int i = 0; i < m_MeasurementVectorSize; i++ )
    {
    point[i] = measurements[i];
    }
}

template< typename TKdTree >
void
KdTreeBasedKmeansEstimator< TKdTree >
::PrintPoint(ParameterType & point)
{
  std::cout << "[ ";
  for ( unsigned int i = 0; i < m_MeasurementVectorSize; i++ )
    {
    std::cout << point[i] << " ";
    }
  std::cout << "]";
}
} // end of namespace Statistics
} // end namespace itk

#endif
