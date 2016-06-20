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
#ifndef itkMahalanobisDistanceThresholdImageFunction_hxx
#define itkMahalanobisDistanceThresholdImageFunction_hxx

#include "itkMahalanobisDistanceThresholdImageFunction.h"

namespace itk
{
template< typename TInputImage, typename TCoordRep >
MahalanobisDistanceThresholdImageFunction< TInputImage, TCoordRep >
::MahalanobisDistanceThresholdImageFunction() :
  m_Threshold( NumericTraits< double >::ZeroValue() ),
  m_MahalanobisDistanceMembershipFunction( MahalanobisDistanceFunctionType::New() )
{
}

template< typename TInputImage, typename TCoordRep >
void
MahalanobisDistanceThresholdImageFunction< TInputImage, TCoordRep >
::SetMean(const MeanVectorType & mean)
{
  // Cache the mean
  m_Mean = mean;

  // Set the mean on the membership function
  typename MahalanobisDistanceFunctionType::MeanVectorType m;
  NumericTraits<typename MahalanobisDistanceFunctionType::MeanVectorType>::SetLength(m, mean.size());
  for (unsigned int i=0; i < mean.size(); ++i)
    {
    m[i] = mean[i];
    }
  m_MahalanobisDistanceMembershipFunction->SetMean(m);
}

template< typename TInputImage, typename TCoordRep >
void
MahalanobisDistanceThresholdImageFunction< TInputImage, TCoordRep >
::SetCovariance(const CovarianceMatrixType & covariance)
{
  // Cache the covariance
  m_Covariance = covariance;

  // Set the covariance on the membership function
  typename MahalanobisDistanceFunctionType::CovarianceMatrixType c;
  c = covariance;
  m_MahalanobisDistanceMembershipFunction->SetCovariance(c);
}

template< typename TInputImage, typename TCoordRep >
bool
MahalanobisDistanceThresholdImageFunction< TInputImage, TCoordRep >
::Evaluate(const PointType & point) const
{
  IndexType index;

  this->ConvertPointToNearestIndex(point, index);
  return ( this->EvaluateAtIndex(index) );
}

template< typename TInputImage, typename TCoordRep >
bool
MahalanobisDistanceThresholdImageFunction< TInputImage, TCoordRep >
::EvaluateAtContinuousIndex(const ContinuousIndexType & index) const
{
  IndexType nindex;

  this->ConvertContinuousIndexToNearestIndex (index, nindex);
  return this->EvaluateAtIndex(nindex);
}

template< typename TInputImage, typename TCoordRep >
bool
MahalanobisDistanceThresholdImageFunction< TInputImage, TCoordRep >
::EvaluateAtIndex(const IndexType & index) const
{
  double mahalanobisDistance = this->EvaluateDistanceAtIndex(index);

  return ( mahalanobisDistance <= m_Threshold );
}

template< typename TInputImage, typename TCoordRep >
double
MahalanobisDistanceThresholdImageFunction< TInputImage, TCoordRep >
::EvaluateDistance(const PointType & point) const
{
  IndexType index;

  this->ConvertPointToNearestIndex(point, index);
  const double mahalanobisDistance = this->EvaluateDistanceAtIndex(index);
  return mahalanobisDistance;
}

template< typename TInputImage, typename TCoordRep >
double
MahalanobisDistanceThresholdImageFunction< TInputImage, TCoordRep >
::EvaluateDistanceAtIndex(const IndexType & index) const
{
  double mahalanobisDistanceSquared =
    m_MahalanobisDistanceMembershipFunction->Evaluate(
      this->GetInputImage()->GetPixel(index) );

  double mahalanobisDistance;

  // Deal with cases that are barely negative.
  // In theory they should never appear, but
  // they may happen and would produce NaNs
  // in the std::sqrt
  if ( mahalanobisDistanceSquared < 0.0 )
    {
    mahalanobisDistance = 0.0;
    }
  else
    {
    mahalanobisDistance = std::sqrt(mahalanobisDistanceSquared);
    }

  return mahalanobisDistance;
}

template< typename TInputImage, typename TCoordRep >
void
MahalanobisDistanceThresholdImageFunction< TInputImage, TCoordRep >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Threshold: " << m_Threshold << std::endl;
  os << indent << "Mean: " << m_Mean << std::endl;
  os << indent << "Covariance: " << m_Covariance << std::endl;
  os << indent << "MahalanobisDistanceMembershipFunction: " << m_MahalanobisDistanceMembershipFunction << std::endl;
}
} // end namespace itk

#endif
