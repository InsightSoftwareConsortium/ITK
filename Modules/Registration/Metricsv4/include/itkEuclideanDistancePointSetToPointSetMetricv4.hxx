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
#ifndef itkEuclideanDistancePointSetToPointSetMetricv4_hxx
#define itkEuclideanDistancePointSetToPointSetMetricv4_hxx

#include "itkEuclideanDistancePointSetToPointSetMetricv4.h"

namespace itk
{

/** Constructor */
template<typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
EuclideanDistancePointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::EuclideanDistancePointSetToPointSetMetricv4()
{
}

/** Destructor */
template<typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
EuclideanDistancePointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::~EuclideanDistancePointSetToPointSetMetricv4()
{
}

template<typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
typename EuclideanDistancePointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::MeasureType
EuclideanDistancePointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::GetLocalNeighborhoodValue( const PointType & point, const PixelType & itkNotUsed( pixel ) ) const
{
  PointType closestPoint;
  closestPoint.Fill( 0.0 );

  PointIdentifier pointId = this->m_MovingTransformedPointsLocator->FindClosestPoint( point );
  closestPoint = this->m_MovingTransformedPointSet->GetPoint( pointId );

  const MeasureType distance = point.EuclideanDistanceTo( closestPoint );
  return distance;
}

template<typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
EuclideanDistancePointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::GetLocalNeighborhoodValueAndDerivative( const PointType & point,
  MeasureType &measure, LocalDerivativeType & localDerivative, const PixelType & itkNotUsed( pixel ) ) const
{
  PointType closestPoint;
  closestPoint.Fill( 0.0 );

  PointIdentifier pointId = this->m_MovingTransformedPointsLocator->FindClosestPoint( point );
  closestPoint = this->m_MovingTransformedPointSet->GetPoint( pointId );

  measure = point.EuclideanDistanceTo( closestPoint );
  localDerivative = closestPoint - point;
}

/** PrintSelf method */
template<typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
EuclideanDistancePointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
::PrintSelf( std::ostream & os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );
}

} // end namespace itk

#endif
