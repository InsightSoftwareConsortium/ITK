/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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


namespace itk
{

template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
typename EuclideanDistancePointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::
  MeasureType
  EuclideanDistancePointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::
    GetLocalNeighborhoodValue(const PointType & point, const PixelType & itkNotUsed(pixel)) const
{
  PointType closestPoint;
  closestPoint.Fill(0.0);

  PointIdentifier pointId = this->m_MovingTransformedPointsLocator->FindClosestPoint(point);
  closestPoint = this->m_MovingTransformedPointSet->GetPoint(pointId);

  const MeasureType distance = point.EuclideanDistanceTo(closestPoint);
  if (this->m_DistanceThreshold <= 0 || distance < this->m_DistanceThreshold)
  {
    return distance;
  }
  else
  {
    return 0;
  }
}

template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
EuclideanDistancePointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::
  GetLocalNeighborhoodValueAndDerivative(const PointType &     point,
                                         MeasureType &         measure,
                                         LocalDerivativeType & localDerivative,
                                         const PixelType &     itkNotUsed(pixel)) const
{
  PointType closestPoint;
  closestPoint.Fill(0.0);

  PointIdentifier pointId = this->m_MovingTransformedPointsLocator->FindClosestPoint(point);
  closestPoint = this->m_MovingTransformedPointSet->GetPoint(pointId);

  auto distance = point.EuclideanDistanceTo(closestPoint);

  if (this->m_DistanceThreshold <= 0 || distance < this->m_DistanceThreshold)
  {
    measure = distance;
    localDerivative = closestPoint - point;
  }
  else
  {
    // Skip the points that are beyond the threshold by making value and derivative as 0.
    measure = 0;
    closestPoint.Fill(0.0);
    localDerivative = closestPoint;
  }
}

/** PrintSelf method */
template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
EuclideanDistancePointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::PrintSelf(
  std::ostream & os,
  Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
