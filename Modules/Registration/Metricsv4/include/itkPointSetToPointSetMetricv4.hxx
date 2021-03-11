/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkPointSetToPointSetMetricv4_hxx
#define itkPointSetToPointSetMetricv4_hxx

#include "itkPointSetToPointSetMetricv4.h"

namespace itk
{

template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
typename PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::LocalDerivativeType
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::
  GetLocalNeighborhoodDerivative(const PointType & point, const PixelType & pixel) const
{
  MeasureType         measure;
  LocalDerivativeType localDerivative;
  this->GetLocalNeighborhoodValueAndDerivative(point, measure, localDerivative, pixel);
  return localDerivative;
}

} // end namespace itk

#endif
