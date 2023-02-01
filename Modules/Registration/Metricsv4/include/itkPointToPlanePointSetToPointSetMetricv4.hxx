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
#ifndef itkPointToPlanePointSetToPointSetMetricv4_hxx
#define itkPointToPlanePointSetToPointSetMetricv4_hxx


namespace itk
{

template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
PointToPlanePointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::
  PointToPlanePointSetToPointSetMetricv4()
{
  this->m_UsePointSetData = true;
}

template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
typename PointToPlanePointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::
  MeasureType
  PointToPlanePointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::
    GetLocalNeighborhoodValue(const PointType & point, const PixelType & itkNotUsed(pixel)) const
{
  PointType closestPoint;
  closestPoint.Fill(0.0);

  PointIdentifier pointId = this->m_MovingTransformedPointsLocator->FindClosestPoint(point);
  closestPoint = this->m_MovingTransformedPointSet->GetPoint(pointId);

  const MeasureType distance = point.EuclideanDistanceTo(closestPoint);
  return distance;
}

template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
PointToPlanePointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::
  GetLocalNeighborhoodValueAndDerivative(const PointType &     point,
                                         MeasureType &         measure,
                                         LocalDerivativeType & localDerivative,
                                         const PixelType &     pixel) const
{
  PointType closestPoint;
  closestPoint.Fill(0.0);

  PointIdentifier pointId = this->m_MovingTransformedPointsLocator->FindClosestPoint(point);
  closestPoint = this->m_MovingTransformedPointSet->GetPoint(pointId);

  measure = 0;
  auto temp_diff = closestPoint - point;
  for (int i = 0; i < pixel.Size(); ++i)
  {
    measure = measure + temp_diff[i] * pixel[i];
  }
  measure = measure * measure;

  localDerivative = closestPoint - point;

  // Perform dot product with the normal vector
  for (int i = 0; i < pixel.Size(); ++i)
  {
    localDerivative[i] = localDerivative[i] * pixel[i];
  }
}


template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
PointToPlanePointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::
  CalculateValueAndDerivative(MeasureType & calculatedValue, DerivativeType & derivative, bool calculateValue) const
{
  this->InitializeForIteration();

  // Virtual point set will be the same size as fixed point set as long as it's
  // generated from the fixed point set.
  if (this->m_VirtualTransformedPointSet->GetNumberOfPoints() != this->m_FixedTransformedPointSet->GetNumberOfPoints())
  {
    itkExceptionMacro("Expected FixedTransformedPointSet to be the same size as VirtualTransformedPointSet.");
  }

  derivative.SetSize(this->GetNumberOfParameters());
  if (!this->GetStoreDerivativeAsSparseFieldForLocalSupportTransforms())
  {
    derivative.SetSize(PointDimension * this->m_FixedTransformedPointSet->GetNumberOfPoints());
  }
  derivative.Fill(NumericTraits<DerivativeValueType>::ZeroValue());

  /*
   * Split pointset in nWorkUnits ranges and sum individually
   * This splitting is required in order to avoid having the threads
   * repeatedly write to same location causing false sharing
   */
  // GetNumberOfLocalParameters is not trhead safe in itkCompositeTransform
  NumberOfParametersType                         numberOfLocalParameters = this->GetNumberOfLocalParameters();
  PointIdentifierRanges                          ranges = this->CreateRanges();
  std::vector<CompensatedSummation<MeasureType>> threadValues(ranges.size());
  using CompensatedDerivative = typename std::vector<CompensatedSummation<ParametersValueType>>;
  std::vector<CompensatedDerivative> threadDerivatives(ranges.size());
  std::function<void(SizeValueType)> sumNeighborhoodValues =
    [this, &derivative, &threadDerivatives, &threadValues, &ranges, &calculateValue, &numberOfLocalParameters](
      SizeValueType rangeIndex) {
      // Use STL container to make sure no unesecarry checks are performed
      using FixedTransformedVectorContainer = typename FixedPointsContainer::STLContainerType;
      using VirtualPointsContainer = typename VirtualPointSetType::PointsContainer;
      using VirtualVectorContainer = typename VirtualPointsContainer::STLContainerType;
      const VirtualVectorContainer & virtualTransformedPointSet =
        this->m_VirtualTransformedPointSet->GetPoints()->CastToSTLConstContainer();
      const FixedTransformedVectorContainer & fixedTransformedPointSet =
        this->m_FixedTransformedPointSet->GetPoints()->CastToSTLConstContainer();

      MovingTransformJacobianType jacobian(MovingPointDimension, numberOfLocalParameters);
      MovingTransformJacobianType jacobianCache;

      DerivativeType threadLocalTransformDerivative(numberOfLocalParameters);
      threadLocalTransformDerivative.Fill(NumericTraits<DerivativeValueType>::ZeroValue());

      CompensatedDerivative threadDerivativeSum(numberOfLocalParameters);

      CompensatedSummation<MeasureType> threadValue;
      PixelType                         pixel;
      // NumericTraits<PixelType>::SetLength(pixel, 1);
      for (PointIdentifier index = ranges[rangeIndex].first; index < ranges[rangeIndex].second; ++index)
      {
        MeasureType         pointValue = NumericTraits<MeasureType>::ZeroValue();
        LocalDerivativeType pointDerivative;

        /* Verify the virtual point is in the virtual domain.
         * If user hasn't defined a virtual space, and the active transform is not
         * a displacement field transform type, then this will always return true. */
        if (!this->IsInsideVirtualDomain(virtualTransformedPointSet[index]))
        {
          continue;
        }

        if (this->m_UsePointSetData)
        {
          bool doesPointDataExist = this->m_FixedPointSet->GetPointData(index, &pixel);
          if (!doesPointDataExist)
          {
            itkExceptionMacro("The corresponding data for point with id " << index << " does not exist.");
          }
        }

        if (calculateValue)
        {
          this->GetLocalNeighborhoodValueAndDerivativeWithIndex(
            index, fixedTransformedPointSet[index], pointValue, pointDerivative, pixel);
          threadValue += pointValue;
        }
        else
        {
          pointDerivative =
            this->GetLocalNeighborhoodDerivativeWithIndex(index, fixedTransformedPointSet[index], pixel);
        }

        // Map into parameter space
        threadLocalTransformDerivative.Fill(NumericTraits<DerivativeValueType>::ZeroValue());

        if (this->m_CalculateValueAndDerivativeInTangentSpace)
        {
          for (DimensionType d = 0; d < PointDimension; ++d)
          {
            threadLocalTransformDerivative[d] += pointDerivative[d];
          }
        }
        else
        {
          this->GetMovingTransform()->ComputeJacobianWithRespectToParametersCachedTemporaries(
            virtualTransformedPointSet[index], jacobian, jacobianCache);

          float new_jacobian[numberOfLocalParameters] = { 0 };

          for (NumberOfParametersType par = 0; par < numberOfLocalParameters; ++par)
          {
            // for (DimensionType d = 0; d < PointDimension; ++d)
            // {
            //   auto temp_jd = jacobian(d, par);
            //   threadLocalTransformDerivative[par] += temp_jd * pointDerivative[d];
            // }

            // Writing new jacobian by taking dot product with the normal
            // auto checking_pixel = pixel;

            for (DimensionType d = 0; d < PointDimension; ++d)
            {
              // Use pixel here instead of pointDerivative
              // Override this method in the new class
              new_jacobian[par] = new_jacobian[par] + jacobian(d, par) * pointDerivative[d];
              // threadLocalTransformDerivative[par] += temp_jd * pointDerivative[d];
            }

            // perform dot product summation here of the dot product error
            // threadLocalTransformDerivative[par] += temp_jd * (pointDerivative[0] + pointDerivative[1]);
          }

          for (NumberOfParametersType par = 0; par < numberOfLocalParameters; ++par)
          {
            // perform dot product summation here of the dot product error with new jacobian
            threadLocalTransformDerivative[par] += new_jacobian[par] * (pointDerivative[0] + pointDerivative[1]);
          }
        }
        // For local-support transforms, store the per-point result
        if (this->HasLocalSupport() || this->m_CalculateValueAndDerivativeInTangentSpace)
        {
          if (this->GetStoreDerivativeAsSparseFieldForLocalSupportTransforms())
          {
            this->StorePointDerivative(virtualTransformedPointSet[index], threadLocalTransformDerivative, derivative);
          }
          else
          {
            for (NumberOfParametersType par = 0; par < numberOfLocalParameters; ++par)
            {
              derivative[this->GetNumberOfLocalParameters() * index + par] = threadLocalTransformDerivative[par];
            }
          }
        }
        for (NumberOfParametersType par = 0; par < numberOfLocalParameters; ++par)
        {
          threadDerivativeSum[par] += threadLocalTransformDerivative[par];
        }
      }
      threadValues[rangeIndex] = threadValue;
      threadDerivatives[rangeIndex] = threadDerivativeSum;
    };

  // Sum per thread
  MultiThreaderBase::New()->ParallelizeArray(
    (SizeValueType)0, (SizeValueType)ranges.size(), sumNeighborhoodValues, nullptr);

  // Sum thread results
  CompensatedSummation<MeasureType> value = 0;
  for (unsigned int i = 0; i < threadValues.size(); ++i)
  {
    value += threadValues[i];
  }
  MeasureType valueSum = value.GetSum();

  if (this->VerifyNumberOfValidPoints(valueSum, derivative))
  {
    // For global-support transforms, average the accumulated derivative result
    if (!this->HasLocalSupport() && !this->m_CalculateValueAndDerivativeInTangentSpace)
    {
      CompensatedDerivative localTransformDerivative(numberOfLocalParameters);
      for (unsigned int i = 0; i < threadDerivatives.size(); ++i)
      {
        for (NumberOfParametersType par = 0; par < numberOfLocalParameters; ++par)
        {
          localTransformDerivative[par] += threadDerivatives[i][par];
        }
      }
      derivative.SetSize(numberOfLocalParameters);
      for (NumberOfParametersType par = 0; par < numberOfLocalParameters; ++par)
      {
        derivative[par] =
          localTransformDerivative[par].GetSum() / static_cast<DerivativeValueType>(this->m_NumberOfValidPoints);
      }
    }
    valueSum /= static_cast<MeasureType>(this->m_NumberOfValidPoints);
  }
  calculatedValue = valueSum;
  this->m_Value = valueSum;
}

/** PrintSelf method */
template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
PointToPlanePointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::PrintSelf(
  std::ostream & os,
  Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
