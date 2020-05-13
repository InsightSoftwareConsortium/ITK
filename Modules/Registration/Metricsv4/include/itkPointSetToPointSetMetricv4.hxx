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
#include "itkIdentityTransform.h"
#include "itkCompensatedSummation.h"

namespace itk
{

/** Constructor */
template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::PointSetToPointSetMetricv4()
{
  this->m_FixedPointSet = nullptr;  // has to be provided by the user.
  this->m_MovingPointSet = nullptr; // has to be provided by the user.

  this->m_FixedTransformedPointSet = nullptr;
  this->m_MovingTransformedPointSet = nullptr;
  this->m_VirtualTransformedPointSet = nullptr;

  this->m_FixedTransformedPointsLocator = nullptr;
  this->m_MovingTransformedPointsLocator = nullptr;

  this->m_MovingTransformPointLocatorsNeedInitialization = false;
  this->m_FixedTransformPointLocatorsNeedInitialization = false;

  this->m_MovingTransformedPointSetTime = this->GetMTime();
  this->m_FixedTransformedPointSetTime = this->GetMTime();

  // We iterate over the fixed points to calculate the value and derivative.
  this->SetGradientSource(ObjectToObjectMetricBaseTemplateEnums::GradientSource::GRADIENT_SOURCE_FIXED);

  this->m_HaveWarnedAboutNumberOfValidPoints = false;

  this->m_UsePointSetData = false;

  this->m_StoreDerivativeAsSparseFieldForLocalSupportTransforms = true;

  this->m_CalculateValueAndDerivativeInTangentSpace = false;
}

/** Initialize the metric */
template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::Initialize()
{
  if (!this->m_FixedPointSet)
  {
    itkExceptionMacro("Fixed point set is not present");
  }

  if (!this->m_MovingPointSet)
  {
    itkExceptionMacro("Moving point set is not present");
  }

  // We don't know how to support gradient source of type moving
  if (this->GetGradientSourceIncludesMoving())
  {
    itkExceptionMacro("GradientSource includes GRADIENT_SOURCE_MOVING. Not supported.");
  }

  // If the PointSet is provided by a source, update the source.
  if (this->m_MovingPointSet->GetSource())
  {
    this->m_MovingPointSet->GetSource()->Update();
  }

  // If the point set is provided by a source, update the source.
  if (this->m_FixedPointSet->GetSource())
  {
    this->m_FixedPointSet->GetSource()->Update();
  }

  // Check for virtual domain if needed.
  // With local-support transforms we need a virtual domain in
  // order to properly store the per-point derivatives.
  // This will create a virtual domain that matches the DisplacementFieldTransform.
  // If the virutal domain has already been set, it will
  // be verified against the transform in Superclass::Initialize.
  if (this->HasLocalSupport())
  {
    if (!this->m_UserHasSetVirtualDomain)
    {
      const typename DisplacementFieldTransformType::ConstPointer displacementTransform =
        this->GetMovingDisplacementFieldTransform();
      if (displacementTransform.IsNull())
      {
        itkExceptionMacro("Expected the moving transform to be of type DisplacementFieldTransform or derived, "
                          "or a CompositeTransform with DisplacementFieldTransform as the last to have been added.");
      }
      using DisplacementFieldType = typename DisplacementFieldTransformType::DisplacementFieldType;
      typename DisplacementFieldType::ConstPointer field = displacementTransform->GetDisplacementField();
      this->SetVirtualDomain(
        field->GetSpacing(), field->GetOrigin(), field->GetDirection(), field->GetBufferedRegion());
    }
  }

  // Superclass initialization. Do after checking for virtual domain.
  Superclass::Initialize();

  // Call this now for derived classes that need
  // a member to be initialized during Initialize().
  this->InitializePointSets();
}

template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::InitializePointSets() const
{
  this->TransformMovingPointSet();
  this->TransformFixedAndCreateVirtualPointSet();
  this->InitializePointsLocators();
}

template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::InitializeForIteration()
  const
{
  this->InitializePointSets();
  this->m_NumberOfValidPoints = this->CalculateNumberOfValidFixedPoints();
  if (this->m_NumberOfValidPoints < this->GetNumberOfComponents() && !this->m_HaveWarnedAboutNumberOfValidPoints)
  {
    itkWarningMacro("Only " << this->m_NumberOfValidPoints << " of " << this->GetNumberOfComponents()
                            << " points are within the virtual domain, and will be used in the evaluation.");
    this->m_HaveWarnedAboutNumberOfValidPoints = true;
  }
}

template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
SizeValueType
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::GetNumberOfComponents()
  const
{
  return this->m_FixedTransformedPointSet->GetNumberOfPoints();
}

template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
typename PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::MeasureType
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::GetValue() const
{
  this->InitializeForIteration();


  // Virtual point set will be the same size as fixed point set as long as it's
  // generated from the fixed point set.
  if (this->m_VirtualTransformedPointSet->GetNumberOfPoints() != this->m_FixedTransformedPointSet->GetNumberOfPoints())
  {
    itkExceptionMacro("Expected FixedTransformedPointSet to be the same size as VirtualTransformedPointSet.");
  }
  /*
   * Split pointset in nWorkUnit ranges and sum individually
   * This splitting is required in order to avoid having the threads
   * repeatedly write to same location causing false sharing
   */
  // Use STL container to make sure no unesecarry checks are performed
  using FixedTransformedVectorContainer = typename FixedPointsContainer::STLContainerType;
  using VirtualPointsContainer = typename VirtualPointSetType::PointsContainer;
  using VirtualVectorContainer = typename VirtualPointsContainer::STLContainerType;
  const VirtualVectorContainer & virtualTransformedPointSet =
    this->m_VirtualTransformedPointSet->GetPoints()->CastToSTLConstContainer();
  const FixedTransformedVectorContainer & fixedTransformedPointSet =
    this->m_FixedTransformedPointSet->GetPoints()->CastToSTLConstContainer();

  PointIdentifierRanges                          ranges = this->CreateRanges();
  std::vector<CompensatedSummation<MeasureType>> threadValues(ranges.size());
  std::function<void(SizeValueType)>             sumNeighborhoodValues =
    [this, &threadValues, &ranges, &virtualTransformedPointSet, &fixedTransformedPointSet](SizeValueType rangeIndex) {
      CompensatedSummation<MeasureType> threadValue = 0;
      PixelType                         pixel;
      NumericTraits<PixelType>::SetLength(pixel, 1);


      for (PointIdentifier index = ranges[rangeIndex].first; index < ranges[rangeIndex].second; index++)
      {
        if (this->IsInsideVirtualDomain(virtualTransformedPointSet[index]))
        {
          if (this->m_UsePointSetData)
          {
            bool doesPointDataExist = this->m_FixedPointSet->GetPointData(index, &pixel);
            if (!doesPointDataExist)
            {
              itkExceptionMacro("The corresponding data for point (pointId = " << index << ") does not exist.");
            }
          }
          threadValue += this->GetLocalNeighborhoodValue(fixedTransformedPointSet[index], pixel);
        }
      }
      threadValues[rangeIndex] = threadValue;
    };

  // Sum per thread
  MultiThreaderBase::New()->ParallelizeArray(
    (SizeValueType)0, (SizeValueType)ranges.size(), sumNeighborhoodValues, nullptr);
  // Join sums
  CompensatedSummation<MeasureType> value = 0;
  for (unsigned int i = 0; i < threadValues.size(); i++)
  {
    value += threadValues[i];
  }

  DerivativeType derivative;
  MeasureType    valueSum = value.GetSum();
  if (this->VerifyNumberOfValidPoints(valueSum, derivative))
  {
    valueSum /= static_cast<MeasureType>(this->m_NumberOfValidPoints);
  }
  this->m_Value = valueSum;

  return valueSum;
}

template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::GetDerivative(
  DerivativeType & derivative) const
{
  MeasureType value = NumericTraits<MeasureType>::ZeroValue();
  this->CalculateValueAndDerivative(value, derivative, false);
}

template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::GetValueAndDerivative(
  MeasureType &    value,
  DerivativeType & derivative) const
{
  this->CalculateValueAndDerivative(value, derivative, true);
}

template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::CalculateValueAndDerivative(
  MeasureType &    calculatedValue,
  DerivativeType & derivative,
  bool             calculateValue) const
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
      NumericTraits<PixelType>::SetLength(pixel, 1);
      for (PointIdentifier index = ranges[rangeIndex].first; index < ranges[rangeIndex].second; index++)
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
          this->GetLocalNeighborhoodValueAndDerivative(
            fixedTransformedPointSet[index], pointValue, pointDerivative, pixel);
          threadValue += pointValue;
        }
        else
        {
          pointDerivative = this->GetLocalNeighborhoodDerivative(fixedTransformedPointSet[index], pixel);
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

          for (NumberOfParametersType par = 0; par < numberOfLocalParameters; par++)
          {
            for (DimensionType d = 0; d < PointDimension; ++d)
            {
              threadLocalTransformDerivative[par] += jacobian(d, par) * pointDerivative[d];
            }
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
            for (NumberOfParametersType par = 0; par < numberOfLocalParameters; par++)
            {
              derivative[this->GetNumberOfLocalParameters() * index + par] = threadLocalTransformDerivative[par];
            }
          }
        }
        for (NumberOfParametersType par = 0; par < numberOfLocalParameters; par++)
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
  for (unsigned int i = 0; i < threadValues.size(); i++)
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
      for (unsigned int i = 0; i < threadDerivatives.size(); i++)
      {
        for (NumberOfParametersType par = 0; par < numberOfLocalParameters; par++)
        {
          localTransformDerivative[par] += threadDerivatives[i][par];
        }
      }
      derivative.SetSize(numberOfLocalParameters);
      for (NumberOfParametersType par = 0; par < numberOfLocalParameters; par++)
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

template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
SizeValueType
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::
  CalculateNumberOfValidFixedPoints() const
{
  // Determine the number of valid fixed points, using
  // their positions in the virtual domain.
  SizeValueType       numberOfValidPoints = NumericTraits<SizeValueType>::ZeroValue();
  PointsConstIterator virtualIt = this->m_VirtualTransformedPointSet->GetPoints()->Begin();
  while (virtualIt != this->m_VirtualTransformedPointSet->GetPoints()->End())
  {
    if (this->IsInsideVirtualDomain(virtualIt.Value()))
    {
      ++numberOfValidPoints;
    }
    ++virtualIt;
  }
  return numberOfValidPoints;
}

template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::StorePointDerivative(
  const VirtualPointType & virtualPoint,
  const DerivativeType &   pointDerivative,
  DerivativeType &         field) const
{
  // Update derivative field at some index.
  // This requires the active transform displacement field to be the
  // same size as virtual domain, and that VirtualImage PixelType
  // is scalar (both of which are verified during Metric initialization).
  try
  {
    OffsetValueType offset =
      this->ComputeParameterOffsetFromVirtualPoint(virtualPoint, this->GetNumberOfLocalParameters());
    for (NumberOfParametersType i = 0; i < this->GetNumberOfLocalParameters(); i++)
    {
      /* Be sure to *add* here and not assign. Required for proper behavior
       * with multi-variate metric. */
      field[offset + i] += pointDerivative[i];
    }
  }
  catch (ExceptionObject & exc)
  {
    std::string msg("Caught exception: \n");
    msg += exc.what();
    ExceptionObject err(__FILE__, __LINE__, msg);
    throw err;
  }
}

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

template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::TransformMovingPointSet()
  const
{
  // Transform the moving point set with the moving transform.
  // We calculate the value and derivatives in the moving space.
  bool update = !this->m_MovingTransformedPointSet;
  update = update || this->m_MovingTransformedPointSetTime < this->GetMTime();
  update = update || (this->m_CalculateValueAndDerivativeInTangentSpace &&
                      (this->m_MovingTransform->GetMTime() > this->m_MovingTransformedPointSetTime));
  if (update)
  {
    this->m_MovingTransformPointLocatorsNeedInitialization = true;
    this->m_MovingTransformedPointSet = MovingTransformedPointSetType::New();
    this->m_MovingTransformedPointSet->Initialize();

    typename MovingTransformType::InverseTransformBasePointer inverseTransform =
      this->m_MovingTransform->GetInverseTransform();

    typename MovingPointsContainer::ConstIterator It = this->m_MovingPointSet->GetPoints()->Begin();
    while (It != this->m_MovingPointSet->GetPoints()->End())
    {
      if (this->m_CalculateValueAndDerivativeInTangentSpace)
      {
        PointType point = inverseTransform->TransformPoint(It.Value());
        this->m_MovingTransformedPointSet->SetPoint(It.Index(), point);
      }
      else
      {
        // evaluation is performed in moving space, so just copy
        this->m_MovingTransformedPointSet->SetPoint(It.Index(), It.Value());
      }
      ++It;
    }
    this->m_MovingTransformedPointSetTime = this->GetMTime();
    if (!this->m_CalculateValueAndDerivativeInTangentSpace)
    {
      this->m_MovingTransformedPointSetTime =
        std::max(this->m_MovingTransformedPointSetTime, this->m_MovingTransform->GetMTime());
    }
  }
}

template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::
  TransformFixedAndCreateVirtualPointSet() const
{
  // Transform the fixed point set through the virtual domain, and into the moving domain
  bool update = !this->m_FixedTransformedPointSet || !this->m_VirtualTransformedPointSet;
  update = update || this->m_FixedTransformedPointSetTime < this->GetMTime();
  update = update || (this->m_CalculateValueAndDerivativeInTangentSpace &&
                      (this->m_FixedTransform->GetMTime() > this->m_FixedTransformedPointSetTime));
  update = update || (!this->m_CalculateValueAndDerivativeInTangentSpace &&
                      ((this->m_FixedTransform->GetMTime() > this->m_FixedTransformedPointSetTime) ||
                       (this->m_MovingTransform->GetMTime() > this->m_FixedTransformedPointSetTime)));
  if (update)
  {
    this->m_FixedTransformPointLocatorsNeedInitialization = true;
    this->m_FixedTransformedPointSet = FixedTransformedPointSetType::New();
    this->m_FixedTransformedPointSet->Initialize();
    this->m_VirtualTransformedPointSet = VirtualPointSetType::New();
    this->m_VirtualTransformedPointSet->Initialize();

    using InverseTransformBasePointer = typename FixedTransformType::InverseTransformBasePointer;
    InverseTransformBasePointer inverseTransform = this->m_FixedTransform->GetInverseTransform();

    typename FixedPointsContainer::ConstIterator It = this->m_FixedPointSet->GetPoints()->Begin();
    while (It != this->m_FixedPointSet->GetPoints()->End())
    {
      if (this->m_CalculateValueAndDerivativeInTangentSpace)
      {
        // txf into virtual space
        PointType point = inverseTransform->TransformPoint(It.Value());
        this->m_VirtualTransformedPointSet->SetPoint(It.Index(), point);
        this->m_FixedTransformedPointSet->SetPoint(It.Index(), point);
      }
      else
      {
        // txf into virtual space
        PointType point = inverseTransform->TransformPoint(It.Value());
        this->m_VirtualTransformedPointSet->SetPoint(It.Index(), point);
        // txf into moving space
        point = this->m_MovingTransform->TransformPoint(point);
        this->m_FixedTransformedPointSet->SetPoint(It.Index(), point);
      }
      ++It;
    }
    this->m_FixedTransformedPointSetTime = std::max(this->GetMTime(), this->m_FixedTransform->GetMTime());
    if (!this->m_CalculateValueAndDerivativeInTangentSpace)
    {
      this->m_FixedTransformedPointSetTime =
        std::max(this->m_FixedTransformedPointSetTime, this->m_MovingTransform->GetMTime());
    }
  }
}

template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
const typename PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::
  VirtualPointSetType *
  PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::
    GetVirtualTransformedPointSet() const
{
  // First make sure the virtual point set is current.
  this->TransformFixedAndCreateVirtualPointSet();
  return this->m_VirtualTransformedPointSet.GetPointer();
}

template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::InitializePointsLocators()
  const
{
  if (this->RequiresFixedPointsLocator() && this->m_FixedTransformPointLocatorsNeedInitialization)
  {
    if (!this->m_FixedTransformedPointSet)
    {
      itkExceptionMacro("The fixed transformed point set does not exist.");
    }
    if (!this->m_FixedTransformedPointsLocator)
    {
      this->m_FixedTransformedPointsLocator = PointsLocatorType::New();
    }
    this->m_FixedTransformedPointsLocator->SetPoints(this->m_FixedTransformedPointSet->GetPoints());
    this->m_FixedTransformedPointsLocator->Initialize();
  }

  if (this->RequiresMovingPointsLocator() && this->m_MovingTransformPointLocatorsNeedInitialization)
  {
    if (!this->m_MovingTransformedPointSet)
    {
      itkExceptionMacro("The moving transformed point set does not exist.");
    }
    if (!this->m_MovingTransformedPointsLocator)
    {
      this->m_MovingTransformedPointsLocator = PointsLocatorType::New();
    }
    this->m_MovingTransformedPointsLocator->SetPoints(this->m_MovingTransformedPointSet->GetPoints());
    this->m_MovingTransformedPointsLocator->Initialize();
  }
}

template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
const typename PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::
  PointIdentifierRanges
  PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::CreateRanges() const
{
  PointIdentifier nPoints = this->m_FixedTransformedPointSet->GetNumberOfPoints();
  PointIdentifier nWorkUnits = MultiThreaderBase::New()->GetNumberOfWorkUnits();
  if (nWorkUnits > nPoints || MultiThreaderBase::New()->GetMaximumNumberOfThreads() <= 1)
  {
    nWorkUnits = 1;
  }
  PointIdentifier       startRange = 0;
  PointIdentifierRanges ranges;
  for (PointIdentifier p = 1; p < nWorkUnits; ++p)
  {
    PointIdentifier endRange = (p * nPoints) / (double)nWorkUnits;
    ranges.push_back(PointIdentifierPair(startRange, endRange));
    startRange = endRange;
  }
  ranges.push_back(PointIdentifierPair(startRange, nPoints));

  return ranges;
}


/** PrintSelf */
template <typename TFixedPointSet, typename TMovingPointSet, class TInternalComputationValueType>
void
PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>::PrintSelf(
  std::ostream & os,
  Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Fixed PointSet: " << this->m_FixedPointSet.GetPointer() << std::endl;
  os << indent << "Fixed Transform: " << this->m_FixedTransform.GetPointer() << std::endl;
  os << indent << "Moving PointSet: " << this->m_MovingPointSet.GetPointer() << std::endl;
  os << indent << "Moving Transform: " << this->m_MovingTransform.GetPointer() << std::endl;

  os << indent << "Store derivative as sparse field = ";
  if (this->m_StoreDerivativeAsSparseFieldForLocalSupportTransforms)
  {
    os << "true." << std::endl;
  }
  else
  {
    os << "false." << std::endl;
  }

  os << indent << "Calculate in tangent space = ";
  if (this->m_CalculateValueAndDerivativeInTangentSpace)
  {
    os << "true." << std::endl;
  }
  else
  {
    os << "false." << std::endl;
  }
}
} // end namespace itk

#endif
