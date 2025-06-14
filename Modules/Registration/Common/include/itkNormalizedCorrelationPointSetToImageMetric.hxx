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
#ifndef itkNormalizedCorrelationPointSetToImageMetric_hxx
#define itkNormalizedCorrelationPointSetToImageMetric_hxx

#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

template <typename TFixedPointSet, typename TMovingImage>
NormalizedCorrelationPointSetToImageMetric<TFixedPointSet, TMovingImage>::NormalizedCorrelationPointSetToImageMetric() =
  default;

template <typename TFixedPointSet, typename TMovingImage>
auto
NormalizedCorrelationPointSetToImageMetric<TFixedPointSet, TMovingImage>::GetValue(
  const TransformParametersType & parameters) const -> MeasureType
{
  const FixedPointSetConstPointer fixedPointSet = this->GetFixedPointSet();

  if (!fixedPointSet)
  {
    itkExceptionMacro("Fixed point set has not been assigned");
  }

  PointIterator       pointItr = fixedPointSet->GetPoints()->Begin();
  const PointIterator pointEnd = fixedPointSet->GetPoints()->End();

  PointDataIterator       pointDataItr = fixedPointSet->GetPointData()->Begin();
  const PointDataIterator pointDataEnd = fixedPointSet->GetPointData()->End();

  MeasureType measure;

  this->m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters(parameters);

  using AccumulateType = typename NumericTraits<MeasureType>::AccumulateType;

  AccumulateType sff{};
  AccumulateType smm{};
  AccumulateType sfm{};
  AccumulateType sf{};
  AccumulateType sm{};

  while (pointItr != pointEnd && pointDataItr != pointDataEnd)
  {
    InputPointType inputPoint;
    inputPoint.CastFrom(pointItr.Value());
    const OutputPointType transformedPoint = this->m_Transform->TransformPoint(inputPoint);

    if (this->m_Interpolator->IsInsideBuffer(transformedPoint))
    {
      const RealType movingValue = this->m_Interpolator->Evaluate(transformedPoint);
      const RealType fixedValue = pointDataItr.Value();
      sff += fixedValue * fixedValue;
      smm += movingValue * movingValue;
      sfm += fixedValue * movingValue;
      if (this->m_SubtractMean)
      {
        sf += fixedValue;
        sm += movingValue;
      }
      this->m_NumberOfPixelsCounted++;
    }

    ++pointItr;
    ++pointDataItr;
  }

  if (this->m_SubtractMean && this->m_NumberOfPixelsCounted > 0)
  {
    sff -= (sf * sf / this->m_NumberOfPixelsCounted);
    smm -= (sm * sm / this->m_NumberOfPixelsCounted);
    sfm -= (sf * sm / this->m_NumberOfPixelsCounted);
  }

  const RealType denom = -1.0 * std::sqrt(sff * smm);

  if (this->m_NumberOfPixelsCounted > 0 && denom != 0.0)
  {
    measure = sfm / denom;
  }
  else
  {
    measure = MeasureType{};
  }

  return measure;
}

template <typename TFixedPointSet, typename TMovingImage>
void
NormalizedCorrelationPointSetToImageMetric<TFixedPointSet, TMovingImage>::GetDerivative(
  const TransformParametersType & parameters,
  DerivativeType &                derivative) const
{
  if (!this->GetGradientImage())
  {
    itkExceptionMacro("The gradient image is null, maybe you forgot to call Initialize()");
  }

  const FixedPointSetConstPointer fixedPointSet = this->GetFixedPointSet();

  if (!fixedPointSet)
  {
    itkExceptionMacro("Fixed image has not been assigned");
  }

  const unsigned int dimension = Superclass::FixedPointSetDimension;

  this->m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters(parameters);

  using AccumulateType = typename NumericTraits<MeasureType>::AccumulateType;

  AccumulateType sff{};
  AccumulateType smm{};
  AccumulateType sfm{};
  AccumulateType sf{};
  AccumulateType sm{};

  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType(ParametersDimension);
  derivative.Fill(typename DerivativeType::ValueType{});

  DerivativeType derivativeF(ParametersDimension);
  derivativeF.Fill(typename DerivativeType::ValueType{});

  DerivativeType derivativeM(ParametersDimension);
  derivativeM.Fill(typename DerivativeType::ValueType{});

  DerivativeType derivativeO(ParametersDimension);
  derivativeO.Fill(typename DerivativeType::ValueType{});

  PointIterator       pointItr = fixedPointSet->GetPoints()->Begin();
  const PointIterator pointEnd = fixedPointSet->GetPoints()->End();

  PointDataIterator       pointDataItr = fixedPointSet->GetPointData()->Begin();
  const PointDataIterator pointDataEnd = fixedPointSet->GetPointData()->End();

  TransformJacobianType jacobian(TMovingImage::ImageDimension, this->m_Transform->GetNumberOfParameters());
  TransformJacobianType jacobianCache(TMovingImage::ImageDimension, TMovingImage::ImageDimension);

  while (pointItr != pointEnd && pointDataItr != pointDataEnd)
  {
    InputPointType inputPoint;
    inputPoint.CastFrom(pointItr.Value());
    const OutputPointType transformedPoint = this->m_Transform->TransformPoint(inputPoint);

    if (this->m_Interpolator->IsInsideBuffer(transformedPoint))
    {
      const RealType movingValue = this->m_Interpolator->Evaluate(transformedPoint);
      const RealType fixedValue = pointDataItr.Value();

      // First compute the sums
      sff += fixedValue * fixedValue;
      smm += movingValue * movingValue;
      sfm += fixedValue * movingValue;
      if (this->m_SubtractMean)
      {
        sf += fixedValue;
        sm += movingValue;
      }
      this->m_NumberOfPixelsCounted++;

      // Now compute the derivatives
      this->m_Transform->ComputeJacobianWithRespectToParametersCachedTemporaries(inputPoint, jacobian, jacobianCache);

      // Get the gradient by NearestNeighborInterpolation:
      // which is equivalent to round up the point components.
      using CoordinateType = typename OutputPointType::CoordinateType;
      using MovingImageContinuousIndexType = ContinuousIndex<CoordinateType, MovingImageType::ImageDimension>;

      const MovingImageContinuousIndexType tempIndex =
        this->GetMovingImage()->template TransformPhysicalPointToContinuousIndex<CoordinateType>(transformedPoint);

      typename MovingImageType::IndexType mappedIndex;
      mappedIndex.CopyWithRound(tempIndex);

      const GradientPixelType gradient = this->GetGradientImage()->GetPixel(mappedIndex);
      for (unsigned int par = 0; par < ParametersDimension; ++par)
      {
        RealType sumD{};
        for (unsigned int dim = 0; dim < dimension; ++dim)
        {
          const RealType differential = jacobian(dim, par) * gradient[dim];
          sumD += differential;
        }
        derivativeF[par] += sumD * fixedValue;
        derivativeM[par] += sumD * movingValue;
        if (this->m_SubtractMean)
        {
          derivativeO[par] += sumD;
        }
      }
    }

    ++pointItr;
    ++pointDataItr;
  }

  if (this->m_SubtractMean && this->m_NumberOfPixelsCounted > 0)
  {
    sff -= (sf * sf / this->m_NumberOfPixelsCounted);
    smm -= (sm * sm / this->m_NumberOfPixelsCounted);
    sfm -= (sf * sm / this->m_NumberOfPixelsCounted);
    for (unsigned int par = 0; par < ParametersDimension; ++par)
    {
      derivativeF[par] -= derivativeO[par] * sf / this->m_NumberOfPixelsCounted;
      derivativeM[par] -= derivativeO[par] * sm / this->m_NumberOfPixelsCounted;
    }
  }

  const RealType denom = -1.0 * std::sqrt(sff * smm);

  if (this->m_NumberOfPixelsCounted > 0 && denom != 0.0)
  {
    for (unsigned int i = 0; i < ParametersDimension; ++i)
    {
      derivative[i] = (derivativeF[i] - (sfm / smm) * derivativeM[i]) / denom;
    }
  }
  else
  {
    for (unsigned int i = 0; i < ParametersDimension; ++i)
    {
      derivative[i] = MeasureType{};
    }
  }
}

template <typename TFixedPointSet, typename TMovingImage>
void
NormalizedCorrelationPointSetToImageMetric<TFixedPointSet, TMovingImage>::GetValueAndDerivative(
  const TransformParametersType & parameters,
  MeasureType &                   value,
  DerivativeType &                derivative) const
{
  if (!this->GetGradientImage())
  {
    itkExceptionMacro("The gradient image is null, maybe you forgot to call Initialize()");
  }

  const FixedPointSetConstPointer fixedPointSet = this->GetFixedPointSet();

  if (!fixedPointSet)
  {
    itkExceptionMacro("Fixed image has not been assigned");
  }

  const unsigned int dimension = Superclass::FixedPointSetDimension;

  this->m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters(parameters);

  using AccumulateType = typename NumericTraits<MeasureType>::AccumulateType;

  AccumulateType sff{};
  AccumulateType smm{};
  AccumulateType sfm{};
  AccumulateType sf{};
  AccumulateType sm{};

  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType(ParametersDimension);
  derivative.Fill(typename DerivativeType::ValueType{});

  DerivativeType derivativeF(ParametersDimension);
  derivativeF.Fill(typename DerivativeType::ValueType{});

  DerivativeType derivativeM(ParametersDimension);
  derivativeM.Fill(typename DerivativeType::ValueType{});

  DerivativeType derivativeO(ParametersDimension);
  derivativeO.Fill(typename DerivativeType::ValueType{});

  PointIterator       pointItr = fixedPointSet->GetPoints()->Begin();
  const PointIterator pointEnd = fixedPointSet->GetPoints()->End();

  PointDataIterator       pointDataItr = fixedPointSet->GetPointData()->Begin();
  const PointDataIterator pointDataEnd = fixedPointSet->GetPointData()->End();

  TransformJacobianType jacobian(TMovingImage::ImageDimension, this->m_Transform->GetNumberOfParameters());
  TransformJacobianType jacobianCache(TMovingImage::ImageDimension, TMovingImage::ImageDimension);

  while (pointItr != pointEnd && pointDataItr != pointDataEnd)
  {
    InputPointType inputPoint;
    inputPoint.CastFrom(pointItr.Value());
    const OutputPointType transformedPoint = this->m_Transform->TransformPoint(inputPoint);

    if (this->m_Interpolator->IsInsideBuffer(transformedPoint))
    {
      const RealType movingValue = this->m_Interpolator->Evaluate(transformedPoint);
      const RealType fixedValue = pointDataItr.Value();

      // First compute the sums
      sff += fixedValue * fixedValue;
      smm += movingValue * movingValue;
      sfm += fixedValue * movingValue;
      if (this->m_SubtractMean)
      {
        sf += fixedValue;
        sm += movingValue;
      }
      this->m_NumberOfPixelsCounted++;

      // Now compute the derivatives
      this->m_Transform->ComputeJacobianWithRespectToParametersCachedTemporaries(inputPoint, jacobian, jacobianCache);

      // Get the gradient by NearestNeighborInterpolation:
      // which is equivalent to round up the point components.
      using CoordinateType = typename OutputPointType::CoordinateType;
      using MovingImageContinuousIndexType = ContinuousIndex<CoordinateType, MovingImageType::ImageDimension>;

      const MovingImageContinuousIndexType tempIndex =
        this->GetMovingImage()->template TransformPhysicalPointToContinuousIndex<CoordinateType>(transformedPoint);

      typename MovingImageType::IndexType mappedIndex;
      mappedIndex.CopyWithRound(tempIndex);

      const GradientPixelType gradient = this->GetGradientImage()->GetPixel(mappedIndex);
      for (unsigned int par = 0; par < ParametersDimension; ++par)
      {
        RealType sumD{};
        for (unsigned int dim = 0; dim < dimension; ++dim)
        {
          const RealType differential = jacobian(dim, par) * gradient[dim];
          sumD += differential;
        }
        derivativeF[par] += sumD * fixedValue;
        derivativeM[par] += sumD * movingValue;
        if (this->m_SubtractMean)
        {
          derivativeO[par] += sumD;
        }
      }
    }

    ++pointItr;
    ++pointDataItr;
  }

  if (this->m_SubtractMean && this->m_NumberOfPixelsCounted > 0)
  {
    sff -= (sf * sf / this->m_NumberOfPixelsCounted);
    smm -= (sm * sm / this->m_NumberOfPixelsCounted);
    sfm -= (sf * sm / this->m_NumberOfPixelsCounted);
    for (unsigned int par = 0; par < ParametersDimension; ++par)
    {
      derivativeF[par] -= derivativeO[par] * sf / this->m_NumberOfPixelsCounted;
      derivativeM[par] -= derivativeO[par] * sm / this->m_NumberOfPixelsCounted;
    }
  }

  const RealType denom = -1.0 * std::sqrt(sff * smm);

  if (this->m_NumberOfPixelsCounted > 0 && denom != 0.0)
  {
    for (unsigned int i = 0; i < ParametersDimension; ++i)
    {
      derivative[i] = (derivativeF[i] - (sfm / smm) * derivativeM[i]) / denom;
    }
    value = sfm / denom;
  }
  else
  {
    for (unsigned int i = 0; i < ParametersDimension; ++i)
    {
      derivative[i] = MeasureType{};
    }
    value = MeasureType{};
  }
}

template <typename TFixedImage, typename TMovingImage>
void
NormalizedCorrelationPointSetToImageMetric<TFixedImage, TMovingImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "SubtractMean: " << m_SubtractMean << std::endl;
}

} // end namespace itk

#endif
