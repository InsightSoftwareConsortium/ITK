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
#ifndef itkMeanReciprocalSquareDifferencePointSetToImageMetric_hxx
#define itkMeanReciprocalSquareDifferencePointSetToImageMetric_hxx

#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

template <typename TFixedPointSet, typename TMovingImage>
MeanReciprocalSquareDifferencePointSetToImageMetric<TFixedPointSet,
                                                    TMovingImage>::MeanReciprocalSquareDifferencePointSetToImageMetric()
{
  m_Lambda = 1.0;
}

template <typename TFixedPointSet, typename TMovingImage>
auto
MeanReciprocalSquareDifferencePointSetToImageMetric<TFixedPointSet, TMovingImage>::GetValue(
  const TransformParametersType & parameters) const -> MeasureType
{
  FixedPointSetConstPointer fixedPointSet = this->GetFixedPointSet();

  if (!fixedPointSet)
  {
    itkExceptionMacro("Fixed point set has not been assigned");
  }

  PointIterator pointItr = fixedPointSet->GetPoints()->Begin();
  PointIterator pointEnd = fixedPointSet->GetPoints()->End();

  PointDataIterator pointDataItr = fixedPointSet->GetPointData()->Begin();
  PointDataIterator pointDataEnd = fixedPointSet->GetPointData()->End();

  MeasureType measure{};

  this->m_NumberOfPixelsCounted = 0;
  double lambdaSquared = std::pow(this->m_Lambda, 2);

  this->SetTransformParameters(parameters);

  while (pointItr != pointEnd && pointDataItr != pointDataEnd)
  {
    InputPointType inputPoint;
    inputPoint.CastFrom(pointItr.Value());
    OutputPointType transformedPoint = this->m_Transform->TransformPoint(inputPoint);

    if (this->m_Interpolator->IsInsideBuffer(transformedPoint))
    {
      const RealType movingValue = this->m_Interpolator->Evaluate(transformedPoint);
      const RealType fixedValue = pointDataItr.Value();
      const RealType diff = movingValue - fixedValue;
      const double   diffSquared = diff * diff;
      measure += 1.0 / (lambdaSquared + diffSquared);
      this->m_NumberOfPixelsCounted++;
    }

    ++pointItr;
    ++pointDataItr;
  }

  if (!this->m_NumberOfPixelsCounted)
  {
    itkExceptionMacro("All the points mapped to outside of the moving image");
  }
  else
  {
    measure *= (lambdaSquared / this->m_NumberOfPixelsCounted);
  }

  return measure;
}

template <typename TFixedPointSet, typename TMovingImage>
void
MeanReciprocalSquareDifferencePointSetToImageMetric<TFixedPointSet, TMovingImage>::GetDerivative(
  const TransformParametersType & parameters,
  DerivativeType &                derivative) const
{
  if (!this->GetGradientImage())
  {
    itkExceptionMacro("The gradient image is null, maybe you forgot to call Initialize()");
  }

  FixedPointSetConstPointer fixedPointSet = this->GetFixedPointSet();

  if (!fixedPointSet)
  {
    itkExceptionMacro("Fixed image has not been assigned");
  }

  this->m_NumberOfPixelsCounted = 0;

  double lambdaSquared = std::pow(this->m_Lambda, 2);

  this->SetTransformParameters(parameters);

  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType(ParametersDimension);
  derivative.Fill(NumericTraits<typename DerivativeType::ValueType>::ZeroValue());

  PointIterator pointItr = fixedPointSet->GetPoints()->Begin();
  PointIterator pointEnd = fixedPointSet->GetPoints()->End();

  PointDataIterator pointDataItr = fixedPointSet->GetPointData()->Begin();
  PointDataIterator pointDataEnd = fixedPointSet->GetPointData()->End();

  TransformJacobianType jacobian(TMovingImage::ImageDimension, this->m_Transform->GetNumberOfParameters());
  TransformJacobianType jacobianCache;

  while (pointItr != pointEnd && pointDataItr != pointDataEnd)
  {
    InputPointType inputPoint;
    inputPoint.CastFrom(pointItr.Value());
    OutputPointType transformedPoint = this->m_Transform->TransformPoint(inputPoint);

    if (this->m_Interpolator->IsInsideBuffer(transformedPoint))
    {
      const RealType movingValue = this->m_Interpolator->Evaluate(transformedPoint);
      const RealType fixedValue = pointDataItr.Value();

      this->m_NumberOfPixelsCounted++;
      const RealType diff = movingValue - fixedValue;
      const RealType diffSquared = diff * diff;

      // Now compute the derivatives
      this->m_Transform->ComputeJacobianWithRespectToParametersCachedTemporaries(inputPoint, jacobian, jacobianCache);

      // Get the gradient by NearestNeighborInterpolation:
      // which is equivalent to round up the point components.
      using CoordRepType = typename OutputPointType::CoordRepType;
      using MovingImageContinuousIndexType = ContinuousIndex<CoordRepType, MovingImageType::ImageDimension>;

      const MovingImageContinuousIndexType tempIndex =
        this->m_MovingImage->template TransformPhysicalPointToContinuousIndex<CoordRepType>(transformedPoint);

      typename MovingImageType::IndexType mappedIndex;
      mappedIndex.CopyWithRound(tempIndex);

      const GradientPixelType gradient = this->GetGradientImage()->GetPixel(mappedIndex);
      for (unsigned int par = 0; par < ParametersDimension; ++par)
      {
        RealType sum{};
        for (unsigned int dim = 0; dim < Self::FixedPointSetDimension; ++dim)
        {
          // Will it be computationally more efficient to instead calculate the
          // derivative using finite differences ?
          sum -= jacobian(dim, par) * gradient[dim] / (std::pow(lambdaSquared + diffSquared, 2));
        }
        derivative[par] += diff * sum;
      }
    }

    ++pointItr;
    ++pointDataItr;
  }

  if (!this->m_NumberOfPixelsCounted)
  {
    itkExceptionMacro("All the points mapped to outside of the moving image");
  }
  else
  {
    for (unsigned int i = 0; i < ParametersDimension; ++i)
    {
      derivative[i] *= 2.0 * lambdaSquared / this->m_NumberOfPixelsCounted;
    }
  }
}

template <typename TFixedPointSet, typename TMovingImage>
void
MeanReciprocalSquareDifferencePointSetToImageMetric<TFixedPointSet, TMovingImage>::GetValueAndDerivative(
  const TransformParametersType & parameters,
  MeasureType &                   value,
  DerivativeType &                derivative) const
{
  if (!this->GetGradientImage())
  {
    itkExceptionMacro("The gradient image is null, maybe you forgot to call Initialize()");
  }

  FixedPointSetConstPointer fixedPointSet = this->GetFixedPointSet();

  if (!fixedPointSet)
  {
    itkExceptionMacro("Fixed image has not been assigned");
  }

  this->m_NumberOfPixelsCounted = 0;
  MeasureType measure{};

  this->SetTransformParameters(parameters);
  double lambdaSquared = std::pow(this->m_Lambda, 2);

  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType(ParametersDimension);
  derivative.Fill(NumericTraits<typename DerivativeType::ValueType>::ZeroValue());

  PointIterator pointItr = fixedPointSet->GetPoints()->Begin();
  PointIterator pointEnd = fixedPointSet->GetPoints()->End();

  PointDataIterator pointDataItr = fixedPointSet->GetPointData()->Begin();
  PointDataIterator pointDataEnd = fixedPointSet->GetPointData()->End();

  TransformJacobianType jacobian(TMovingImage::ImageDimension, this->m_Transform->GetNumberOfParameters());
  TransformJacobianType jacobianCache(TMovingImage::ImageDimension, TMovingImage::ImageDimension);
  while (pointItr != pointEnd && pointDataItr != pointDataEnd)
  {
    InputPointType inputPoint;
    inputPoint.CastFrom(pointItr.Value());
    OutputPointType transformedPoint = this->m_Transform->TransformPoint(inputPoint);

    if (this->m_Interpolator->IsInsideBuffer(transformedPoint))
    {
      const RealType movingValue = this->m_Interpolator->Evaluate(transformedPoint);
      const RealType fixedValue = pointDataItr.Value();

      this->m_NumberOfPixelsCounted++;

      // Now compute the derivatives
      this->m_Transform->ComputeJacobianWithRespectToParametersCachedTemporaries(inputPoint, jacobian, jacobianCache);

      const RealType diff = movingValue - fixedValue;
      const RealType diffSquared = diff * diff;
      measure += 1.0 / (lambdaSquared + diffSquared);

      // Get the gradient by NearestNeighborInterpolation:
      // which is equivalent to round up the point components.
      using CoordRepType = typename OutputPointType::CoordRepType;
      using MovingImageContinuousIndexType = ContinuousIndex<CoordRepType, MovingImageType::ImageDimension>;

      const MovingImageContinuousIndexType tempIndex =
        this->m_MovingImage->template TransformPhysicalPointToContinuousIndex<CoordRepType>(transformedPoint);

      typename MovingImageType::IndexType mappedIndex;
      mappedIndex.CopyWithRound(tempIndex);

      const GradientPixelType gradient = this->GetGradientImage()->GetPixel(mappedIndex);
      for (unsigned int par = 0; par < ParametersDimension; ++par)
      {
        RealType sum{};
        for (unsigned int dim = 0; dim < Self::FixedPointSetDimension; ++dim)
        {
          sum -= jacobian(dim, par) * gradient[dim] * std::pow(lambdaSquared + diffSquared, 2);
        }
        derivative[par] += diff * sum;
      }
    }

    ++pointItr;
    ++pointDataItr;
  }

  if (!this->m_NumberOfPixelsCounted)
  {
    itkExceptionMacro("All the points mapped to outside of the moving image");
  }
  else
  {
    for (unsigned int i = 0; i < ParametersDimension; ++i)
    {
      derivative[i] *= 2.0 * lambdaSquared / this->m_NumberOfPixelsCounted;
    }
    measure *= lambdaSquared / this->m_NumberOfPixelsCounted;
  }

  value = measure;
}

template <typename TFixedPointSet, typename TMovingImage>
void
MeanReciprocalSquareDifferencePointSetToImageMetric<TFixedPointSet, TMovingImage>::PrintSelf(std::ostream & os,
                                                                                             Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Lambda: " << m_Lambda << std::endl;
}

} // end namespace itk

#endif
