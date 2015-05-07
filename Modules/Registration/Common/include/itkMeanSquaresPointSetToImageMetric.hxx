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
#ifndef itkMeanSquaresPointSetToImageMetric_hxx
#define itkMeanSquaresPointSetToImageMetric_hxx

#include "itkMeanSquaresPointSetToImageMetric.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{
/**
 * Constructor
 */
template <typename TFixedPointSet, typename TMovingImage>
MeanSquaresPointSetToImageMetric<TFixedPointSet, TMovingImage>
::MeanSquaresPointSetToImageMetric()
{
}

/**
 * Get the match Measure
 */
template <typename TFixedPointSet, typename TMovingImage>
typename MeanSquaresPointSetToImageMetric<TFixedPointSet, TMovingImage>::MeasureType
MeanSquaresPointSetToImageMetric<TFixedPointSet, TMovingImage>
::GetValue(const TransformParametersType & parameters) const
{
  FixedPointSetConstPointer fixedPointSet = this->GetFixedPointSet();

  if( !fixedPointSet )
    {
    itkExceptionMacro(<< "Fixed point set has not been assigned");
    }

  PointIterator pointItr = fixedPointSet->GetPoints()->Begin();
  PointIterator pointEnd = fixedPointSet->GetPoints()->End();

  PointDataIterator pointDataItr = fixedPointSet->GetPointData()->Begin();
  PointDataIterator pointDataEnd = fixedPointSet->GetPointData()->End();

  MeasureType measure = NumericTraits<MeasureType>::ZeroValue();

  this->m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters(parameters);


  while( pointItr != pointEnd && pointDataItr != pointDataEnd )
    {
    InputPointType inputPoint;
    inputPoint.CastFrom( pointItr.Value() );
    OutputPointType transformedPoint =
      this->m_Transform->TransformPoint(inputPoint);

    if( this->m_Interpolator->IsInsideBuffer(transformedPoint) )
      {
      const RealType movingValue  = this->m_Interpolator->Evaluate(transformedPoint);
      const RealType fixedValue   = pointDataItr.Value();
      const RealType diff = movingValue - fixedValue;
      measure += diff * diff;
      this->m_NumberOfPixelsCounted++;
      }

    ++pointItr;
    ++pointDataItr;
    }

  if( !this->m_NumberOfPixelsCounted )
    {
    itkExceptionMacro(<< "All the points mapped to outside of the moving image");
    }
  else
    {
    measure /= this->m_NumberOfPixelsCounted;
    }

  return measure;
}

/**
 * Get the Derivative Measure
 */
template <typename TFixedPointSet, typename TMovingImage>
void
MeanSquaresPointSetToImageMetric<TFixedPointSet, TMovingImage>
::GetDerivative(const TransformParametersType & parameters,
                DerivativeType & derivative) const
{
  if( !this->GetGradientImage() )
    {
    itkExceptionMacro(<< "The gradient image is null, maybe you forgot to call Initialize()");
    }

  FixedPointSetConstPointer fixedPointSet = this->GetFixedPointSet();

  if( !fixedPointSet )
    {
    itkExceptionMacro(<< "Fixed image has not been assigned");
    }

  this->m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters(parameters);

  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType(ParametersDimension);
  derivative.Fill(NumericTraits<typename DerivativeType::ValueType>::ZeroValue());

  PointIterator pointItr = fixedPointSet->GetPoints()->Begin();
  PointIterator pointEnd = fixedPointSet->GetPoints()->End();

  PointDataIterator pointDataItr = fixedPointSet->GetPointData()->Begin();
  PointDataIterator pointDataEnd = fixedPointSet->GetPointData()->End();

  TransformJacobianType jacobian(TMovingImage::ImageDimension,
                                 this->m_Transform->GetNumberOfParameters());
  TransformJacobianType jacobianCache(TMovingImage::ImageDimension,TMovingImage::ImageDimension);

  while( pointItr != pointEnd && pointDataItr != pointDataEnd )
    {
    InputPointType inputPoint;
    inputPoint.CastFrom( pointItr.Value() );
    OutputPointType transformedPoint =
      this->m_Transform->TransformPoint(inputPoint);

    if( this->m_Interpolator->IsInsideBuffer(transformedPoint) )
      {
      const RealType movingValue  = this->m_Interpolator->Evaluate(transformedPoint);
      const RealType fixedValue   = pointDataItr.Value();

      this->m_NumberOfPixelsCounted++;
      const RealType diff = movingValue - fixedValue;

      // Now compute the derivatives
      this->m_Transform->ComputeJacobianWithRespectToParametersCachedTemporaries(inputPoint,
                                                                                 jacobian,
                                                                                 jacobianCache);

      // Get the gradient by NearestNeighboorInterpolation:
      // which is equivalent to round up the point components.
      typedef typename OutputPointType::CoordRepType CoordRepType;
      typedef ContinuousIndex<CoordRepType, MovingImageType::ImageDimension>
      MovingImageContinuousIndexType;

      MovingImageContinuousIndexType tempIndex;
      this->m_MovingImage->TransformPhysicalPointToContinuousIndex(transformedPoint, tempIndex);

      typename MovingImageType::IndexType mappedIndex;
      mappedIndex.CopyWithRound(tempIndex);

      const GradientPixelType gradient =
        this->GetGradientImage()->GetPixel(mappedIndex);
      for( unsigned int par = 0; par < ParametersDimension; par++ )
        {
        RealType sum = NumericTraits<RealType>::ZeroValue();
        for( unsigned int dim = 0; dim < Self::FixedPointSetDimension; dim++ )
          {
          sum += 2.0 *diff *jacobian(dim, par) * gradient[dim];
          }
        derivative[par] += sum;
        }
      }

    ++pointItr;
    ++pointDataItr;
    }

  if( !this->m_NumberOfPixelsCounted )
    {
    itkExceptionMacro(<< "All the points mapped to outside of the moving image");
    }
  else
    {
    for( unsigned int i = 0; i < ParametersDimension; i++ )
      {
      derivative[i] /= this->m_NumberOfPixelsCounted;
      }
    }
}

/*
 * Get both the match Measure and theDerivative Measure
 */
template <typename TFixedPointSet, typename TMovingImage>
void
MeanSquaresPointSetToImageMetric<TFixedPointSet, TMovingImage>
::GetValueAndDerivative(const TransformParametersType & parameters,
                        MeasureType & value, DerivativeType  & derivative) const
{
  if( !this->GetGradientImage() )
    {
    itkExceptionMacro(<< "The gradient image is null, maybe you forgot to call Initialize()");
    }

  FixedPointSetConstPointer fixedPointSet = this->GetFixedPointSet();

  if( !fixedPointSet )
    {
    itkExceptionMacro(<< "Fixed image has not been assigned");
    }

  this->m_NumberOfPixelsCounted = 0;
  MeasureType measure = NumericTraits<MeasureType>::ZeroValue();

  this->SetTransformParameters(parameters);

  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType(ParametersDimension);
  derivative.Fill(NumericTraits<typename DerivativeType::ValueType>::ZeroValue());

  PointIterator pointItr = fixedPointSet->GetPoints()->Begin();
  PointIterator pointEnd = fixedPointSet->GetPoints()->End();

  PointDataIterator pointDataItr = fixedPointSet->GetPointData()->Begin();
  PointDataIterator pointDataEnd = fixedPointSet->GetPointData()->End();

  TransformJacobianType jacobian(TMovingImage::ImageDimension,
                                 this->m_Transform->GetNumberOfParameters());
  TransformJacobianType jacobianCache(TMovingImage::ImageDimension,TMovingImage::ImageDimension);

  while( pointItr != pointEnd && pointDataItr != pointDataEnd )
    {
    InputPointType inputPoint;
    inputPoint.CastFrom( pointItr.Value() );
    OutputPointType transformedPoint =
      this->m_Transform->TransformPoint(inputPoint);

    if( this->m_Interpolator->IsInsideBuffer(transformedPoint) )
      {
      const RealType movingValue  = this->m_Interpolator->Evaluate(transformedPoint);
      const RealType fixedValue   = pointDataItr.Value();

      this->m_NumberOfPixelsCounted++;

      // Now compute the derivatives
      this->m_Transform->ComputeJacobianWithRespectToParametersCachedTemporaries(inputPoint,
                                                                                 jacobian,
                                                                                 jacobianCache);

      const RealType diff = movingValue - fixedValue;

      measure += diff * diff;

      // Get the gradient by NearestNeighboorInterpolation:
      // which is equivalent to round up the point components.
      typedef typename OutputPointType::CoordRepType CoordRepType;
      typedef ContinuousIndex<CoordRepType, MovingImageType::ImageDimension>
      MovingImageContinuousIndexType;

      MovingImageContinuousIndexType tempIndex;
      this->m_MovingImage->TransformPhysicalPointToContinuousIndex(transformedPoint, tempIndex);

      typename MovingImageType::IndexType mappedIndex;
      mappedIndex.CopyWithRound(tempIndex);

      const GradientPixelType gradient =
        this->GetGradientImage()->GetPixel(mappedIndex);
      for( unsigned int par = 0; par < ParametersDimension; par++ )
        {
        RealType sum = NumericTraits<RealType>::ZeroValue();
        for( unsigned int dim = 0; dim < Self::FixedPointSetDimension; dim++ )
          {
          sum += 2.0 *diff *jacobian(dim, par) * gradient[dim];
          }
        derivative[par] += sum;
        }
      }

    ++pointItr;
    ++pointDataItr;
    }

  if( !this->m_NumberOfPixelsCounted )
    {
    itkExceptionMacro(<< "All the points mapped to outside of the moving image");
    }
  else
    {
    for( unsigned int i = 0; i < ParametersDimension; i++ )
      {
      derivative[i] /= this->m_NumberOfPixelsCounted;
      }
    measure /= this->m_NumberOfPixelsCounted;
    }

  value = measure;
}

} // end namespace itk

#endif
