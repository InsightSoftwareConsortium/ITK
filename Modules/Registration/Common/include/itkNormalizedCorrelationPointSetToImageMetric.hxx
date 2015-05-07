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
#ifndef itkNormalizedCorrelationPointSetToImageMetric_hxx
#define itkNormalizedCorrelationPointSetToImageMetric_hxx

#include "itkNormalizedCorrelationPointSetToImageMetric.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{
/**
 * Constructor
 */
template <typename TFixedPointSet, typename TMovingImage>
NormalizedCorrelationPointSetToImageMetric<TFixedPointSet, TMovingImage>
::NormalizedCorrelationPointSetToImageMetric()
{
  m_SubtractMean = false;
}

/**
 * Get the match Measure
 */
template <typename TFixedPointSet, typename TMovingImage>
typename NormalizedCorrelationPointSetToImageMetric<TFixedPointSet, TMovingImage>::MeasureType
NormalizedCorrelationPointSetToImageMetric<TFixedPointSet, TMovingImage>
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

  MeasureType measure;

  this->m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters(parameters);

  typedef  typename NumericTraits<MeasureType>::AccumulateType AccumulateType;

  AccumulateType sff = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType smm = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType sfm = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType sf  = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType sm  = NumericTraits<AccumulateType>::ZeroValue();

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
      sff += fixedValue  * fixedValue;
      smm += movingValue * movingValue;
      sfm += fixedValue  * movingValue;
      if( this->m_SubtractMean )
        {
        sf += fixedValue;
        sm += movingValue;
        }
      this->m_NumberOfPixelsCounted++;
      }

    ++pointItr;
    ++pointDataItr;
    }

  if( this->m_SubtractMean && this->m_NumberOfPixelsCounted > 0 )
    {
    sff -= ( sf * sf / this->m_NumberOfPixelsCounted );
    smm -= ( sm * sm / this->m_NumberOfPixelsCounted );
    sfm -= ( sf * sm / this->m_NumberOfPixelsCounted );
    }

  const RealType denom = -1.0 * std::sqrt(sff * smm);

  if( this->m_NumberOfPixelsCounted > 0 && denom != 0.0 )
    {
    measure = sfm / denom;
    }
  else
    {
    measure = NumericTraits<MeasureType>::ZeroValue();
    }

  return measure;
}

/**
 * Get the Derivative Measure
 */
template <typename TFixedPointSet, typename TMovingImage>
void
NormalizedCorrelationPointSetToImageMetric<TFixedPointSet, TMovingImage>
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

  const unsigned int dimension = Superclass::FixedPointSetDimension;

  this->m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters(parameters);

  typedef  typename NumericTraits<MeasureType>::AccumulateType AccumulateType;

  AccumulateType sff  = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType smm  = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType sfm  = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType sf  = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType sm  = NumericTraits<AccumulateType>::ZeroValue();

  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType(ParametersDimension);
  derivative.Fill(NumericTraits<typename DerivativeType::ValueType>::ZeroValue());

  DerivativeType derivativeF = DerivativeType(ParametersDimension);
  derivativeF.Fill(NumericTraits<typename DerivativeType::ValueType>::ZeroValue());

  DerivativeType derivativeM = DerivativeType(ParametersDimension);
  derivativeM.Fill(NumericTraits<typename DerivativeType::ValueType>::ZeroValue());

  DerivativeType derivativeO = DerivativeType(ParametersDimension);
  derivativeO.Fill(NumericTraits<typename DerivativeType::ValueType>::ZeroValue());

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

      // First compute the sums
      sff += fixedValue  * fixedValue;
      smm += movingValue * movingValue;
      sfm += fixedValue  * movingValue;
      if( this->m_SubtractMean )
        {
        sf += fixedValue;
        sm += movingValue;
        }
      this->m_NumberOfPixelsCounted++;

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
      this->GetMovingImage()->TransformPhysicalPointToContinuousIndex(transformedPoint, tempIndex);

      typename MovingImageType::IndexType mappedIndex;
      mappedIndex.CopyWithRound(tempIndex);

      const GradientPixelType gradient =
        this->GetGradientImage()->GetPixel(mappedIndex);
      for( unsigned int par = 0; par < ParametersDimension; par++ )
        {
        RealType sumD = NumericTraits<RealType>::ZeroValue();
        for( unsigned int dim = 0; dim < dimension; dim++ )
          {
          const RealType differential = jacobian(dim, par) * gradient[dim];
          sumD += differential;
          }
        derivativeF[par] += sumD * fixedValue;
        derivativeM[par] += sumD * movingValue;
        if( this->m_SubtractMean )
          {
          derivativeO[par] += sumD;
          }
        }
      }

    ++pointItr;
    ++pointDataItr;
    }

  if( this->m_SubtractMean && this->m_NumberOfPixelsCounted > 0 )
    {
    sff -= ( sf * sf / this->m_NumberOfPixelsCounted );
    smm -= ( sm * sm / this->m_NumberOfPixelsCounted );
    sfm -= ( sf * sm / this->m_NumberOfPixelsCounted );
    for( unsigned int par = 0; par < ParametersDimension; par++ )
      {
      derivativeF[par] -= derivativeO[par] * sf / this->m_NumberOfPixelsCounted;
      derivativeM[par] -= derivativeO[par] * sm / this->m_NumberOfPixelsCounted;
      }
    }

  const RealType denom = -1.0 * std::sqrt(sff * smm);

  if( this->m_NumberOfPixelsCounted > 0 && denom != 0.0 )
    {
    for( unsigned int i = 0; i < ParametersDimension; i++ )
      {
      derivative[i] = ( derivativeF[i] - ( sfm / smm ) * derivativeM[i] ) / denom;
      }
    }
  else
    {
    for( unsigned int i = 0; i < ParametersDimension; i++ )
      {
      derivative[i] = NumericTraits<MeasureType>::ZeroValue();
      }
    }
}

/*
 * Get both the match Measure and theDerivative Measure
 */
template <typename TFixedPointSet, typename TMovingImage>
void
NormalizedCorrelationPointSetToImageMetric<TFixedPointSet, TMovingImage>
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

  const unsigned int dimension = Superclass::FixedPointSetDimension;

  this->m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters(parameters);

  typedef  typename NumericTraits<MeasureType>::AccumulateType AccumulateType;

  AccumulateType sff  = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType smm  = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType sfm  = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType sf  = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType sm  = NumericTraits<AccumulateType>::ZeroValue();

  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType(ParametersDimension);
  derivative.Fill(NumericTraits<typename DerivativeType::ValueType>::ZeroValue());

  DerivativeType derivativeF = DerivativeType(ParametersDimension);
  derivativeF.Fill(NumericTraits<typename DerivativeType::ValueType>::ZeroValue());

  DerivativeType derivativeM = DerivativeType(ParametersDimension);
  derivativeM.Fill(NumericTraits<typename DerivativeType::ValueType>::ZeroValue());

  DerivativeType derivativeO = DerivativeType(ParametersDimension);
  derivativeO.Fill(NumericTraits<typename DerivativeType::ValueType>::ZeroValue());

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

      // First compute the sums
      sff += fixedValue  * fixedValue;
      smm += movingValue * movingValue;
      sfm += fixedValue  * movingValue;
      if( this->m_SubtractMean )
        {
        sf += fixedValue;
        sm += movingValue;
        }
      this->m_NumberOfPixelsCounted++;

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
      this->GetMovingImage()->TransformPhysicalPointToContinuousIndex(transformedPoint, tempIndex);

      typename MovingImageType::IndexType mappedIndex;
      mappedIndex.CopyWithRound(tempIndex);

      const GradientPixelType gradient =
        this->GetGradientImage()->GetPixel(mappedIndex);
      for( unsigned int par = 0; par < ParametersDimension; par++ )
        {
        RealType sumD = NumericTraits<RealType>::ZeroValue();
        for( unsigned int dim = 0; dim < dimension; dim++ )
          {
          const RealType differential = jacobian(dim, par) * gradient[dim];
          sumD += differential;
          }
        derivativeF[par] += sumD * fixedValue;
        derivativeM[par] += sumD * movingValue;
        if( this->m_SubtractMean )
          {
          derivativeO[par] += sumD;
          }
        }
      }

    ++pointItr;
    ++pointDataItr;
    }

  if( this->m_SubtractMean && this->m_NumberOfPixelsCounted > 0 )
    {
    sff -= ( sf * sf / this->m_NumberOfPixelsCounted );
    smm -= ( sm * sm / this->m_NumberOfPixelsCounted );
    sfm -= ( sf * sm / this->m_NumberOfPixelsCounted );
    for( unsigned int par = 0; par < ParametersDimension; par++ )
      {
      derivativeF[par] -= derivativeO[par] * sf / this->m_NumberOfPixelsCounted;
      derivativeM[par] -= derivativeO[par] * sm / this->m_NumberOfPixelsCounted;
      }
    }

  const RealType denom = -1.0 * std::sqrt(sff * smm);

  if( this->m_NumberOfPixelsCounted > 0 && denom != 0.0 )
    {
    for( unsigned int i = 0; i < ParametersDimension; i++ )
      {
      derivative[i] = ( derivativeF[i] - ( sfm / smm ) * derivativeM[i] ) / denom;
      }
    value = sfm / denom;
    }
  else
    {
    for( unsigned int i = 0; i < ParametersDimension; i++ )
      {
      derivative[i] = NumericTraits<MeasureType>::ZeroValue();
      }
    value = NumericTraits<MeasureType>::ZeroValue();
    }
}

template <typename TFixedImage, typename TMovingImage>
void
NormalizedCorrelationPointSetToImageMetric<TFixedImage, TMovingImage>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "SubtractMean: " << m_SubtractMean << std::endl;
}

} // end namespace itk

#endif
