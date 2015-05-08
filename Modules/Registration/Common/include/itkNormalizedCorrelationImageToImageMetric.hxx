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
#ifndef itkNormalizedCorrelationImageToImageMetric_hxx
#define itkNormalizedCorrelationImageToImageMetric_hxx

#include "itkNormalizedCorrelationImageToImageMetric.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{
/**
 * Constructor
 */
template <typename TFixedImage, typename TMovingImage>
NormalizedCorrelationImageToImageMetric<TFixedImage, TMovingImage>
::NormalizedCorrelationImageToImageMetric()
{
  m_SubtractMean = false;
}

/**
 * Get the match Measure
 */
template <typename TFixedImage, typename TMovingImage>
typename NormalizedCorrelationImageToImageMetric<TFixedImage, TMovingImage>::MeasureType
NormalizedCorrelationImageToImageMetric<TFixedImage, TMovingImage>
::GetValue(const TransformParametersType & parameters) const
{
  FixedImageConstPointer fixedImage = this->m_FixedImage;

  if( !fixedImage )
    {
    itkExceptionMacro(<< "Fixed image has not been assigned");
    }

  typedef  itk::ImageRegionConstIteratorWithIndex<FixedImageType> FixedIteratorType;

  FixedIteratorType ti( fixedImage, this->GetFixedImageRegion() );

  typename FixedImageType::IndexType index;

  MeasureType measure;

  this->m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters(parameters);

  typedef  typename NumericTraits<MeasureType>::AccumulateType AccumulateType;

  AccumulateType sff = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType smm = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType sfm = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType sf  = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType sm  = NumericTraits<AccumulateType>::ZeroValue();

  while( !ti.IsAtEnd() )
    {
    index = ti.GetIndex();

    InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint(index, inputPoint);

    if( this->m_FixedImageMask && !this->m_FixedImageMask->IsInside(inputPoint) )
      {
      ++ti;
      continue;
      }

    OutputPointType transformedPoint = this->m_Transform->TransformPoint(inputPoint);

    if( this->m_MovingImageMask && !this->m_MovingImageMask->IsInside(transformedPoint) )
      {
      ++ti;
      continue;
      }

    if( this->m_Interpolator->IsInsideBuffer(transformedPoint) )
      {
      const RealType movingValue  = this->m_Interpolator->Evaluate(transformedPoint);
      const RealType fixedValue   = ti.Get();
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

    ++ti;
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
template <typename TFixedImage, typename TMovingImage>
void
NormalizedCorrelationImageToImageMetric<TFixedImage, TMovingImage>
::GetDerivative(const TransformParametersType & parameters,
                DerivativeType & derivative) const
{
  if( !this->GetGradientImage() )
    {
    itkExceptionMacro(<< "The gradient image is null, maybe you forgot to call Initialize()");
    }

  FixedImageConstPointer fixedImage = this->m_FixedImage;

  if( !fixedImage )
    {
    itkExceptionMacro(<< "Fixed image has not been assigned");
    }

  const unsigned int dimension = FixedImageType::ImageDimension;

  typedef  itk::ImageRegionConstIteratorWithIndex<FixedImageType> FixedIteratorType;

  FixedIteratorType ti( fixedImage, this->GetFixedImageRegion() );

  typename FixedImageType::IndexType index;

  this->m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters(parameters);

  typedef  typename NumericTraits<MeasureType>::AccumulateType AccumulateType;

  AccumulateType sff  = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType smm  = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType sfm = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType sf  = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType sm  = NumericTraits<AccumulateType>::ZeroValue();

  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType(ParametersDimension);
  derivative.Fill(NumericTraits<typename DerivativeType::ValueType>::ZeroValue());

  DerivativeType derivativeF = DerivativeType(ParametersDimension);
  derivativeF.Fill(NumericTraits<typename DerivativeType::ValueType>::ZeroValue());

  DerivativeType derivativeM = DerivativeType(ParametersDimension);
  derivativeM.Fill(NumericTraits<typename DerivativeType::ValueType>::ZeroValue());

  ti.GoToBegin();
  // First compute the sums
  while( !ti.IsAtEnd() )
    {
    index = ti.GetIndex();

    InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint(index, inputPoint);

    if( this->m_FixedImageMask && !this->m_FixedImageMask->IsInside(inputPoint) )
      {
      ++ti;
      continue;
      }

    OutputPointType transformedPoint = this->m_Transform->TransformPoint(inputPoint);

    if( this->m_MovingImageMask && !this->m_MovingImageMask->IsInside(transformedPoint) )
      {
      ++ti;
      continue;
      }

    if( this->m_Interpolator->IsInsideBuffer(transformedPoint) )
      {
      const RealType movingValue  = this->m_Interpolator->Evaluate(transformedPoint);
      const RealType fixedValue   = ti.Get();
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

    ++ti;
    }

  TransformJacobianType jacobian(TFixedImage::ImageDimension,
                                 this->m_Transform->GetNumberOfParameters());
  TransformJacobianType jacobianCache(TFixedImage::ImageDimension,TFixedImage::ImageDimension);

  // Compute contributions to derivatives
  ti.GoToBegin();
  while( !ti.IsAtEnd() )
    {
    index = ti.GetIndex();

    InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint(index, inputPoint);

    if( this->m_FixedImageMask && !this->m_FixedImageMask->IsInside(inputPoint) )
      {
      ++ti;
      continue;
      }

    OutputPointType transformedPoint = this->m_Transform->TransformPoint(inputPoint);

    if( this->m_MovingImageMask && !this->m_MovingImageMask->IsInside(transformedPoint) )
      {
      ++ti;
      continue;
      }

    if( this->m_Interpolator->IsInsideBuffer(transformedPoint) )
      {
      const RealType movingValue  = this->m_Interpolator->Evaluate(transformedPoint);
      const RealType fixedValue     = ti.Get();

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
        RealType sumF = NumericTraits<RealType>::ZeroValue();
        RealType sumM = NumericTraits<RealType>::ZeroValue();
        for( unsigned int dim = 0; dim < dimension; dim++ )
          {
          const RealType differential = jacobian(dim, par) * gradient[dim];
          sumF += fixedValue  * differential;
          sumM += movingValue * differential;
          if( this->m_SubtractMean && this->m_NumberOfPixelsCounted > 0 )
            {
            sumF -= differential * sf / this->m_NumberOfPixelsCounted;
            sumM -= differential * sm / this->m_NumberOfPixelsCounted;
            }
          }
        derivativeF[par] += sumF;
        derivativeM[par] += sumM;
        }
      }

    ++ti;
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
template <typename TFixedImage, typename TMovingImage>
void
NormalizedCorrelationImageToImageMetric<TFixedImage, TMovingImage>
::GetValueAndDerivative(const TransformParametersType & parameters,
                        MeasureType & value, DerivativeType  & derivative) const
{
  if( !this->GetGradientImage() )
    {
    itkExceptionMacro(<< "The gradient image is null, maybe you forgot to call Initialize()");
    }

  FixedImageConstPointer fixedImage = this->m_FixedImage;

  if( !fixedImage )
    {
    itkExceptionMacro(<< "Fixed image has not been assigned");
    }

  const unsigned int dimension = FixedImageType::ImageDimension;

  typedef  itk::ImageRegionConstIteratorWithIndex<FixedImageType> FixedIteratorType;

  FixedIteratorType ti( fixedImage, this->GetFixedImageRegion() );

  typename FixedImageType::IndexType index;

  this->m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters(parameters);

  typedef  typename NumericTraits<MeasureType>::AccumulateType AccumulateType;

  AccumulateType sff  = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType smm  = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType sfm = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType sf  = NumericTraits<AccumulateType>::ZeroValue();
  AccumulateType sm  = NumericTraits<AccumulateType>::ZeroValue();

  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType(ParametersDimension);
  derivative.Fill(NumericTraits<typename DerivativeType::ValueType>::ZeroValue());

  DerivativeType derivativeF = DerivativeType(ParametersDimension);
  derivativeF.Fill(NumericTraits<typename DerivativeType::ValueType>::ZeroValue());

  DerivativeType derivativeM = DerivativeType(ParametersDimension);
  derivativeM.Fill(NumericTraits<typename DerivativeType::ValueType>::ZeroValue());

  DerivativeType derivativeM1 = DerivativeType(ParametersDimension);
  derivativeM1.Fill(NumericTraits<typename DerivativeType::ValueType>::ZeroValue());

  ti.GoToBegin();
  // First compute the sums
  while( !ti.IsAtEnd() )
    {
    index = ti.GetIndex();

    InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint(index, inputPoint);

    if( this->m_FixedImageMask && !this->m_FixedImageMask->IsInside(inputPoint) )
      {
      ++ti;
      continue;
      }

    OutputPointType transformedPoint = this->m_Transform->TransformPoint(inputPoint);

    if( this->m_MovingImageMask && !this->m_MovingImageMask->IsInside(transformedPoint) )
      {
      ++ti;
      continue;
      }

    if( this->m_Interpolator->IsInsideBuffer(transformedPoint) )
      {
      const RealType movingValue  = this->m_Interpolator->Evaluate(transformedPoint);
      const RealType fixedValue   = ti.Get();
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

    ++ti;
    }

  TransformJacobianType jacobianCache(TFixedImage::ImageDimension,TFixedImage::ImageDimension);
  TransformJacobianType jacobian(TFixedImage::ImageDimension,
                                 this->m_Transform->GetNumberOfParameters());
  // Compute contributions to derivatives
  ti.GoToBegin();
  while( !ti.IsAtEnd() )
    {
    index = ti.GetIndex();

    InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint(index, inputPoint);

    if( this->m_FixedImageMask && !this->m_FixedImageMask->IsInside(inputPoint) )
      {
      ++ti;
      continue;
      }

    OutputPointType transformedPoint = this->m_Transform->TransformPoint(inputPoint);

    if( this->m_MovingImageMask && !this->m_MovingImageMask->IsInside(transformedPoint) )
      {
      ++ti;
      continue;
      }

    if( this->m_Interpolator->IsInsideBuffer(transformedPoint) )
      {
      const RealType movingValue  = this->m_Interpolator->Evaluate(transformedPoint);
      const RealType fixedValue     = ti.Get();

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
        RealType sumF = NumericTraits<RealType>::ZeroValue();
        RealType sumM = NumericTraits<RealType>::ZeroValue();
        for( unsigned int dim = 0; dim < dimension; dim++ )
          {
          const RealType differential = jacobian(dim, par) * gradient[dim];
          sumF += fixedValue  * differential;
          sumM += movingValue * differential;
          if( this->m_SubtractMean && this->m_NumberOfPixelsCounted > 0 )
            {
            sumF -= differential * sf / this->m_NumberOfPixelsCounted;
            sumM -= differential * sm / this->m_NumberOfPixelsCounted;
            }
          }
        derivativeF[par] += sumF;
        derivativeM[par] += sumM;
        }
      }
    ++ti;
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
NormalizedCorrelationImageToImageMetric<TFixedImage, TMovingImage>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "SubtractMean: " << m_SubtractMean << std::endl;
}

} // end namespace itk

#endif
