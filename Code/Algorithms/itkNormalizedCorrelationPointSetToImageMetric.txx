/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNormalizedCorrelationPointSetToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkNormalizedCorrelationPointSetToImageMetric_txx
#define _itkNormalizedCorrelationPointSetToImageMetric_txx

#include "itkNormalizedCorrelationPointSetToImageMetric.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

/*
 * Constructor
 */
template <class TFixedPointSet, class TMovingImage> 
NormalizedCorrelationPointSetToImageMetric<TFixedPointSet,TMovingImage>
::NormalizedCorrelationPointSetToImageMetric()
{
  m_SubtractMean = false;
}

/*
 * Get the match Measure
 */
template <class TFixedPointSet, class TMovingImage> 
typename NormalizedCorrelationPointSetToImageMetric<TFixedPointSet,TMovingImage>::MeasureType
NormalizedCorrelationPointSetToImageMetric<TFixedPointSet,TMovingImage>
::GetValue( const TransformParametersType & parameters ) const
{

  FixedPointSetConstPointer fixedPointSet = this->GetFixedPointSet();

  if( !fixedPointSet ) 
    {
    itkExceptionMacro( << "Fixed point set has not been assigned" );
    }

  PointIterator pointItr = fixedPointSet->GetPoints()->Begin();
  PointIterator pointEnd = fixedPointSet->GetPoints()->End();

  PointDataIterator pointDataItr = fixedPointSet->GetPointData()->Begin();
  PointDataIterator pointDataEnd = fixedPointSet->GetPointData()->End();

  MeasureType measure;

  this->m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters( parameters );

  typedef  typename NumericTraits< MeasureType >::AccumulateType AccumulateType;

  AccumulateType sff = NumericTraits< AccumulateType >::Zero;
  AccumulateType smm = NumericTraits< AccumulateType >::Zero;
  AccumulateType sfm = NumericTraits< AccumulateType >::Zero;
  AccumulateType sf  = NumericTraits< AccumulateType >::Zero;
  AccumulateType sm  = NumericTraits< AccumulateType >::Zero;

  while( pointItr != pointEnd && pointDataItr != pointDataEnd )
    {
    typename Superclass::InputPointType  inputPoint;
    inputPoint.CastFrom( pointItr.Value() );
    typename Superclass::OutputPointType transformedPoint = 
      this->m_Transform->TransformPoint( inputPoint );

    if(this->m_Interpolator->IsInsideBuffer( transformedPoint ) )
      {
      const RealType movingValue  = this->m_Interpolator->Evaluate( transformedPoint );
      const RealType fixedValue   = pointDataItr.Value();
      sff += fixedValue  * fixedValue;
      smm += movingValue * movingValue;
      sfm += fixedValue  * movingValue;
      if ( this->m_SubtractMean )
        {
        sf += fixedValue;
        sm += movingValue;
        }
      this->m_NumberOfPixelsCounted++;
      }

    ++pointItr;
    ++pointDataItr;
    }

   if ( this->m_SubtractMean && this->m_NumberOfPixelsCounted > 0 )
    {
    sff -= ( sf * sf / this->m_NumberOfPixelsCounted );
    smm -= ( sm * sm / this->m_NumberOfPixelsCounted );
    sfm -= ( sf * sm / this->m_NumberOfPixelsCounted );
    }

  const RealType denom = -1.0 * sqrt( sff * smm );

  if( this->m_NumberOfPixelsCounted > 0 && denom != 0.0)
    {
    measure = sfm / denom;
    }
  else
    {
    measure = NumericTraits< MeasureType >::Zero;
    }

  return measure;

}





/*
 * Get the Derivative Measure
 */
template < class TFixedPointSet, class TMovingImage> 
void
NormalizedCorrelationPointSetToImageMetric<TFixedPointSet,TMovingImage>
::GetDerivative( const TransformParametersType & parameters,
                 DerivativeType & derivative ) const
{

  if( !this->GetGradientImage() )
    {
    itkExceptionMacro(<<"The gradient image is null, maybe you forgot to call Initialize()");
    }

  FixedPointSetConstPointer fixedPointSet = this->GetFixedPointSet();

  if( !fixedPointSet ) 
    {
    itkExceptionMacro( << "Fixed image has not been assigned" );
    }

  const unsigned int dimension = Superclass::FixedPointSetDimension;

  this->m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters( parameters );

  typedef  typename NumericTraits< MeasureType >::AccumulateType AccumulateType;

  AccumulateType sff  = NumericTraits< AccumulateType >::Zero;
  AccumulateType smm  = NumericTraits< AccumulateType >::Zero;
  AccumulateType sfm  = NumericTraits< AccumulateType >::Zero;
  AccumulateType sf  = NumericTraits< AccumulateType >::Zero;
  AccumulateType sm  = NumericTraits< AccumulateType >::Zero;

  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType( ParametersDimension );
  derivative.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  DerivativeType derivativeF = DerivativeType( ParametersDimension );
  derivativeF.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  DerivativeType derivativeM = DerivativeType( ParametersDimension );
  derivativeM.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  DerivativeType derivativeO = DerivativeType( ParametersDimension );
  derivativeO.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  PointIterator pointItr = fixedPointSet->GetPoints()->Begin();
  PointIterator pointEnd = fixedPointSet->GetPoints()->End();

  PointDataIterator pointDataItr = fixedPointSet->GetPointData()->Begin();
  PointDataIterator pointDataEnd = fixedPointSet->GetPointData()->End();

  while( pointItr != pointEnd && pointDataItr != pointDataEnd )
    {
    typename Superclass::InputPointType  inputPoint;
    inputPoint.CastFrom( pointItr.Value() );
    typename Superclass::OutputPointType transformedPoint = 
      this->m_Transform->TransformPoint( inputPoint );

    if( this->m_Interpolator->IsInsideBuffer( transformedPoint ) )
      {
      const RealType movingValue  = this->m_Interpolator->Evaluate( transformedPoint );
      const RealType fixedValue   = pointDataItr.Value();


      // First compute the sums
      sff += fixedValue  * fixedValue;
      smm += movingValue * movingValue;
      sfm += fixedValue  * movingValue;
      if ( this->m_SubtractMean )
        {
        sf += fixedValue;
        sm += movingValue;
        }
      this->m_NumberOfPixelsCounted++;


      // Now compute the derivatives
      const TransformJacobianType & jacobian =
        this->m_Transform->GetJacobian( inputPoint ); 

      // Get the gradient by NearestNeighboorInterpolation: 
      // which is equivalent to round up the point components.
      typedef typename Superclass::OutputPointType OutputPointType;
      typedef typename OutputPointType::CoordRepType CoordRepType;
      typedef ContinuousIndex<CoordRepType,MovingImageType::ImageDimension>
        MovingImageContinuousIndexType;

      MovingImageContinuousIndexType tempIndex;
      this->GetMovingImage()->TransformPhysicalPointToContinuousIndex( transformedPoint, tempIndex );

      typename MovingImageType::IndexType mappedIndex; 
      for( unsigned int j = 0; j < MovingImageType::ImageDimension; j++ )
        {
        mappedIndex[j] = static_cast<long>( vnl_math_rnd( tempIndex[j] ) );
        }

      const GradientPixelType gradient = 
        this->GetGradientImage()->GetPixel( mappedIndex );

      for(unsigned int par=0; par<ParametersDimension; par++)
        {
        RealType sumD = NumericTraits< RealType >::Zero;
        for(unsigned int dim=0; dim<dimension; dim++)
          {
          const RealType differential = jacobian( dim, par ) * gradient[dim];
          sumD += differential;
          }
        derivativeF[par] += sumD * fixedValue;
        derivativeM[par] += sumD * movingValue;
        if ( this->m_SubtractMean )
          {
          derivativeO[par] += sumD;
          }
        }
      }

    ++pointItr;
    ++pointDataItr;
    }

  if ( this->m_SubtractMean && this->m_NumberOfPixelsCounted > 0 )
    {
    sff -= ( sf * sf / this->m_NumberOfPixelsCounted );
    smm -= ( sm * sm / this->m_NumberOfPixelsCounted );
    sfm -= ( sf * sm / this->m_NumberOfPixelsCounted );

    for( unsigned int par = 0; par<ParametersDimension; par++)
      {
      derivativeF[par] -= derivativeO[par] * sf / this->m_NumberOfPixelsCounted;
      derivativeM[par] -= derivativeO[par] * sm / this->m_NumberOfPixelsCounted;
      }
    }

  const RealType denom = -1.0 * sqrt( sff * smm );

  if( this->m_NumberOfPixelsCounted > 0 && denom != 0.0)
    {
    for(unsigned int i=0; i<ParametersDimension; i++)
      {
      derivative[i] = ( derivativeF[i] - (sfm/smm)* derivativeM[i] ) / denom;
      }
    }
  else
    {
    for(unsigned int i=0; i<ParametersDimension; i++)
      {
      derivative[i] = NumericTraits< MeasureType >::Zero;
      }
    }

}


/*
 * Get both the match Measure and theDerivative Measure 
 */
template <class TFixedPointSet, class TMovingImage> 
void
NormalizedCorrelationPointSetToImageMetric<TFixedPointSet,TMovingImage>
::GetValueAndDerivative(const TransformParametersType & parameters, 
                        MeasureType & value, DerivativeType  & derivative) const
{

  if( !this->GetGradientImage() )
    {
    itkExceptionMacro(<<"The gradient image is null, maybe you forgot to call Initialize()");
    }

  FixedPointSetConstPointer fixedPointSet = this->GetFixedPointSet();

  if( !fixedPointSet ) 
    {
    itkExceptionMacro( << "Fixed image has not been assigned" );
    }

  const unsigned int dimension = Superclass::FixedPointSetDimension;

  this->m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters( parameters );

  typedef  typename NumericTraits< MeasureType >::AccumulateType AccumulateType;

  AccumulateType sff  = NumericTraits< AccumulateType >::Zero;
  AccumulateType smm  = NumericTraits< AccumulateType >::Zero;
  AccumulateType sfm  = NumericTraits< AccumulateType >::Zero;
  AccumulateType sf  = NumericTraits< AccumulateType >::Zero;
  AccumulateType sm  = NumericTraits< AccumulateType >::Zero;

  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType( ParametersDimension );
  derivative.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  DerivativeType derivativeF = DerivativeType( ParametersDimension );
  derivativeF.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  DerivativeType derivativeM = DerivativeType( ParametersDimension );
  derivativeM.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  DerivativeType derivativeO = DerivativeType( ParametersDimension );
  derivativeO.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  PointIterator pointItr = fixedPointSet->GetPoints()->Begin();
  PointIterator pointEnd = fixedPointSet->GetPoints()->End();

  PointDataIterator pointDataItr = fixedPointSet->GetPointData()->Begin();
  PointDataIterator pointDataEnd = fixedPointSet->GetPointData()->End();

  while( pointItr != pointEnd && pointDataItr != pointDataEnd )
    {
    typename Superclass::InputPointType  inputPoint;
    inputPoint.CastFrom( pointItr.Value() );
    typename Superclass::OutputPointType transformedPoint = 
      this->m_Transform->TransformPoint( inputPoint );

    if( this->m_Interpolator->IsInsideBuffer( transformedPoint ) )
      {
      const RealType movingValue  = this->m_Interpolator->Evaluate( transformedPoint );
      const RealType fixedValue   = pointDataItr.Value();


      // First compute the sums
      sff += fixedValue  * fixedValue;
      smm += movingValue * movingValue;
      sfm += fixedValue  * movingValue;
      if ( this->m_SubtractMean )
        {
        sf += fixedValue;
        sm += movingValue;
        }
      this->m_NumberOfPixelsCounted++;


      // Now compute the derivatives
      const TransformJacobianType & jacobian =
        this->m_Transform->GetJacobian( inputPoint ); 

      // Get the gradient by NearestNeighboorInterpolation: 
      // which is equivalent to round up the point components.
      typedef typename Superclass::OutputPointType OutputPointType;
      typedef typename OutputPointType::CoordRepType CoordRepType;
      typedef ContinuousIndex<CoordRepType,MovingImageType::ImageDimension>
        MovingImageContinuousIndexType;

      MovingImageContinuousIndexType tempIndex;
      this->GetMovingImage()->TransformPhysicalPointToContinuousIndex( transformedPoint, tempIndex );

      typename MovingImageType::IndexType mappedIndex; 
      for( unsigned int j = 0; j < MovingImageType::ImageDimension; j++ )
        {
        mappedIndex[j] = static_cast<long>( vnl_math_rnd( tempIndex[j] ) );
        }

      const GradientPixelType gradient = 
        this->GetGradientImage()->GetPixel( mappedIndex );

      for(unsigned int par=0; par<ParametersDimension; par++)
        {
        RealType sumD = NumericTraits< RealType >::Zero;
        for(unsigned int dim=0; dim<dimension; dim++)
          {
          const RealType differential = jacobian( dim, par ) * gradient[dim];
          sumD += differential;
          }
        derivativeF[par] += sumD * fixedValue;
        derivativeM[par] += sumD * movingValue;
        if ( this->m_SubtractMean )
          {
          derivativeO[par] += sumD;
          }
        }
      }

    ++pointItr;
    ++pointDataItr;
    }

  if ( this->m_SubtractMean && this->m_NumberOfPixelsCounted > 0 )
    {
    sff -= ( sf * sf / this->m_NumberOfPixelsCounted );
    smm -= ( sm * sm / this->m_NumberOfPixelsCounted );
    sfm -= ( sf * sm / this->m_NumberOfPixelsCounted );

    for( unsigned int par = 0; par<ParametersDimension; par++)
      {
      derivativeF[par] -= derivativeO[par] * sf / this->m_NumberOfPixelsCounted;
      derivativeM[par] -= derivativeO[par] * sm / this->m_NumberOfPixelsCounted;
      }
    }

  const RealType denom = -1.0 * sqrt( sff * smm );

  if( this->m_NumberOfPixelsCounted > 0 && denom != 0.0)
    {
    for(unsigned int i=0; i<ParametersDimension; i++)
      {
      derivative[i] = ( derivativeF[i] - (sfm/smm)* derivativeM[i] ) / denom;
      }
    value = sfm / denom;
    }
  else
    {
    for(unsigned int i=0; i<ParametersDimension; i++)
      {
      derivative[i] = NumericTraits< MeasureType >::Zero;
      }
    value = NumericTraits< MeasureType >::Zero;
    }


}

template < class TFixedImage, class TMovingImage> 
void
NormalizedCorrelationPointSetToImageMetric<TFixedImage,TMovingImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "SubtractMean: " << m_SubtractMean << std::endl;
}


} // end namespace itk


#endif
