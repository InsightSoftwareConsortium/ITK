/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNormalizedCorrelationPointSetToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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

  m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters( parameters );

  typedef  typename NumericTraits< MeasureType >::AccumulateType AccumulateType;

  AccumulateType sff = NumericTraits< AccumulateType >::Zero;
  AccumulateType smm = NumericTraits< AccumulateType >::Zero;
  AccumulateType sfm = NumericTraits< AccumulateType >::Zero;

  while( pointItr != pointEnd && pointDataItr != pointDataEnd )
    {
    typename Superclass::InputPointType  inputPoint;
    inputPoint.CastFrom( pointItr.Value() );
    typename Superclass::OutputPointType transformedPoint = 
                           m_Transform->TransformPoint( inputPoint );

    if( m_Interpolator->IsInsideBuffer( transformedPoint ) )
      {
      const RealType movingValue  = m_Interpolator->Evaluate( transformedPoint );
      const RealType fixedValue   = pointDataItr.Value();
      sff += fixedValue  * fixedValue;
      smm += movingValue * movingValue;
      sfm += fixedValue  * movingValue;
      m_NumberOfPixelsCounted++;
      }

    ++pointItr;
    ++pointDataItr;
    }

  if( m_NumberOfPixelsCounted > 0 && (sff*smm) != 0.0)
    {
    const RealType factor = -1.0 / sqrt( sff * smm );
    measure = sfm * factor;
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

  if( !m_GradientImage )
    {
    itkExceptionMacro(<<"The gradient image is null, maybe you forgot to call Initialize()");
    }

  FixedPointSetConstPointer fixedPointSet = this->GetFixedPointSet();

  if( !fixedPointSet ) 
    {
    itkExceptionMacro( << "Fixed image has not been assigned" );
    }

  const unsigned int dimension = FixedPointSetDimension;

  m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters( parameters );

  typedef  typename NumericTraits< MeasureType >::AccumulateType AccumulateType;

  AccumulateType sff  = NumericTraits< AccumulateType >::Zero;
  AccumulateType smm  = NumericTraits< AccumulateType >::Zero;
  AccumulateType sfm  = NumericTraits< AccumulateType >::Zero;

  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType( ParametersDimension );
  derivative.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  DerivativeType derivativeF = DerivativeType( ParametersDimension );
  derivativeF.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  DerivativeType derivativeM = DerivativeType( ParametersDimension );
  derivativeM.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  PointIterator pointItr = fixedPointSet->GetPoints()->Begin();
  PointIterator pointEnd = fixedPointSet->GetPoints()->End();

  PointDataIterator pointDataItr = fixedPointSet->GetPointData()->Begin();
  PointDataIterator pointDataEnd = fixedPointSet->GetPointData()->End();

  while( pointItr != pointEnd && pointDataItr != pointDataEnd )
    {
    typename Superclass::InputPointType  inputPoint;
    inputPoint.CastFrom( pointItr.Value() );
    typename Superclass::OutputPointType transformedPoint = 
                           m_Transform->TransformPoint( inputPoint );

    if( m_Interpolator->IsInsideBuffer( transformedPoint ) )
      {
      const RealType movingValue  = m_Interpolator->Evaluate( transformedPoint );
      const RealType fixedValue   = pointDataItr.Value();


      // First compute the sums
      sff += fixedValue  * fixedValue;
      smm += movingValue * movingValue;
      sfm += fixedValue  * movingValue;
      m_NumberOfPixelsCounted++;


      // Now compute the derivatives
      const TransformJacobianType & jacobian =
                          m_Transform->GetJacobian( inputPoint ); 

      // Get the gradient by NearestNeighboorInterpolation: 
      // which is equivalent to round up the point components.
      typedef typename Superclass::OutputPointType OutputPointType;
      typedef typename OutputPointType::CoordRepType CoordRepType;
      typedef ContinuousIndex<CoordRepType,MovingImageType::ImageDimension>
        MovingImageContinuousIndexType;

      MovingImageContinuousIndexType tempIndex;
      m_MovingImage->TransformPhysicalPointToContinuousIndex( transformedPoint, tempIndex );

      typename MovingImageType::IndexType mappedIndex; 
      for( unsigned int j = 0; j < MovingImageType::ImageDimension; j++ )
        {
        mappedIndex[j] = static_cast<long>( vnl_math_rnd( tempIndex[j] ) );
        }

      const GradientPixelType gradient = 
                                m_GradientImage->GetPixel( mappedIndex );

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
        }
      }

    ++pointItr;
    ++pointDataItr;
    }

  if( m_NumberOfPixelsCounted > 0 && (sff*smm) != 0.0)
    {
    const RealType factor = -1.0 / sqrt( sff * smm );
    for(unsigned int i=0; i<ParametersDimension; i++)
      {
      derivative[i] = factor * ( derivativeF[i] - (sfm/smm)*derivativeM[i]);
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

  if( !m_GradientImage )
    {
    itkExceptionMacro(<<"The gradient image is null, maybe you forgot to call Initialize()");
    }

  FixedPointSetConstPointer fixedPointSet = this->GetFixedPointSet();

  if( !fixedPointSet ) 
    {
    itkExceptionMacro( << "Fixed image has not been assigned" );
    }

  const unsigned int dimension = FixedPointSetDimension;

  m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters( parameters );

  typedef  typename NumericTraits< MeasureType >::AccumulateType AccumulateType;

  AccumulateType sff  = NumericTraits< AccumulateType >::Zero;
  AccumulateType smm  = NumericTraits< AccumulateType >::Zero;
  AccumulateType sfm  = NumericTraits< AccumulateType >::Zero;

  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType( ParametersDimension );
  derivative.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  DerivativeType derivativeF = DerivativeType( ParametersDimension );
  derivativeF.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  DerivativeType derivativeM = DerivativeType( ParametersDimension );
  derivativeM.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  PointIterator pointItr = fixedPointSet->GetPoints()->Begin();
  PointIterator pointEnd = fixedPointSet->GetPoints()->End();

  PointDataIterator pointDataItr = fixedPointSet->GetPointData()->Begin();
  PointDataIterator pointDataEnd = fixedPointSet->GetPointData()->End();

  while( pointItr != pointEnd && pointDataItr != pointDataEnd )
    {
    typename Superclass::InputPointType  inputPoint;
    inputPoint.CastFrom( pointItr.Value() );
    typename Superclass::OutputPointType transformedPoint = 
                           m_Transform->TransformPoint( inputPoint );

    if( m_Interpolator->IsInsideBuffer( transformedPoint ) )
      {
      const RealType movingValue  = m_Interpolator->Evaluate( transformedPoint );
      const RealType fixedValue   = pointDataItr.Value();


      // First compute the sums
      sff += fixedValue  * fixedValue;
      smm += movingValue * movingValue;
      sfm += fixedValue  * movingValue;
      m_NumberOfPixelsCounted++;


      // Now compute the derivatives
      const TransformJacobianType & jacobian =
                          m_Transform->GetJacobian( inputPoint ); 

      // Get the gradient by NearestNeighboorInterpolation: 
      // which is equivalent to round up the point components.
      typedef typename Superclass::OutputPointType OutputPointType;
      typedef typename OutputPointType::CoordRepType CoordRepType;
      typedef ContinuousIndex<CoordRepType,MovingImageType::ImageDimension>
        MovingImageContinuousIndexType;

      MovingImageContinuousIndexType tempIndex;
      m_MovingImage->TransformPhysicalPointToContinuousIndex( transformedPoint, tempIndex );

      typename MovingImageType::IndexType mappedIndex; 
      for( unsigned int j = 0; j < MovingImageType::ImageDimension; j++ )
        {
        mappedIndex[j] = static_cast<long>( vnl_math_rnd( tempIndex[j] ) );
        }

      const GradientPixelType gradient = 
                                m_GradientImage->GetPixel( mappedIndex );

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
        }
      }

    ++pointItr;
    ++pointDataItr;
    }

  if( m_NumberOfPixelsCounted > 0 && (sff*smm) != 0.0)
    {
    const RealType factor = -1.0 / sqrt( sff * smm );
    for(unsigned int i=0; i<ParametersDimension; i++)
      {
      derivative[i] = factor * ( derivativeF[i] - (sfm/smm)*derivativeM[i]);
      value = sfm * factor;
      }
    }
  else
    {
    for(unsigned int i=0; i<ParametersDimension; i++)
      {
      derivative[i] = NumericTraits< MeasureType >::Zero;
      value = NumericTraits< MeasureType >::Zero;
      }
    }


}

} // end namespace itk


#endif
