/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNormalizedCorrelationImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkNormalizedCorrelationImageToImageMetric_txx
#define _itkNormalizedCorrelationImageToImageMetric_txx

#include "itkNormalizedCorrelationImageToImageMetric.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

/*
 * Constructor
 */
template <class TFixedImage, class TMovingImage> 
NormalizedCorrelationImageToImageMetric<TFixedImage,TMovingImage>
::NormalizedCorrelationImageToImageMetric()
{
}

/*
 * Get the match Measure
 */
template <class TFixedImage, class TMovingImage> 
typename NormalizedCorrelationImageToImageMetric<TFixedImage,TMovingImage>::MeasureType
NormalizedCorrelationImageToImageMetric<TFixedImage,TMovingImage>
::GetValue( const TransformParametersType & parameters ) const
{

  FixedImageConstPointer fixedImage = this->GetFixedImage();

  if( !fixedImage ) 
    {
    itkExceptionMacro( << "Fixed image has not been assigned" );
    }

  typedef  itk::ImageRegionConstIteratorWithIndex<FixedImageType> FixedIteratorType;

  FixedIteratorType ti( fixedImage, this->GetFixedImageRegion() );

  typename FixedImageType::IndexType index;

  MeasureType measure;

  m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters( parameters );

  typedef  typename NumericTraits< MeasureType >::AccumulateType AccumulateType;

  AccumulateType sff = NumericTraits< AccumulateType >::Zero;
  AccumulateType smm = NumericTraits< AccumulateType >::Zero;
  AccumulateType sfm = NumericTraits< AccumulateType >::Zero;

  while(!ti.IsAtEnd())
    {

    index = ti.GetIndex();
    
    typename Superclass::InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint( index, inputPoint );

    typename Superclass::OutputPointType transformedPoint = m_Transform->TransformPoint( inputPoint );

    if( m_Interpolator->IsInsideBuffer( transformedPoint ) )
      {
      const RealType movingValue  = m_Interpolator->Evaluate( transformedPoint );
      const RealType fixedValue   = ti.Get();
      sff += fixedValue  * fixedValue;
      smm += movingValue * movingValue;
      sfm += fixedValue  * movingValue;
      m_NumberOfPixelsCounted++;
      }

    ++ti;
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
template < class TFixedImage, class TMovingImage> 
void
NormalizedCorrelationImageToImageMetric<TFixedImage,TMovingImage>
::GetDerivative( const TransformParametersType & parameters,
                       DerivativeType & derivative ) const
{

  if( !m_GradientImage )
    {
    itkExceptionMacro(<<"The gradient image is null, maybe you forgot to call Initialize()");
    }

  FixedImageConstPointer fixedImage = this->GetFixedImage();

  if( !fixedImage ) 
    {
    itkExceptionMacro( << "Fixed image has not been assigned" );
    }

  const unsigned int dimension = FixedImageType::ImageDimension;

  typedef  itk::ImageRegionConstIteratorWithIndex<FixedImageType> FixedIteratorType;

  typedef  itk::ImageRegionConstIteratorWithIndex<
                  ITK_TYPENAME Superclass::GradientImageType> GradientIteratorType;


  FixedIteratorType ti( fixedImage, this->GetFixedImageRegion() );

  GradientIteratorType gi( m_GradientImage, this->GetFixedImageRegion() );

  typename FixedImageType::IndexType index;

  m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters( parameters );

  typedef  typename NumericTraits< MeasureType >::AccumulateType AccumulateType;

  AccumulateType sff  = NumericTraits< AccumulateType >::Zero;
  AccumulateType smm  = NumericTraits< AccumulateType >::Zero;
  AccumulateType sfm = NumericTraits< AccumulateType >::Zero;

  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType( ParametersDimension );
  derivative.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  DerivativeType derivativeF = DerivativeType( ParametersDimension );
  derivativeF.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  DerivativeType derivativeM = DerivativeType( ParametersDimension );
  derivativeM.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  typename MovingImageType::TransformPointer movingImageTransform = 
                                 m_MovingImage->GetPhysicalToIndexTransform();

  ti.GoToBegin();
  gi.GoToBegin();

  // First compute the sums
  while(!ti.IsAtEnd())
    {

    index = ti.GetIndex();
    
    typename Superclass::InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint( index, inputPoint );

    typename Superclass::OutputPointType transformedPoint = m_Transform->TransformPoint( inputPoint );

    if( m_Interpolator->IsInsideBuffer( transformedPoint ) )
      {
      const RealType movingValue  = m_Interpolator->Evaluate( transformedPoint );
      const RealType fixedValue   = ti.Get();
      sff += fixedValue  * fixedValue;
      smm += movingValue * movingValue;
      sfm += fixedValue  * movingValue;
      m_NumberOfPixelsCounted++;
      }

    ++ti;
    }

  // Compute contributions to derivatives
  ti.GoToBegin();
  while(!ti.IsAtEnd())
    {

    index = ti.GetIndex();
    
    typename Superclass::InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint( index, inputPoint );

    typename Superclass::OutputPointType transformedPoint = m_Transform->TransformPoint( inputPoint );

    if( m_Interpolator->IsInsideBuffer( transformedPoint ) )
      {
      const RealType movingValue  = m_Interpolator->Evaluate( transformedPoint );
      const RealType fixedValue     = ti.Get();

      const TransformJacobianType & jacobian =
                          m_Transform->GetJacobian( inputPoint ); 

      // Get the gradient by NearestNeighboorInterpolation: 
      // which is equivalent to round up the point components.
      typename Superclass::OutputPointType tempPoint = 
               movingImageTransform->TransformPoint( transformedPoint );

      typename MovingImageType::IndexType mappedIndex; 
      for( unsigned int j = 0; j < MovingImageType::ImageDimension; j++ )
        {
        mappedIndex[j] = static_cast<long>( vnl_math_rnd( tempPoint[j] ) );
        }

      const GradientPixelType gradient = 
                                m_GradientImage->GetPixel( mappedIndex );

      for(unsigned int par=0; par<ParametersDimension; par++)
        {
        RealType sumF = NumericTraits< RealType >::Zero;
        RealType sumM = NumericTraits< RealType >::Zero;
        for(unsigned int dim=0; dim<dimension; dim++)
          {
          const RealType differential = jacobian( dim, par ) * gradient[dim];
          sumF += fixedValue  * differential;
          sumM += movingValue * differential;
          }
        derivativeF[par] += sumF;
        derivativeM[par] += sumM;
        }
      }

    ++ti;
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
template <class TFixedImage, class TMovingImage> 
void
NormalizedCorrelationImageToImageMetric<TFixedImage,TMovingImage>
::GetValueAndDerivative(const TransformParametersType & parameters, 
                        MeasureType & value, DerivativeType  & derivative) const
{


  if( !m_GradientImage )
    {
    itkExceptionMacro(<<"The gradient image is null, maybe you forgot to call Initialize()");
    }

  FixedImageConstPointer fixedImage = this->GetFixedImage();

  if( !fixedImage ) 
    {
    itkExceptionMacro( << "Fixed image has not been assigned" );
    }

  const unsigned int dimension = FixedImageType::ImageDimension;

  typedef  itk::ImageRegionConstIteratorWithIndex<FixedImageType> FixedIteratorType;

  typedef  itk::ImageRegionConstIteratorWithIndex<
                  ITK_TYPENAME Superclass::GradientImageType> GradientIteratorType;

  FixedIteratorType ti( fixedImage, this->GetFixedImageRegion() );

  GradientIteratorType gi( m_GradientImage, this->GetFixedImageRegion() );

  typename FixedImageType::IndexType index;

  m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters( parameters );

  typedef  typename NumericTraits< MeasureType >::AccumulateType AccumulateType;

  AccumulateType sff  = NumericTraits< AccumulateType >::Zero;
  AccumulateType smm  = NumericTraits< AccumulateType >::Zero;
  AccumulateType sfm = NumericTraits< AccumulateType >::Zero;

  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType( ParametersDimension );
  derivative.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  DerivativeType derivativeF = DerivativeType( ParametersDimension );
  derivativeF.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  DerivativeType derivativeM = DerivativeType( ParametersDimension );
  derivativeM.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );


  typename MovingImageType::TransformPointer movingImageTransform = 
                                 m_MovingImage->GetPhysicalToIndexTransform();

  ti.GoToBegin();
  gi.GoToBegin();

  // First compute the sums
  while(!ti.IsAtEnd())
    {

    index = ti.GetIndex();
    
    typename Superclass::InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint( index, inputPoint );

    typename Superclass::OutputPointType transformedPoint = m_Transform->TransformPoint( inputPoint );

    if( m_Interpolator->IsInsideBuffer( transformedPoint ) )
      {
      const RealType movingValue  = m_Interpolator->Evaluate( transformedPoint );
      const RealType fixedValue   = ti.Get();
      sff += fixedValue  * fixedValue;
      smm += movingValue * movingValue;
      sfm += fixedValue  * movingValue;
      m_NumberOfPixelsCounted++;
      }

    ++ti;
    }


  // Compute contributions to derivatives
  ti.GoToBegin();
  while(!ti.IsAtEnd())
    {

    index = ti.GetIndex();
    
    typename Superclass::InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint( index, inputPoint );

    typename Superclass::OutputPointType transformedPoint = m_Transform->TransformPoint( inputPoint );

    if( m_Interpolator->IsInsideBuffer( transformedPoint ) )
      {
      const RealType movingValue  = m_Interpolator->Evaluate( transformedPoint );
      const RealType fixedValue     = ti.Get();

      const TransformJacobianType & jacobian =
                          m_Transform->GetJacobian( inputPoint ); 

      // Get the gradient by NearestNeighboorInterpolation: 
      // which is equivalent to round up the point components.
      typename Superclass::OutputPointType tempPoint = 
               movingImageTransform->TransformPoint( transformedPoint );

      typename MovingImageType::IndexType mappedIndex; 
      for( unsigned int j = 0; j < MovingImageType::ImageDimension; j++ )
        {
        mappedIndex[j] = static_cast<long>( vnl_math_rnd( tempPoint[j] ) );
        }

      const GradientPixelType gradient = 
                                m_GradientImage->GetPixel( mappedIndex );

      for(unsigned int par=0; par<ParametersDimension; par++)
        {
        RealType sumF = NumericTraits< RealType >::Zero;
        RealType sumM = NumericTraits< RealType >::Zero;
        for(unsigned int dim=0; dim<dimension; dim++)
          {
          const RealType differential = jacobian( dim, par ) * gradient[dim];
          sumF += fixedValue  * differential;
          sumM += movingValue * differential;
          }
        derivativeF[par] += sumF;
        derivativeM[par] += sumM;
        }
      }
    ++ti;
    }
  if( m_NumberOfPixelsCounted > 0 && (sff*smm) != 0.0)
    {
    const RealType factor = -1.0 / sqrt( sff * smm );
    for(unsigned int i=0; i<ParametersDimension; i++)
      {
      derivative[i] = factor * ( derivativeF[i] - (sfm/smm)*derivativeM[i]);
      }
    value = sfm * factor;
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

} // end namespace itk


#endif
