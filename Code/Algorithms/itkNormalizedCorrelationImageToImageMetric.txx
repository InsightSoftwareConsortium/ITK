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
NormalizedCorrelationImageToImageMetric<TFixedImage,TMovingImage>::MeasureType
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

  MeasureType measure = NumericTraits< MeasureType >::Zero;

  m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters( parameters );

  typedef  NumericTraits< MeasureType >::AccumulateType AccumulateType;

  AccumulateType sff = NumericTraits< AccumulateType >::Zero;
  AccumulateType smm = NumericTraits< AccumulateType >::Zero;
  AccumulateType sfm = NumericTraits< AccumulateType >::Zero;

  while(!ti.IsAtEnd())
    {

    index = ti.GetIndex();
    
    InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint( index, inputPoint );

    OutputPointType transformedPoint = m_Transform->TransformPoint( inputPoint );

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

  if( m_NumberOfPixelsCounted )
    {
    measure = -sfm / sqrt( sff * smm );
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
                                        GradientImageType> GradientIteratorType;


  FixedIteratorType ti( fixedImage, this->GetFixedImageRegion() );

  GradientIteratorType gi( m_GradientImage, this->GetFixedImageRegion() );

  typename FixedImageType::IndexType index;

  m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters( parameters );

  typedef  NumericTraits< MeasureType >::AccumulateType AccumulateType;

  AccumulateType sff  = NumericTraits< AccumulateType >::Zero;
  AccumulateType smm  = NumericTraits< AccumulateType >::Zero;

  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType( ParametersDimension );
  derivative.Fill( NumericTraits<typename DerivativeType::ValueType>::Zero );

  ti.GoToBegin();
  gi.GoToBegin();

  while(!ti.IsAtEnd())
    {

    index = ti.GetIndex();
    
    InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint( index, inputPoint );

    OutputPointType transformedPoint = m_Transform->TransformPoint( inputPoint );

    if( m_Interpolator->IsInsideBuffer( transformedPoint ) )
      {
      const RealType movingValue  = m_Interpolator->Evaluate( transformedPoint );
      const RealType fixedValue     = ti.Get();
      sff += fixedValue  * fixedValue;
      smm += movingValue * movingValue;

      const TransformJacobianType & jacobian =
                          m_Transform->GetJacobian( inputPoint ); 

      const GradientPixelType & gradient = gi.Value(); 
      for(unsigned int par=0; par<ParametersDimension; par++)
        {
        RealType sum = NumericTraits< RealType >::Zero;
        for(unsigned int dim=0; dim<dimension; dim++)
          {
          sum += movingValue * jacobian( dim, par ) * gradient[dim];
          }
        derivative[par] += sum;
        }
      m_NumberOfPixelsCounted++;
      }

    ++ti;
    }

  if( m_NumberOfPixelsCounted )
    {
    const RealType factor = -1.0 / sqrt( sff * smm );
    for(unsigned int i=0; i<ParametersDimension; i++)
      {
      derivative[i] /= factor;
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
                                        GradientImageType> GradientIteratorType;

  FixedIteratorType ti( fixedImage, this->GetFixedImageRegion() );

  GradientIteratorType gi( m_GradientImage, this->GetFixedImageRegion() );

  typename FixedImageType::IndexType index;

  m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters( parameters );

  typedef  NumericTraits< MeasureType >::AccumulateType AccumulateType;

  AccumulateType sff  = NumericTraits< AccumulateType >::Zero;
  AccumulateType smm  = NumericTraits< AccumulateType >::Zero;
  AccumulateType sfm = NumericTraits< AccumulateType >::Zero;

  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType( ParametersDimension );
  derivative.Fill( NumericTraits<typename DerivativeType::ValueType>::Zero );

  ti.GoToBegin();
  gi.GoToBegin();

  while(!ti.IsAtEnd())
    {

    index = ti.GetIndex();
    
    InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint( index, inputPoint );

    OutputPointType transformedPoint = m_Transform->TransformPoint( inputPoint );

    if( m_Interpolator->IsInsideBuffer( transformedPoint ) )
      {
      const RealType movingValue  = m_Interpolator->Evaluate( transformedPoint );
      const RealType fixedValue     = ti.Get();
      sff += fixedValue  * fixedValue;
      smm += movingValue * movingValue;
      sfm += fixedValue  * movingValue;

      const TransformJacobianType & jacobian =
                          m_Transform->GetJacobian( inputPoint ); 

      const GradientPixelType & gradient = gi.Value(); 
std::cout << " jacobian " << jacobian << std::endl;
std::cout << " gradient " << gradient << std::endl;
std::cout << " movingValue " << movingValue << std::endl;
std::cout << " dimension " << dimension << std::endl;
      for(unsigned int par=0; par<ParametersDimension; par++)
        {
        RealType sum = NumericTraits< RealType >::Zero;
        for(unsigned int dim=0; dim<dimension; dim++)
          {
          sum += movingValue * jacobian( dim, par ) * gradient[dim];
          }
        derivative[par] += sum;
std::cout <<  "derivative[ " << par << " ]" << derivative << " += sum " << sum << std::endl;
        }
      m_NumberOfPixelsCounted++;
      }

    ++ti;
    }
std::cout << "m_NumberOfPixelsCounted = " << m_NumberOfPixelsCounted << std::endl;
std::cout << "derivative              = " << derivative << std::endl;
  if( m_NumberOfPixelsCounted )
    {
    const RealType factor = -1.0 / sqrt( sff * smm );
    for(unsigned int i=0; i<ParametersDimension; i++)
      {
      derivative[i] /= factor;
      }
    value = -sfm / sqrt( sff * smm );
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
