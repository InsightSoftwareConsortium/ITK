/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanSquaresImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMeanSquaresImageToImageMetric_txx
#define __itkMeanSquaresImageToImageMetric_txx

// First make sure that the configuration is available.
// This line can be removed once the optimized versions
// gets integrated into the main directories.
#include "itkConfigure.h"

#ifdef ITK_USE_OPTIMIZED_REGISTRATION_METHODS
#include "itkOptMeanSquaresImageToImageMetric.txx"
#else

#include "itkMeanSquaresImageToImageMetric.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

/**
 * Constructor
 */
template <class TFixedImage, class TMovingImage> 
MeanSquaresImageToImageMetric<TFixedImage,TMovingImage>
::MeanSquaresImageToImageMetric()
{
  itkDebugMacro("Constructor");
}

/**
 * Get the match Measure
 */
template <class TFixedImage, class TMovingImage> 
typename MeanSquaresImageToImageMetric<TFixedImage,TMovingImage>::MeasureType
MeanSquaresImageToImageMetric<TFixedImage,TMovingImage>
::GetValue( const TransformParametersType & parameters ) const
{

  itkDebugMacro("GetValue( " << parameters << " ) ");

  FixedImageConstPointer fixedImage = this->m_FixedImage;

  if( !fixedImage ) 
    {
    itkExceptionMacro( << "Fixed image has not been assigned" );
    }

  typedef  itk::ImageRegionConstIteratorWithIndex<FixedImageType> FixedIteratorType;


  FixedIteratorType ti( fixedImage, this->GetFixedImageRegion() );

  typename FixedImageType::IndexType index;

  MeasureType measure = NumericTraits< MeasureType >::Zero;

  this->m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters( parameters );

  while(!ti.IsAtEnd())
    {

    index = ti.GetIndex();
    
    InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint( index, inputPoint );

    if( this->m_FixedImageMask && !this->m_FixedImageMask->IsInside( inputPoint ) )
      {
      ++ti;
      continue;
      }

    OutputPointType transformedPoint = this->m_Transform->TransformPoint( inputPoint );

    if( this->m_MovingImageMask && !this->m_MovingImageMask->IsInside( transformedPoint ) )
      {
      ++ti;
      continue;
      }

    if( this->m_Interpolator->IsInsideBuffer( transformedPoint ) )
      {
      const RealType movingValue  = this->m_Interpolator->Evaluate( transformedPoint );
      const RealType fixedValue   = ti.Get();
      this->m_NumberOfPixelsCounted++;
      const RealType diff = movingValue - fixedValue; 
      measure += diff * diff; 
      }

    ++ti;
    }

  if( !this->m_NumberOfPixelsCounted )
    {
    itkExceptionMacro(<<"All the points mapped to outside of the moving image");
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
template < class TFixedImage, class TMovingImage> 
void
MeanSquaresImageToImageMetric<TFixedImage,TMovingImage>
::GetDerivative( const TransformParametersType & parameters,
                 DerivativeType & derivative  ) const
{

  itkDebugMacro("GetDerivative( " << parameters << " ) ");
  
  if( !this->GetGradientImage() )
    {
    itkExceptionMacro(<<"The gradient image is null, maybe you forgot to call Initialize()");
    }

  FixedImageConstPointer fixedImage = this->m_FixedImage;

  if( !fixedImage ) 
    {
    itkExceptionMacro( << "Fixed image has not been assigned" );
    }

  const unsigned int ImageDimension = FixedImageType::ImageDimension;


  typedef  itk::ImageRegionConstIteratorWithIndex<
    FixedImageType> FixedIteratorType;

  typedef  itk::ImageRegionConstIteratorWithIndex<GradientImageType> GradientIteratorType;


  FixedIteratorType ti( fixedImage, this->GetFixedImageRegion() );

  typename FixedImageType::IndexType index;

  this->m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters( parameters );

  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType( ParametersDimension );
  derivative.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  ti.GoToBegin();

  while(!ti.IsAtEnd())
    {

    index = ti.GetIndex();
    
    InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint( index, inputPoint );

    if( this->m_FixedImageMask && !this->m_FixedImageMask->IsInside( inputPoint ) )
      {
      ++ti;
      continue;
      }

    OutputPointType transformedPoint = this->m_Transform->TransformPoint( inputPoint );

    if( this->m_MovingImageMask && !this->m_MovingImageMask->IsInside( transformedPoint ) )
      {
      ++ti;
      continue;
      }

    if( this->m_Interpolator->IsInsideBuffer( transformedPoint ) )
      {
      const RealType movingValue  = this->m_Interpolator->Evaluate( transformedPoint );

      const TransformJacobianType & jacobian =
        this->m_Transform->GetJacobian( inputPoint ); 

      
      const RealType fixedValue     = ti.Value();
      this->m_NumberOfPixelsCounted++;
      const RealType diff = movingValue - fixedValue; 

      // Get the gradient by NearestNeighboorInterpolation: 
      // which is equivalent to round up the point components.
      typedef typename OutputPointType::CoordRepType CoordRepType;
      typedef ContinuousIndex<CoordRepType,MovingImageType::ImageDimension>
        MovingImageContinuousIndexType;

      MovingImageContinuousIndexType tempIndex;
      this->m_MovingImage->TransformPhysicalPointToContinuousIndex( transformedPoint, tempIndex );

      typename MovingImageType::IndexType mappedIndex; 
      mappedIndex.CopyWithRound( tempIndex );

      const GradientPixelType gradient = 
        this->GetGradientImage()->GetPixel( mappedIndex );

      for(unsigned int par=0; par<ParametersDimension; par++)
        {
        RealType sum = NumericTraits< RealType >::Zero;
        for(unsigned int dim=0; dim<ImageDimension; dim++)
          {
          sum += 2.0 * diff * jacobian( dim, par ) * gradient[dim];
          }
        derivative[par] += sum;
        }
      }

    ++ti;
    }

  if( !this->m_NumberOfPixelsCounted )
    {
    itkExceptionMacro(<<"All the points mapped to outside of the moving image");
    }
  else
    {
    for(unsigned int i=0; i<ParametersDimension; i++)
      {
      derivative[i] /= this->m_NumberOfPixelsCounted;
      }
    }

}


/*
 * Get both the match Measure and theDerivative Measure 
 */
template <class TFixedImage, class TMovingImage> 
void
MeanSquaresImageToImageMetric<TFixedImage,TMovingImage>
::GetValueAndDerivative(const TransformParametersType & parameters, 
                        MeasureType & value, DerivativeType  & derivative) const
{

  itkDebugMacro("GetValueAndDerivative( " << parameters << " ) ");

  if( !this->GetGradientImage() )
    {
    itkExceptionMacro(<<"The gradient image is null, maybe you forgot to call Initialize()");
    }

  FixedImageConstPointer fixedImage = this->m_FixedImage;

  if( !fixedImage ) 
    {
    itkExceptionMacro( << "Fixed image has not been assigned" );
    }

  const unsigned int ImageDimension = FixedImageType::ImageDimension;

  typedef  itk::ImageRegionConstIteratorWithIndex<
    FixedImageType> FixedIteratorType;

  typedef  itk::ImageRegionConstIteratorWithIndex<GradientImageType> GradientIteratorType;


  FixedIteratorType ti( fixedImage, this->GetFixedImageRegion() );

  typename FixedImageType::IndexType index;

  MeasureType measure = NumericTraits< MeasureType >::Zero;

  this->m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters( parameters );

  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType( ParametersDimension );
  derivative.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  ti.GoToBegin();

  while(!ti.IsAtEnd())
    {

    index = ti.GetIndex();
    
    InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint( index, inputPoint );

    if( this->m_FixedImageMask && !this->m_FixedImageMask->IsInside( inputPoint ) )
      {
      ++ti;
      continue;
      }

    OutputPointType transformedPoint = this->m_Transform->TransformPoint( inputPoint );

    if( this->m_MovingImageMask && !this->m_MovingImageMask->IsInside( transformedPoint ) )
      {
      ++ti;
      continue;
      }

    if( this->m_Interpolator->IsInsideBuffer( transformedPoint ) )
      {
      const RealType movingValue  = this->m_Interpolator->Evaluate( transformedPoint );

      const TransformJacobianType & jacobian =
        this->m_Transform->GetJacobian( inputPoint ); 

      
      const RealType fixedValue     = ti.Value();
      this->m_NumberOfPixelsCounted++;

      const RealType diff = movingValue - fixedValue; 
  
      measure += diff * diff;

      // Get the gradient by NearestNeighboorInterpolation: 
      // which is equivalent to round up the point components.
      typedef typename OutputPointType::CoordRepType CoordRepType;
      typedef ContinuousIndex<CoordRepType,MovingImageType::ImageDimension>
        MovingImageContinuousIndexType;

      MovingImageContinuousIndexType tempIndex;
      this->m_MovingImage->TransformPhysicalPointToContinuousIndex( transformedPoint, tempIndex );

      typename MovingImageType::IndexType mappedIndex; 
      mappedIndex.CopyWithRound( tempIndex );

      const GradientPixelType gradient = 
        this->GetGradientImage()->GetPixel( mappedIndex );

      for(unsigned int par=0; par<ParametersDimension; par++)
        {
        RealType sum = NumericTraits< RealType >::Zero;
        for(unsigned int dim=0; dim<ImageDimension; dim++)
          {
          sum += 2.0 * diff * jacobian( dim, par ) * gradient[dim];
          }
        derivative[par] += sum;
        }
      }

    ++ti;
    }

  if( !this->m_NumberOfPixelsCounted )
    {
    itkExceptionMacro(<<"All the points mapped to outside of the moving image");
    }
  else
    {
    for(unsigned int i=0; i<ParametersDimension; i++)
      {
      derivative[i] /= this->m_NumberOfPixelsCounted;
      }
    measure /= this->m_NumberOfPixelsCounted;
    }

  value = measure;

}

} // end namespace itk


#endif

#endif
