/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKappaStatisticImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkKappaStatisticImageToImageMetric_txx
#define _itkKappaStatisticImageToImageMetric_txx

#include "itkKappaStatisticImageToImageMetric.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageFileWriter.h" //debug
#include "itkImage.h" //debug
namespace itk
{

/*
 * Constructor
 */
template <class TFixedImage, class TMovingImage> 
KappaStatisticImageToImageMetric<TFixedImage,TMovingImage>
::KappaStatisticImageToImageMetric()
{
  itkDebugMacro("Constructor");

  this->SetComputeGradient( true ); 
  m_ForegroundValue = 255;
  m_Complement = false;
}


/*
 * Get the match Measure 
 */
template <class TFixedImage, class TMovingImage> 
typename KappaStatisticImageToImageMetric<TFixedImage,TMovingImage>::MeasureType
KappaStatisticImageToImageMetric<TFixedImage,TMovingImage>
::GetValue( const TransformParametersType & parameters ) const
{
  itkDebugMacro("GetValue( " << parameters << " ) ");

  this->SetTransformParameters( parameters );

  //Get the fixed image
  //
  //
  FixedImageConstPointer fixedImage = this->m_FixedImage;
  if( !fixedImage ) 
    {
    itkExceptionMacro( << "Fixed image has not been assigned" );
    }

  //Get an iterator over the fixed image
  //
  //
  typedef  itk::ImageRegionConstIteratorWithIndex<FixedImageType> FixedIteratorType;
  typename FixedImageType::IndexType fixedIndex;
  FixedIteratorType fi( fixedImage, fixedImage->GetBufferedRegion() );

  //Get the moving image
  //
  //
  MovingImageConstPointer movingImage = this->m_MovingImage;
  if( !movingImage ) 
    {
    itkExceptionMacro( << "Moving image has not been assigned" );
    }
  
  //Following are used in the metric computation.  'measure' is the
  //value of the metric.  'fixedForegroundArea' is the total area
  //of the foreground region in the fixed image.
  //'movingForegroundArea' is the foreground area in the moving image
  //in the area of overlap under the current transformation.
  //'intersection' is the area of foreground intersection between the
  //fixed and moving image.
  //
  //
  MeasureType measure              = NumericTraits< MeasureType >::Zero;  
  MeasureType intersection         = NumericTraits< MeasureType >::Zero;  
  MeasureType movingForegroundArea = NumericTraits< MeasureType >::Zero;  
  MeasureType fixedForegroundArea  = NumericTraits< MeasureType >::Zero;  

  //Compute fixedForegroundArea, movingForegroundArea, and
  //intersection.  Loop over the fixed image.
  //
  //
  while(!fi.IsAtEnd())
    {
    fixedIndex = fi.GetIndex();
    
    typename Superclass::InputPointType fixedInputPoint;
    fixedImage->TransformIndexToPhysicalPoint( fixedIndex, fixedInputPoint );

    if( this->m_FixedImageMask && !this->m_FixedImageMask->IsInside( fixedInputPoint ) )
      {
      ++fi;
      continue;
      }

    const RealType fixedValue = fi.Get();

    //Increment 'fixedForegroundArea' 
    //
    //
    if (fixedValue==m_ForegroundValue)
      {
      fixedForegroundArea++;
      }

    //Get the point in the transformed moving image corresponding to
    //the point in the fixed image (physical coordinates)
    //
    //
    typename Superclass::OutputPointType
      transformedPoint = this->m_Transform->TransformPoint( fixedInputPoint );

    if( this->m_MovingImageMask && !this->m_MovingImageMask->IsInside( transformedPoint ) )
      {
      ++fi;
      continue;
      }

    //Compute movingForegroundArea and intersection
    //
    //
    if( this->m_Interpolator->IsInsideBuffer( transformedPoint ) )
      {    
      const RealType movingValue = this->m_Interpolator->Evaluate( transformedPoint );
      if (movingValue==m_ForegroundValue)
        {        
        movingForegroundArea++;
        }
      if ((movingValue==m_ForegroundValue)&&(fixedValue==m_ForegroundValue))
        {        
        intersection++;
        }
      }    
    ++fi;
    }

  //Compute the final metric value
  //
  //
  if (!m_Complement)
    { 
    measure = 2.0*(intersection)/(fixedForegroundArea+movingForegroundArea);
    }
  else
    {
    measure = 1.0-2.0*(intersection)/(fixedForegroundArea+movingForegroundArea);
    }

  return measure;
}


/*
 * Get the Derivative Measure
 */
template < class TFixedImage, class TMovingImage> 
void
KappaStatisticImageToImageMetric<TFixedImage,TMovingImage>
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

  typedef  itk::ImageRegionConstIteratorWithIndex<
    ITK_TYPENAME Superclass::GradientImageType> GradientIteratorType;

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
    
    typename Superclass::InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint( index, inputPoint );

    if( this->m_FixedImageMask && !this->m_FixedImageMask->IsInside( inputPoint ) )
      {
      ++ti;
      continue;
      }

    typename Superclass::OutputPointType transformedPoint = this->m_Transform->TransformPoint( inputPoint );

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
      typedef typename Superclass::OutputPointType OutputPointType;
      typedef typename OutputPointType::CoordRepType CoordRepType;
      typedef ContinuousIndex<CoordRepType,MovingImageType::ImageDimension>
        MovingImageContinuousIndexType;

      MovingImageContinuousIndexType tempIndex;
      this->m_MovingImage->TransformPhysicalPointToContinuousIndex( transformedPoint, tempIndex );

      typename MovingImageType::IndexType mappedIndex; 
      for( unsigned int j = 0; j < MovingImageType::ImageDimension; j++ )
        {
        mappedIndex[j] = static_cast<long>( vnl_math_rnd( tempIndex[j] ) );
        }
      
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
KappaStatisticImageToImageMetric<TFixedImage,TMovingImage>
::GetValueAndDerivative(const TransformParametersType & parameters, 
                        MeasureType & Value, DerivativeType  & Derivative) const
{
  Value      = this->GetValue( parameters );
  this->GetDerivative( parameters, Derivative );
}


/*
 * PrintSelf
 */
template <class TFixedImage, class TMovingImage> 
void
KappaStatisticImageToImageMetric<TFixedImage,TMovingImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Complement: "         << (m_Complement ? "On" : "Off")  << std::endl; 
  os << indent << "ForegroundValue: "    << m_ForegroundValue << std::endl;
}

} // end namespace itk


#endif
