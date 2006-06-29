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
#include "itkImageRegionIteratorWithIndex.h"

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

  typedef  itk::ImageRegionConstIteratorWithIndex< FixedImageType > FixedIteratorType;

  FixedIteratorType ti( fixedImage, this->GetFixedImageRegion() );
  
  typename FixedImageType::IndexType index;

  this->m_NumberOfPixelsCounted = 0;
  
  this->SetTransformParameters( parameters );
  
  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType( ParametersDimension );
  derivative.Fill( NumericTraits<ITK_TYPENAME DerivativeType::ValueType>::Zero );

  typedef Array<double> ArrayType;

  ArrayType sum1 = ArrayType( ParametersDimension );
  sum1.Fill(  NumericTraits<ITK_TYPENAME ArrayType::ValueType>::Zero );

  ArrayType sum2 = ArrayType( ParametersDimension );
  sum2.Fill(  NumericTraits<ITK_TYPENAME ArrayType::ValueType>::Zero );

  int fixedArea = 0;
  int movingArea = 0;
  int intersection = 0;

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

    const RealType fixedValue = ti.Value();       
    if ( fixedValue == m_ForegroundValue )
      {
      fixedArea++;
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
      
      if ( movingValue == m_ForegroundValue )
        {
        movingArea++;
        }

      if (( movingValue == m_ForegroundValue )&&( fixedValue == m_ForegroundValue ))
        {
        intersection++;
        }

      const TransformJacobianType & jacobian =
        this->m_Transform->GetJacobian( inputPoint ); 
      
      this->m_NumberOfPixelsCounted++;

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
      
      const GradientPixelType gradient = this->m_GradientImage->GetPixel( mappedIndex );

      for(unsigned int par=0; par<ParametersDimension; par++)
        {
        for(unsigned int dim=0; dim<ImageDimension; dim++)
          {
          sum2[par] += jacobian( dim, par )*gradient[dim];
          if ( fixedValue == m_ForegroundValue )
            {
            sum1[par] += 2.0*jacobian( dim, par )*gradient[dim];
            }            
          }
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
    double areaSum = double(fixedArea)+double(movingArea);
    for(unsigned int par=0; par<ParametersDimension; par++)
      {
      derivative[par] = -(areaSum*sum1[par]-2.0*intersection*sum2[par])/(areaSum*areaSum);
      }
    }
}


/*
 * Compute the image gradient and assign to m_GradientImage.
 */
template <class TFixedImage, class TMovingImage> 
void
KappaStatisticImageToImageMetric<TFixedImage,TMovingImage>
::ComputeGradient()
{
  const unsigned int dim = MovingImageType::ImageDimension;

  typedef itk::Image< GradientPixelType, dim > GradientImageType;
  typename GradientImageType::Pointer tempGradientImage = GradientImageType::New();
    tempGradientImage->SetRegions( this->m_MovingImage->GetBufferedRegion().GetSize() );
    tempGradientImage->Allocate();
    tempGradientImage->Update();

  typedef  itk::ImageRegionIteratorWithIndex< GradientImageType > GradientIteratorType;
  typedef  itk::ImageRegionConstIteratorWithIndex< MovingImageType > MovingIteratorType; 

  GradientIteratorType git( tempGradientImage, tempGradientImage->GetBufferedRegion() );
  MovingIteratorType mit( this->m_MovingImage, this->m_MovingImage->GetBufferedRegion() );

  git.GoToBegin();
  mit.GoToBegin();

  typename MovingImageType::IndexType minusIndex;
  typename MovingImageType::IndexType plusIndex;
  typename MovingImageType::IndexType currIndex;
  typename GradientImageType::PixelType tempGradPixel;
  typename MovingImageType::SizeType movingSize = this->m_MovingImage->GetBufferedRegion().GetSize();
  while(!mit.IsAtEnd())
    {
    currIndex = mit.GetIndex();
    minusIndex = mit.GetIndex();
    plusIndex = mit.GetIndex();
    for ( int i=0; i<dim; i++ )
      {
      if ((currIndex[i] == 0)||(currIndex[i]==(movingSize[i]-1)))
        {
        tempGradPixel[i] = 0;
        }
      else
        {
        minusIndex[i] = currIndex[i]-1;
        plusIndex[i] = currIndex[i]+1;
        double minusVal = double(this->m_MovingImage->GetPixel(minusIndex));
        double val      = double(this->m_MovingImage->GetPixel(currIndex));
        double plusVal  = double(this->m_MovingImage->GetPixel(plusIndex));
        if ((minusVal != m_ForegroundValue)&&(plusVal == m_ForegroundValue))
          {
          tempGradPixel[i] = 1;
          }
        else if ((minusVal == m_ForegroundValue)&&(plusVal != m_ForegroundValue))
          {
          tempGradPixel[i] = -1;
          }
        else
          {
          tempGradPixel[i] = 0;
          }
        }
      minusIndex = currIndex;
      plusIndex  = currIndex;
      }
    git.Set( tempGradPixel );
    ++git;
    ++mit;
    }

  this->m_GradientImage = tempGradientImage;
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
