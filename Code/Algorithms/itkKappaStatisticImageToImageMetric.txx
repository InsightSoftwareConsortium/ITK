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

  m_ComputeGradient = false; // don't use the default gradients
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
  FixedImageConstPointer fixedImage = this->GetFixedImage();
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
  MovingImageConstPointer movingImage = this->GetMovingImage();
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

    if( m_FixedImageMask && !m_FixedImageMask->IsInside( fixedInputPoint ) )
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
      transformedPoint = m_Transform->TransformPoint( fixedInputPoint );

    if( m_MovingImageMask && !m_MovingImageMask->IsInside( transformedPoint ) )
      {
      ++fi;
      continue;
      }

    //Compute movingForegroundArea and intersection
    //
    //
    if( m_Interpolator->IsInsideBuffer( transformedPoint ) )
      {    
      const RealType movingValue = m_Interpolator->Evaluate( transformedPoint );
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
