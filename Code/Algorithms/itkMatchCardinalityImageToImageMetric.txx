/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMatchCardinalityImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMatchCardinalityImageToImageMetric_txx
#define _itkMatchCardinalityImageToImageMetric_txx

#include "itkMatchCardinalityImageToImageMetric.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

/*
 * Constructor
 */
template <class TFixedImage, class TMovingImage> 
MatchCardinalityImageToImageMetric<TFixedImage,TMovingImage>
::MatchCardinalityImageToImageMetric()
{
  itkDebugMacro("Constructor");

  m_ComputeGradient = false; // don't use the default gradients
  m_MeasureMatches = true;  // default to measure percentage of pixel matches
}

/*
 * Get the match Measure
 */
template <class TFixedImage, class TMovingImage> 
typename MatchCardinalityImageToImageMetric<TFixedImage,TMovingImage>::MeasureType
MatchCardinalityImageToImageMetric<TFixedImage,TMovingImage>
::GetValue( const TransformParametersType & parameters ) const
{

  itkDebugMacro("GetValue( " << parameters << " ) ");

  FixedImageConstPointer fixedImage = this->GetFixedImage();
  if( !fixedImage ) 
    {
    itkExceptionMacro( << "Fixed image has not been assigned" );
    }

  typedef  itk::ImageRegionConstIteratorWithIndex<FixedImageType> FixedIteratorType;
  typename FixedImageType::IndexType index;
  FixedIteratorType ti( fixedImage, this->GetFixedImageRegion() );

  MeasureType measure = NumericTraits< MeasureType >::Zero;
  m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters( parameters );
  while(!ti.IsAtEnd())
    {
    index = ti.GetIndex();
    
    typename Superclass::InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint( index, inputPoint );

    if( m_FixedImageMask && !m_FixedImageMask->IsInside( inputPoint ) )
      {
      ++ti;
      continue;
      }

    typename Superclass::OutputPointType
      transformedPoint = m_Transform->TransformPoint( inputPoint );

    if( m_MovingImageMask && !m_MovingImageMask->IsInside( transformedPoint ) )
      {
      ++ti;
      continue;
      }

    if( m_Interpolator->IsInsideBuffer( transformedPoint ) )
      {
      const RealType movingValue= m_Interpolator->Evaluate( transformedPoint );
      const RealType fixedValue = ti.Get();
      RealType diff;
      
      m_NumberOfPixelsCounted++;

      if (m_MeasureMatches)
        {
        diff = (movingValue == fixedValue); // count matches
        }
      else
        {
        diff = (movingValue != fixedValue); // count mismatches
        }
      measure += diff; 
      }

    ++ti;
    }

  if( !m_NumberOfPixelsCounted )
    {
    itkExceptionMacro(<<"All the points mapped to outside of the moving image");
    }
  else
    {
    measure /= m_NumberOfPixelsCounted;
    }

  return measure;

}


/*
 * PrintSelf
 */
template <class TFixedImage, class TMovingImage> 
void
MatchCardinalityImageToImageMetric<TFixedImage,TMovingImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "MeasureMatches: " << (m_MeasureMatches ? "On" : "Off")  << std::endl;
}

} // end namespace itk


#endif
