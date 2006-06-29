/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageToImageMetric_txx
#define _itkImageToImageMetric_txx


#include "itkImageToImageMetric.h"


namespace itk
{

/*
 * Constructor
 */
template <class TFixedImage, class TMovingImage> 
ImageToImageMetric<TFixedImage,TMovingImage>
::ImageToImageMetric()
{
  m_FixedImage    = 0; // has to be provided by the user.
  m_MovingImage   = 0; // has to be provided by the user.
  m_Transform     = 0; // has to be provided by the user.
  m_Interpolator  = 0; // has to be provided by the user.
  m_GradientImage = 0; // will receive the output of the filter;
  m_ComputeGradient = true; // metric computes gradient by default
  m_NumberOfPixelsCounted = 0; // initialize to zero
  m_GradientImage = NULL; // computed at initialization
}


/*
 * Set the parameters that define a unique transform
 */
template <class TFixedImage, class TMovingImage> 
void
ImageToImageMetric<TFixedImage,TMovingImage>
::SetTransformParameters( const ParametersType & parameters ) const
{
  if( !m_Transform )
    {
    itkExceptionMacro(<<"Transform has not been assigned");
    }
  m_Transform->SetParameters( parameters );
}



/*
 * Initialize
 */
template <class TFixedImage, class TMovingImage> 
void
ImageToImageMetric<TFixedImage,TMovingImage>
::Initialize(void) throw ( ExceptionObject )
{

  if( !m_Transform )
    {
    itkExceptionMacro(<<"Transform is not present");
    }

  if( !m_Interpolator )
    {
    itkExceptionMacro(<<"Interpolator is not present");
    }

  if( !m_MovingImage )
    {
    itkExceptionMacro(<<"MovingImage is not present");
    }

  if( !m_FixedImage )
    {
    itkExceptionMacro(<<"FixedImage is not present");
    }

  if( m_FixedImageRegion.GetNumberOfPixels() == 0 )
    {
    itkExceptionMacro(<<"FixedImageRegion is empty");
    }

  // If the image is provided by a source, update the source.
  if( m_MovingImage->GetSource() )
    {
    m_MovingImage->GetSource()->Update();
    }

  // If the image is provided by a source, update the source.
  if( m_FixedImage->GetSource() )
    {
    m_FixedImage->GetSource()->Update();
    }

  // Make sure the FixedImageRegion is within the FixedImage buffered region
  if ( !m_FixedImageRegion.Crop( m_FixedImage->GetBufferedRegion() ) )
    {
    itkExceptionMacro(<<"FixedImageRegion does not overlap the fixed image buffered region" );
    }

  m_Interpolator->SetInputImage( m_MovingImage );
 
  if ( m_ComputeGradient )
    {
    this->ComputeGradient();
    }

  // If there are any observers on the metric, call them to give the
  // user code a chance to set parameters on the metric
  this->InvokeEvent( InitializeEvent() );
}


/*
 * Compute the gradient image and assign it to m_GradientImage.
 */
template <class TFixedImage, class TMovingImage> 
void
ImageToImageMetric<TFixedImage,TMovingImage>
::ComputeGradient() 
{
  GradientImageFilterPointer gradientFilter
    = GradientImageFilterType::New();

  gradientFilter->SetInput( m_MovingImage );

  const typename MovingImageType::SpacingType&
    spacing = m_MovingImage->GetSpacing();
  double maximumSpacing=0.0;
  for(unsigned int i=0; i<MovingImageDimension; i++)
    {
    if( spacing[i] > maximumSpacing )
      {
      maximumSpacing = spacing[i];
      }
    }
  gradientFilter->SetSigma( maximumSpacing );
  gradientFilter->SetNormalizeAcrossScale( true );
  
  gradientFilter->Update();
  
  m_GradientImage = gradientFilter->GetOutput();
}


/*
 * PrintSelf
 */
template <class TFixedImage, class TMovingImage> 
void
ImageToImageMetric<TFixedImage,TMovingImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "ComputeGradient: "
     << static_cast<typename NumericTraits<bool>::PrintType>(m_ComputeGradient)
     << std::endl;
  os << indent << "Moving Image: " << m_MovingImage.GetPointer()  << std::endl;
  os << indent << "Fixed  Image: " << m_FixedImage.GetPointer()   << std::endl;
  os << indent << "Gradient Image: " << m_GradientImage.GetPointer()   << std::endl;
  os << indent << "Transform:    " << m_Transform.GetPointer()    << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
  os << indent << "FixedImageRegion: " << m_FixedImageRegion << std::endl;
  os << indent << "Moving Image Mask: " << m_MovingImageMask.GetPointer() << std::endl;
  os << indent << "Fixed Image Mask: " << m_FixedImageMask.GetPointer() << std::endl;
  os << indent << "Number of Pixels Counted: " << m_NumberOfPixelsCounted << std::endl;

}


} // end namespace itk

#endif
