/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
  m_ScaleGradient = 1.0f; // Default value of sigma for the gradient
  m_ComputeGradient = true; // metric computes gradient by default
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
 * PrintSelf
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

  m_Interpolator->SetInputImage( m_MovingImage );
 
  if ( m_ComputeGradient )
    {

    GradientImageFilterPointer gradientFilter
                      = GradientImageFilterType::New();

    gradientFilter->SetInput( m_MovingImage );

    gradientFilter->SetSigma( m_ScaleGradient );  
    gradientFilter->SetNormalizeAcrossScale( true );

    gradientFilter->Update();

    m_GradientImage = gradientFilter->GetOutput();

    }


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
  os << indent << "Moving Image: " << m_MovingImage.GetPointer()  << std::endl;
  os << indent << "Fixed  Image: " << m_FixedImage.GetPointer()   << std::endl;
  os << indent << "Transform:    " << m_Transform.GetPointer()    << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
  os << indent << "FixedImageRegion: " << m_FixedImageRegion << std::endl;
  os << indent << "Number of Pixels Counted: " << m_NumberOfPixelsCounted << std::endl;
  os << indent << "ScaleGradient: " << m_ScaleGradient << std::endl;

}


} // end namespace itk

#endif
