/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPointSetToImageMetric_txx
#define _itkPointSetToImageMetric_txx


#include "itkPointSetToImageMetric.h"


namespace itk
{

/*
 * Constructor
 */
template <class TFixedPointSet, class TMovingImage> 
PointSetToImageMetric<TFixedPointSet,TMovingImage>
::PointSetToImageMetric()
{
  m_FixedPointSet = 0; // has to be provided by the user.
  m_MovingImage   = 0; // has to be provided by the user.
  m_Transform     = 0; // has to be provided by the user.
  m_Interpolator  = 0; // has to be provided by the user.
  m_ScaleGradient = 1.0f; // Default value of sigma for the gradient
  m_ComputeGradient = true; // metric computes gradient by default
  m_NumberOfPixelsCounted = 0; // initialize to zero
  m_GradientImage = NULL; // computed at initialization
}


/*
 * Set the parameters that define a unique transform
 */
template <class TFixedPointSet, class TMovingImage> 
void
PointSetToImageMetric<TFixedPointSet,TMovingImage>
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
template <class TFixedPointSet, class TMovingImage> 
void
PointSetToImageMetric<TFixedPointSet,TMovingImage>
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

  if( !m_FixedPointSet )
    {
    itkExceptionMacro(<<"FixedPointSet is not present");
    }

  // If the image is provided by a source, update the source.
  if( m_MovingImage->GetSource() )
    {
    m_MovingImage->GetSource()->Update();
    }

  // If the point set is provided by a source, update the source.
  if( m_FixedPointSet->GetSource() )
    {
    m_FixedPointSet->GetSource()->Update();
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
template <class TFixedPointSet, class TMovingImage> 
void
PointSetToImageMetric<TFixedPointSet,TMovingImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Moving Image: " << m_MovingImage.GetPointer()  << std::endl;
  os << indent << "Fixed  Image: " << m_FixedPointSet.GetPointer()   << std::endl;
  os << indent << "Transform:    " << m_Transform.GetPointer()    << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
  os << indent << "Number of Pixels Counted: " << m_NumberOfPixelsCounted << std::endl;
  os << indent << "ScaleGradient: " << m_ScaleGradient << std::endl;

}


} // end namespace itk

#endif
