/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegistrationMethod.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageRegistrationMethod_txx
#define _itkImageRegistrationMethod_txx

#include "itkImageRegistrationMethod.h"


namespace itk
{


/**
 * Initialize by setting the interconnects between components. 
 */
template < typename TFixedImage, typename TMovingImage >
void
ImageRegistrationMethod<TFixedImage,TMovingImage>
::Initialize() throw (ExceptionObject)
{

  if( !m_FixedImage )
    {
    itkExceptionMacro(<<"FixedImage is not present");
    }

  if( !m_MovingImage )
    {
    itkExceptionMacro(<<"MovingImage is not present");
    }

  if ( !m_Metric )
    {
    itkExceptionMacro(<<"Transform is not present" );
    }

  if ( !m_Optimizer )
    {
    itkExceptionMacro(<<"Optimizer is not present" );
    }

  if( !m_Transform )
    {
    itkExceptionMacro(<<"Transform is not present");
    }

  if( !m_Interpolator )
    {
    itkExceptionMacro(<<"Interpolator is not present");
    }

  // Setup the metric
  m_Metric->SetMovingImage( m_MovingImage );
  m_Metric->SetFixedImage( m_FixedImage );
  m_Metric->SetTransform( m_Transform );
  m_Metric->SetInterpolator( m_Interpolator );
  m_Metric->Initialize();

  // Setup the optimizer
  m_Optimizer->SetCostFunction( m_Metric );

}


/**
 * Starts the Registration Process
 */
template < typename TFixedImage, typename TMovingImage >
void
ImageRegistrationMethod<TFixedImage,TMovingImage>
::StartRegistration( void )
{ 
  this->Initialize();

//  itkWarningMacro(<<"This function has not be fully implemented" );

}


/**
 * PrintSelf
 */
template < typename TFixedImage, typename TMovingImage >
void
ImageRegistrationMethod<TFixedImage,TMovingImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Metric: " << m_Metric.GetPointer() << std::endl;
  os << indent << "Optimizer: " << m_Optimizer.GetPointer() << std::endl;
  os << indent << "Transform: " << m_Transform.GetPointer() << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
  os << indent << "Fixed Image " << m_FixedImage.GetPointer() << std::endl;
  os << indent << "Moving Image " << m_MovingImage.GetPointer() << std::endl;
}




} // end namespace itk


#endif
