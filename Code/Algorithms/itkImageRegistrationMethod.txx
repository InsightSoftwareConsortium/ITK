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
 * Starts the Registration Process
 */
template < typename TFixedImage, typename TMovingImage >
void
ImageRegistrationMethod<TFixedImage,TMovingImage>
::StartRegistration( void )
{ 
  itkExceptionMacro(<< "ImageRegistrationMethod::StartRegistration::" );
  itkExceptionMacro(<< "This method should have been overloaded" );

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
  os << indent << "Optimizer: ";
  os << m_Optimizer.GetPointer() << std::endl;
  os << indent << "Fixed Image " << m_FixedImage.GetPointer() << std::endl;
  os << indent << "Moving Image " << m_MovingImage.GetPointer() << std::endl;
}




} // end namespace itk


#endif
