/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageMapper.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageMapper_txx
#define _itkImageMapper_txx

#include "itkImageMapper.h"

namespace itk
{

/*
 * Constructor
 */
template <class TImage, class TTransformation> 
ImageMapper<TImage,TTransformation>
::ImageMapper()
{
}



/*
 * Set the Domain
 */
template <class TImage, class TTransformation> 
void
ImageMapper<TImage,TTransformation>
::SetDomain(const DomainType *  domain)
{
  Superclass::SetDomain( domain );

  m_Interpolator = InterpolatorType::New();
  m_Interpolator->SetInputImage( domain );

}





/*
 * Test whether the point is inside the image domain
 */
template <class TImage, class TTransformation> 
bool
ImageMapper<TImage,TTransformation>
::IsInside( const InputPointType & point ) 
{ 

  m_CurrentPoint = m_Transform->TransformPoint( point );
  return ( m_Interpolator->IsInsideBuffer( m_CurrentPoint ) );

}




/*
 * Evaluate the image at some point
 */
template <class TImage, class TTransformation> 
double
ImageMapper<TImage,TTransformation>
::Evaluate( void ) const
{ 
  return( m_Interpolator->Evaluate( m_CurrentPoint ) );

}


/*
 * PrintSelf
 */
template <class TImage, class TTransformation> 
void
ImageMapper<TImage,TTransformation>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Interpolator: ";
  os << m_Interpolator.GetPointer() << std::endl;
  os << indent << "CurrentPoint: ";
  os << m_CurrentPoint << std::endl;

}


} // end namespace itk

#endif
