/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationMapper.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRegistrationMapper_txx
#define _itkRegistrationMapper_txx



namespace itk
{

/*
 * Constructor
 */
template <class TDomain, class TTransform> 
RegistrationMapper<TDomain,TTransform>
::RegistrationMapper()
{
  m_Domain         = DomainType::New();
  m_Transform      = TransformType::New();
}


/*
 * PrintSelf
 */
template <class TDomain, class TTransform> 
void
RegistrationMapper<TDomain,TTransform>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Domain: " << m_Domain.GetPointer() << std::endl;
  os << indent << "Transform: " << m_Transform.GetPointer() << std::endl;
}


} // end namespace itk

#endif
