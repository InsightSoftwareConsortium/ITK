/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationMapper.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkRegistrationMapper_txx
#define _itkRegistrationMapper_txx



namespace itk
{

/**
 * Constructor
 */
template <class TDomain, class TTransformation> 
RegistrationMapper<TDomain,TTransformation>
::RegistrationMapper()
{
}

/**
 * Set the Domain
 */
template <class TDomain, class TTransformation> 
void
RegistrationMapper<TDomain,TTransformation>
::SetDomain(DomainPointer & domain)
{
  m_Domain = DomainType::New();
  m_Domain = domain;
}





} // end namespace itk

#endif
