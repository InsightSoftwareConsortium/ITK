/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationMapperProcrustes.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRegistrationMapperProcrustes_txx
#define _itkRegistrationMapperProcrustes_txx



namespace itk
{

/**
 * Constructor
 */
template <class TTransformation, unsigned int NDimension> 
RegistrationMapperProcrustes<TTransformation,NDimension>
::RegistrationMapperProcrustes()
{
}


/**
 * Set Domain 
 */
template <class TTransformation, unsigned int NDimension>
void
RegistrationMapperProcrustes<TTransformation,NDimension>
::SetDomain( DomainType * domain ) 
{
  this->m_Domain = domain;
}




/**
 * Set Transformation
 */
template <class TTransformation, unsigned int NDimension>
void
RegistrationMapperProcrustes<TTransformation,NDimension>
::SetTransformation( TTransformation * transformation ) 
{
  this->m_Transformation = transformation;
}



/**
 * Transform a point from one coordinate system to the 
 * other.
 */
template <class TTransformation, unsigned int NDimension>
RegistrationMapperProcrustes<TTransformation,NDimension>::OutputPointType
RegistrationMapperProcrustes<TTransformation,NDimension>
::Transform( const InputPointType & point )
{
  return this->m_Transformation->TransformPoint( point ); 
}






} // end namespace itk

#endif
