/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationMapperProcrustes.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

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
Point<double,NDimension>
RegistrationMapperProcrustes<TTransformation,NDimension>
::Transform( const Point<double,NDimension> & point )
{
  return this->m_Transformation->Transform( point ); 
}






} // end namespace itk

#endif
