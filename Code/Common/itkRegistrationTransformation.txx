/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationTransformation.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/


namespace itk
{

/**
 * Constructor
 */
template <class TParameters>
RegistrationTransformation<TParameters>
::RegistrationTransformation()
{
  m_Parameters = ParametersType::New();
}


/**
 * Constructor
 */
template <class TParameters>
RegistrationTransformation<TParameters>
::RegistrationTransformation( const Self & other )
{
}


/**
 * Assignment Operator
 */
template <class TParameters>
const RegistrationTransformation<TParameters> &
RegistrationTransformation<TParameters>
::operator=( const Self & other )
{
  return *this;
}



} // end namespace itk
