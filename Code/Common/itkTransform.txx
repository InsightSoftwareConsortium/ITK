/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkTransform_txx
#define _itkTransform_txx


namespace itk
{

/**
 * Constructor
 */
template <class TParameters>
Transform<TParameters>
::Transform()
{
  m_Parameters = ParametersType::New();
}


/**
 * Constructor
 */
template <class TParameters>
Transform<TParameters>
::Transform( const Self & other )
{
}


/**
 * Assignment Operator
 */
template <class TParameters>
const Transform<TParameters> &
Transform<TParameters>
::operator=( const Self & other )
{
  return *this;
}



} // end namespace itk

#endif
