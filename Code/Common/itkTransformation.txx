/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: 
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#ifndef _itkTransformation_txx
#define _itkTransformation_txx

#include "itkTransformation.h"

namespace itk
{

/**
 * Constructor
 */
template <class TScalarType,int NDimensions>
Transformation<TScalarType,NDimensions>
::Transformation()
{
 
}


/**
 * Constructor
 */
template <class TScalarType,int NDimensions>
Transformation<TScalarType,NDimensions>
::Transformation( const Self & other )
{
}


/**
 * Assignment Operator
 */
template <class TScalarType,int NDimensions>
const Transformation<TScalarType,NDimensions> &
Transformation<TScalarType,NDimensions>
::operator=( const Self & other )
{
  return *this;
}



} // end namespace itk


#endif

