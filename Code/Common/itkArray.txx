/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkArray.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkArray_txx
#define _itkArray_txx

#include "itkArray.h"

namespace itk
{



/**
 * Default constructor 
 */
template < typename TValueType >
Array<TValueType >
::Array():vnl_vector_ref<TValueType>(0,NULL)
{
}


/**
 * Constructor with size
 */
template < typename TValueType >
Array<TValueType >
::Array(unsigned int dimension):vnl_vector_ref<TValueType>(dimension,vnl_c_vector<TValueType>::allocate_T(dimension))
{
}

/** Constructor with size and data */
template < typename TValueType >
Array<TValueType >
::Array(unsigned int dimension,TValueType* data):vnl_vector_ref<TValueType>(dimension,data)
{

}

template < typename TValueType >
void Array<TValueType >
::SetSize(unsigned int sz)
{
  this->set_size(sz);
}


} // namespace itk

#endif
