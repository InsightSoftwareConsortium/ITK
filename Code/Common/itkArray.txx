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
#include <iostream>

namespace itk
{



/** Default constructor  */
template < typename TValueType >
Array<TValueType >
::Array():vnl_vector_ref<TValueType>(1,vnl_c_vector<TValueType>::allocate_T(1))
{

}


/** Constructor with size */
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

/** Copy Constructor */ 
template < typename TValueType >
Array<TValueType >
::Array(Array<TValueType> const& v) : vnl_vector_ref<TValueType>(v.Size(),vnl_c_vector<TValueType>::allocate_T(v.Size()))
{
 for (unsigned i = 0; i < this->num_elmts; i++)
   {
   this->data[i] = v.data[i];
   }

}

template < typename TValueType >
void Array<TValueType >
::SetSize(unsigned int sz)
{
  this->set_size(sz);
}


template < typename TValueType >
Array<TValueType> Array<TValueType>::operator= (Array<TValueType> const rhs) 
{
if (this != &rhs) { // make sure *this != m
    if (rhs.data) {
      if (this->num_elmts != rhs.num_elmts)
        this->set_size(rhs.size());
      for (unsigned i = 0; i < this->num_elmts; i++)
        this->data[i] = rhs.data[i];
    }
    else {
      // rhs is default-constructed.
      clear();
    }
  }
  return *this;
}


} // namespace itk

#endif
