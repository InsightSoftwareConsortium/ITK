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



/** Default constructor  */
template < typename TValueType >
Array<TValueType >
::Array():vnl_vector<TValueType>()
{
  m_Array_Own_Data = false;
}


/** Constructor with size */
template < typename TValueType >
Array<TValueType >
::Array(unsigned int dimension):vnl_vector<TValueType>(dimension)
{
  m_Array_Own_Data = false;
}

/** Destructor*/
template < typename TValueType >
Array<TValueType >
::~Array()
{
  if(m_Array_Own_Data)
    {
    vnl_vector<TValueType>::data = 0;
    }
}


/** Set the pointer to the data. Pointer is not destroyed*/
template < typename TValueType >
void 
Array<TValueType >
::SetData(TValueType* data)
{
  vnl_vector<TValueType>::data = data;
  m_Array_Own_Data = true;
}

template < typename TValueType >
void Array<TValueType >
::SetSize(unsigned int sz)
{
  this->set_size(sz);
}


} // namespace itk

#endif
