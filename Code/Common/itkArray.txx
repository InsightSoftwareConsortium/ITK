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
  m_LetArrayManageMemory = true;
}


/** Constructor with size */
template < typename TValueType >
Array<TValueType >
::Array(unsigned int dimension):vnl_vector<TValueType>(dimension)
{
  m_LetArrayManageMemory = true;
}

/** Destructor*/
template < typename TValueType >
Array<TValueType >
::~Array()
{
  if(!m_LetArrayManageMemory)
    {
    vnl_vector<TValueType>::data = 0;
    }
}


/** Set the pointer from which the data is imported.
 * If "LetArrayManageMemory" is false, then the application retains
 * the responsibility of freeing the memory for this data.  If
 * "LetArrayManageMemory" is true, then this class will free the
 * memory when this object is destroyed. */
template < typename TValueType >
void 
Array<TValueType >
::SetData(TValueType* data,bool LetArrayManageMemory)
{
  if(m_LetArrayManageMemory)
    {
    vnl_vector<TValueType>::destroy();
    }
  vnl_vector<TValueType>::data = data;
  m_LetArrayManageMemory = LetArrayManageMemory;
}

template < typename TValueType >
void Array<TValueType >
::SetSize(unsigned int sz)
{
  // If the array doesn't own the data we do not want to erase it
  // on a resize
  if(!m_LetArrayManageMemory)
    {
    vnl_vector<TValueType>::data = 0;
    }
  this->set_size(sz);
}


} // namespace itk

#endif
