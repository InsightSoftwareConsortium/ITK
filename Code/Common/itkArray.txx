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
#ifndef __itkArray_txx
#define __itkArray_txx

#include "itkArray.h"

namespace itk
{

/** Default constructor s*/
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

/** Constructor with user specified data */
template < typename TValueType >
Array<TValueType >
::Array( ValueType *datain, unsigned int sz, bool LetArrayManageMemory)
{
  vnl_vector<TValueType>::data = datain;
  vnl_vector<TValueType>::num_elmts = sz;
  m_LetArrayManageMemory = LetArrayManageMemory;
}

/** Constructor with user specified data */
template < typename TValueType >
Array<TValueType >
::Array( const ValueType *datain, unsigned int sz, bool LetArrayManageMemory)
{
  vnl_vector<TValueType>::data = const_cast< TValueType * >( datain ); 
                               // Argh!! Discard constness WRONG.!!
  vnl_vector<TValueType>::num_elmts = sz;
  m_LetArrayManageMemory = LetArrayManageMemory;
}


/** Destructor */
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
::SetData(TValueType* datain,bool LetArrayManageMemory)
{
  if(m_LetArrayManageMemory)
    {
    vnl_vector<TValueType>::destroy();
    }
  vnl_vector<TValueType>::data = datain;
  m_LetArrayManageMemory = LetArrayManageMemory;
}


/** Similar to the previous method. In the above method, the size must be 
 * seperately set prior to using user-supplied data. This introduces an
 * unnecessary allocation step to be performed. This method avoids it 
 * and should be used to import data whereever possible to avoid this.
 * Set the pointer from which the data is imported.
 * If "LetArrayManageMemory" is false, then the application retains
 * the responsibility of freeing the memory for this data.  If
 * "LetArrayManageMemory" is true, then this class will free the
 * memory when this object is destroyed. */
template < typename TValueType >
void 
Array<TValueType >
::SetData(TValueType* datain, unsigned int sz, bool LetArrayManageMemory)
{
  if(m_LetArrayManageMemory)
    {
    vnl_vector<TValueType>::destroy();
    }
  vnl_vector<TValueType>::data = datain;
  vnl_vector<TValueType>::num_elmts = sz;
  m_LetArrayManageMemory = LetArrayManageMemory;
}


template < typename TValueType >
void Array<TValueType >
::SetSize(unsigned int sz)
{

  if ( this->size() != sz )
    {

    // If the array doesn't own the data we do not want to erase it
    // on a resize
    if(!m_LetArrayManageMemory)
      {
      vnl_vector<TValueType>::data = 0;
      }

    // Call the superclass's set_size
    this->set_size(sz);

    // Size we have allocated new data we need to take
    // responsibility for deleting it
    m_LetArrayManageMemory = true;

    }
    
}


template < typename TValueType >
const typename Array<TValueType>
::Self&
Array<TValueType>
::operator=( const Self& rhs )
{

  if( this == &rhs ) { return *this; }

  // Set the size the same as rhs.
  // The SetSize method takes care of who is responsible
  // for memory management
  //
  this->SetSize( rhs.GetSize() );

  // Call the superclass implementation
  this->VnlVectorType::operator=(rhs);

  return *this;
}


template < typename TValueType >
const typename Array<TValueType>
::Self&
Array<TValueType>
::operator=( const VnlVectorType& rhs )
{

  if( this == &rhs ) { return *this; }

  // Set the size the same as rhs.
  // The SetSize method takes care of who is responsible
  // for memory management
  //
  this->SetSize( rhs.size() );

  // Call the superclass implementation
  this->VnlVectorType::operator=(rhs);

  return *this;
}


} // namespace itk

#endif
