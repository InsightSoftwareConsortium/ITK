/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFixedArray.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFixedArray_txx
#define _itkFixedArray_txx

#include "itkFixedArray.h"

namespace itk
{

/**
 * Default constructor uses compiler's default initialization of memory.
 * For efficiency, no initialization to zero is done.
 */
template <typename TValueType, unsigned int VLength>
FixedArray<TValueType, VLength>
::FixedArray()
{
}

/**
 * Copy constructor copies all FixedArray values.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template <typename TValueType, unsigned int VLength>
FixedArray<TValueType, VLength>
::FixedArray(const FixedArray& r)
{
  ConstIterator input = r.Begin();
  for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
}

/**
 * Constructor assumes input points to array of correct size.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template <typename TValueType, unsigned int VLength>
FixedArray<TValueType, VLength>
::FixedArray(const ValueType r[VLength])
{
  ConstIterator input = r;
  for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
}


/**
 * Destructor does nothing special.  Here for completeness.
 */
template <typename TValueType, unsigned int VLength>
FixedArray<TValueType, VLength>
::~FixedArray()
{
}


/**
 * Assignment operator copies all FixedArray values.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template <typename TValueType, unsigned int VLength>
FixedArray<TValueType, VLength>&
FixedArray<TValueType, VLength>
::operator= (const FixedArray& r)
{
  if(r.Begin() == m_InternalArray) return *this;
  ConstIterator input = r.Begin();
  for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
  return *this;
}


/**
 * Assignment operator assumes input points to array of correct size.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template <typename TValueType, unsigned int VLength>
FixedArray<TValueType, VLength>&
FixedArray<TValueType, VLength>
::operator= (const ValueType r[VLength])
{
  if(r == m_InternalArray) return *this;
  ConstIterator input = r;
  for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
  return *this;
}


/**
 * Operator != compares different types of arrays.
 */
template <typename TValueType, unsigned int VLength>
bool
FixedArray<TValueType, VLength>
::operator== (const FixedArray& r) const
{
  ConstIterator i = this->Begin();
  ConstIterator j = r.Begin();
  
  for( ; i != this->End(); ++i, ++j )
    {
    if ( *i != *j ) {return false;}
    }

  return true;
}

/**
 * Get an Iterator for the beginning of the FixedArray.
 */
template <typename TValueType, unsigned int VLength>
typename FixedArray<TValueType, VLength>::Iterator
FixedArray<TValueType, VLength>
::Begin()
{
  return Iterator(m_InternalArray);
}


/**
 * Get a ConstIterator for the beginning of the FixedArray.
 */
template <typename TValueType, unsigned int VLength>
typename FixedArray<TValueType, VLength>::ConstIterator
FixedArray<TValueType, VLength>
::Begin() const
{
  return ConstIterator(m_InternalArray);
}


/**
 * Get an Iterator for the end of the FixedArray.
 */
template <typename TValueType, unsigned int VLength>
typename FixedArray<TValueType, VLength>::Iterator
FixedArray<TValueType, VLength>
::End()
{
  return Iterator(m_InternalArray+VLength);
}


/**
 * Get a ConstIterator for the end of the FixedArray.
 */
template <typename TValueType, unsigned int VLength>
typename FixedArray<TValueType, VLength>::ConstIterator
FixedArray<TValueType, VLength>
::End() const
{
  return ConstIterator(m_InternalArray+VLength);
}


/**
 * Get a begin ReverseIterator.
 */
template <typename TValueType, unsigned int VLength>
typename FixedArray<TValueType, VLength>::ReverseIterator
FixedArray<TValueType, VLength>
::rBegin() 
{
  return ReverseIterator(m_InternalArray+VLength);
}


/**
 * Get a begin ConstReverseIterator.
 */
template <typename TValueType, unsigned int VLength>
typename FixedArray<TValueType, VLength>::ConstReverseIterator
FixedArray<TValueType, VLength>
::rBegin() const
{
  return ConstReverseIterator(m_InternalArray+VLength);
}


/**
 * Get an end ReverseIterator.
 */
template <typename TValueType, unsigned int VLength>
typename FixedArray<TValueType, VLength>::ReverseIterator
FixedArray<TValueType, VLength>
::rEnd() 
{
  return ReverseIterator(m_InternalArray);
}


/**
 * Get an end ConstReverseIterator.
 */
template <typename TValueType, unsigned int VLength>
typename FixedArray<TValueType, VLength>::ConstReverseIterator
FixedArray<TValueType, VLength>
::rEnd() const 
{
  return ConstReverseIterator(m_InternalArray);
}


/**
 * Get the size of the FixedArray.
 */
template <typename TValueType, unsigned int VLength>
typename FixedArray<TValueType, VLength>::SizeType
FixedArray<TValueType, VLength>
::Size() const
{
  return VLength; 
}


/**
 * Fill all elements of the array with the given value.
 */
template <typename TValueType, unsigned int VLength>
void
FixedArray<TValueType, VLength>
::Fill(const ValueType& value)
{
  for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = value;
}


/**
 * Return an FixedArray with all elements assigned to the given value.
 */
template <typename TValueType, unsigned int VLength>
FixedArray<TValueType, VLength>
FixedArray<TValueType, VLength>
::Filled(const ValueType& value)
{
  FixedArray<ValueType, VLength> array;
  array.Fill(value);
  return array;
}


} // namespace itk

#endif
