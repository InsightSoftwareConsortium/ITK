/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkArray.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkArray_txx
#define _itkArray_txx

#include "itkArray.h"

namespace itk
{

/**
 * Default constructor uses compiler's default initialization of memory.
 * For efficiency, no initialization to zero is done.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>
::Array()
{
}

/**
 * Copy constructor copies all Array values.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>
::Array(const Array& r)
{
  ConstIterator input = r.Begin();
  for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
}

/**
 * Copy constructor copies all Array values.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>
::Array(const Reference& r)
{
  ConstIterator input = r.Begin();
  for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
}


/**
 * Copy constructor copies all Array values.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>
::Array(const ConstReference& r)
{
  ConstIterator input = r.Begin();
  for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
}


/**
 * Constructor assumes input points to array of correct size.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>
::Array(const ValueType r[Length])
{
  ConstIterator input = r;
  for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
}


/**
 * Destructor does nothing special.  Here for completeness.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>
::~Array()
{
}


/**
 * Assignment operator copies all Array values.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>&
Array<TValueType, VLength>
::operator= (const Array& r)
{
  if(r.Begin() == m_InternalArray) return *this;
  ConstIterator input = r.Begin();
  for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
  return *this;
}


/**
 * Assignment operator copies all Array values.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>&
Array<TValueType, VLength>
::operator= (const Reference& r)
{
  if(r.Begin() == m_InternalArray) return *this;
  ConstIterator input = r.Begin();
  for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
  return *this;
}


/**
 * Assignment operator copies all Array values.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>&
Array<TValueType, VLength>
::operator= (const ConstReference& r)
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
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>&
Array<TValueType, VLength>
::operator= (const ValueType r[Length])
{
  if(r == m_InternalArray) return *this;
  ConstIterator input = r;
  for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
  return *this;
}


/**
 * Assignment operator to allow assignment via a comma-separated list.
 * It is assumed that the list is of the appropriate length.
 */
template <typename TValueType, unsigned long VLength>
typename Array<TValueType, VLength>::ArrayCommaListCopier
Array<TValueType, VLength>
::operator= (const ValueType& r)
{
  return ArrayCommaListCopier(this->Begin(), r);
}


/**
 * Get an Iterator for the beginning of the Array.
 */
template <typename TValueType, unsigned long VLength>
typename Array<TValueType, VLength>::Iterator
Array<TValueType, VLength>
::Begin()
{
  return Iterator(m_InternalArray);
}


/**
 * Get a ConstIterator for the beginning of the Array.
 */
template <typename TValueType, unsigned long VLength>
typename Array<TValueType, VLength>::ConstIterator
Array<TValueType, VLength>
::Begin() const
{
  return ConstIterator(m_InternalArray);
}


/**
 * Get an Iterator for the end of the Array.
 */
template <typename TValueType, unsigned long VLength>
typename Array<TValueType, VLength>::Iterator
Array<TValueType, VLength>
::End()
{
  return Iterator(m_InternalArray+Length);
}


/**
 * Get a ConstIterator for the end of the Array.
 */
template <typename TValueType, unsigned long VLength>
typename Array<TValueType, VLength>::ConstIterator
Array<TValueType, VLength>
::End() const
{
  return ConstIterator(m_InternalArray+Length);
}


/**
 * Get a begin ReverseIterator.
 */
template <typename TValueType, unsigned long VLength>
typename Array<TValueType, VLength>::ReverseIterator
Array<TValueType, VLength>
::rBegin() 
{
  return ReverseIterator(m_InternalArray+Length);
}


/**
 * Get a begin ConstReverseIterator.
 */
template <typename TValueType, unsigned long VLength>
typename Array<TValueType, VLength>::ConstReverseIterator
Array<TValueType, VLength>
::rBegin() const
{
  return ConstReverseIterator(m_InternalArray+Length);
}


/**
 * Get an end ReverseIterator.
 */
template <typename TValueType, unsigned long VLength>
typename Array<TValueType, VLength>::ReverseIterator
Array<TValueType, VLength>
::rEnd() 
{
  return ReverseIterator(m_InternalArray);
}


/**
 * Get an end ConstReverseIterator.
 */
template <typename TValueType, unsigned long VLength>
typename Array<TValueType, VLength>::ConstReverseIterator
Array<TValueType, VLength>
::rEnd() const 
{
  return ConstReverseIterator(m_InternalArray);
}


/**
 * Get the size of the Array.
 */
template <typename TValueType, unsigned long VLength>
typename Array<TValueType, VLength>::SizeType
Array<TValueType, VLength>
::Size() const
{
  return Length; 
}


/**
 * Fill all elements of the array with the given value.
 */
template <typename TValueType, unsigned long VLength>
void
Array<TValueType, VLength>
::Fill(const ValueType& value)
{
  for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = value;
}


/**
 * Return an Array with all elements assigned to the given value.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>
Array<TValueType, VLength>
::Filled(const ValueType& value)
{
  Array<ValueType, Length> array;
  array.Fill(value);
  return array;
}


} // namespace itk

#endif
