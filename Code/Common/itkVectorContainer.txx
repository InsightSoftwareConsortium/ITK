/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorContainer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkVectorContainer_txx
#define _itkVectorContainer_txx
#include "itkVectorContainer.h"

#include "itkNumericTraits.h"

namespace itk
{

/**
 * Get a reference to the element at the given index.
 * It is assumed that the index exists, and it will not automatically
 * be created.
 *
 * It is assumed that the value of the element is modified through the
 * reference.
 */
template <typename TElementIdentifier, typename TElement>
typename VectorContainer< TElementIdentifier , TElement >::Element&
VectorContainer< TElementIdentifier , TElement >
::ElementAt(ElementIdentifier id)
{
  this->Modified();
  return this->VectorType::operator[](id);
}

/**
 * Get a reference to the element at the given index.
 * It is assumed that the index exists, and it will not automatically
 * be created.
 *
 */
template <typename TElementIdentifier, typename TElement>
const typename VectorContainer< TElementIdentifier , TElement >::Element&
VectorContainer< TElementIdentifier , TElement >
::ElementAt(ElementIdentifier id) const
{
  return this->VectorType::operator[](id);
}



/**
 * Get a reference to the element at the given index.
 * If the element location does not exist, it will be created with a
 * default element value.
 *
 * It is assumed that the value of the element is modified through the
 * reference.
 */
template <typename TElementIdentifier, typename TElement>
typename VectorContainer< TElementIdentifier , TElement >::Element&
VectorContainer< TElementIdentifier , TElement >
::CreateElementAt(ElementIdentifier id)
{
  if(id >= this->VectorType::size())
    {
    this->CreateIndex(id);
    }
  this->Modified();
  return this->VectorType::operator[](id);
}


/**
 * Read the element from the given index.
 * It is assumed that the index exists.
 */
template <typename TElementIdentifier, typename TElement>
typename VectorContainer< TElementIdentifier , TElement >::Element
VectorContainer< TElementIdentifier , TElement >
::GetElement(ElementIdentifier id) const
{
  return this->VectorType::operator[](id);
}


/**
 * Set the element value at the given index.
 * It is assumed that the index exists.
 */
template <typename TElementIdentifier, typename TElement>
void
VectorContainer< TElementIdentifier , TElement >
::SetElement(ElementIdentifier id, Element element)
{
  this->VectorType::operator[](id) = element;
  this->Modified();
}


/**
 * Set the element value at the given index.
 * If the element location does not exist, it will be created with a
 * default element value.
 */
template <typename TElementIdentifier, typename TElement>
void
VectorContainer< TElementIdentifier , TElement >
::InsertElement(ElementIdentifier id, Element element)
{
  if(id >= this->VectorType::size())
    {
    this->CreateIndex(id);
    }
  this->VectorType::operator[](id) = element;
  this->Modified();
}


/**
 * Check if the index range of the STL vector is large enough to allow the
 * given index without expansion.
 */
template <typename TElementIdentifier, typename TElement>
bool
VectorContainer< TElementIdentifier , TElement >
::IndexExists(ElementIdentifier id) const
{
  return (NumericTraits<ElementIdentifier>::IsNonnegative(id)
          && (id < this->VectorType::size()));
}



/**
 * Check if the given index is in range of the STL vector.  If it is not,
 * return false.  Otherwise, set the element through the pointer (if it isn't
 * NULL), and return true.
 */
template <typename TElementIdentifier, typename TElement>
bool
VectorContainer< TElementIdentifier , TElement >
::GetElementIfIndexExists(ElementIdentifier id, Element* element) const
{
  if (NumericTraits<ElementIdentifier>::IsNonnegative(id)
      && (id < this->VectorType::size()))
    {
    if(element)
      {
      *element = this->VectorType::operator[](id);
      }
    return true;
    }
  return false;
}


/**
 * Make sure that the index range of the STL vector is large enough to allow
 * the given index, expanding it if necessary.  The index will contain
 * the default element regardless of whether expansion occured.
 */
template <typename TElementIdentifier, typename TElement>
void
VectorContainer< TElementIdentifier , TElement >
::CreateIndex(ElementIdentifier id)
{
  if(id >= this->VectorType::size())
    {
    /**
     * The vector must be expanded to fit the
     * new id.
     */
    this->VectorType::resize(id+1);
    this->Modified();
    }
  else if(id > 0)
    {
    /**
     * No expansion was necessary.  Just overwrite the index's entry with
     * the default element.
     */
    this->VectorType::operator[](id) = Element();
    this->Modified();
    }
}


/**
 * It doesn't make sense to delete a vector index.
 * Instead, just overwrite the index with the default element.
 */
template <typename TElementIdentifier, typename TElement>
void
VectorContainer< TElementIdentifier , TElement >
::DeleteIndex(ElementIdentifier id)
{
  this->VectorType::operator[](id) = Element();
  this->Modified();
}


/**
 * Get a begin const iterator for the vector.
 */
template <typename TElementIdentifier, typename TElement>
typename VectorContainer< TElementIdentifier , TElement >::ConstIterator
VectorContainer< TElementIdentifier , TElement >
::Begin(void) const
{
  return ConstIterator(0, this->VectorType::begin());
}


/**
 * Get an end const iterator for the vector.
 */
template <typename TElementIdentifier, typename TElement>
typename VectorContainer< TElementIdentifier , TElement >::ConstIterator
VectorContainer< TElementIdentifier , TElement >
::End(void) const
{
  return ConstIterator(this->VectorType::size()-1, this->VectorType::end());
}


/**
 * Get a begin iterator for the vector.
 */
template <typename TElementIdentifier, typename TElement>
typename VectorContainer< TElementIdentifier , TElement >::Iterator
VectorContainer< TElementIdentifier , TElement >
::Begin(void) 
{
  return Iterator(0, this->VectorType::begin());
}


/**
 * Get an end iterator for the vector.
 */
template <typename TElementIdentifier, typename TElement>
typename VectorContainer< TElementIdentifier , TElement >::Iterator
VectorContainer< TElementIdentifier , TElement >
::End(void) 
{
  return Iterator(this->VectorType::size()-1, this->VectorType::end());
}


/**
 * Get the number of elements currently stored in the vector.
 */
template <typename TElementIdentifier, typename TElement>
unsigned long
VectorContainer< TElementIdentifier , TElement >
::Size(void) const
{
  return static_cast<unsigned long>( this->VectorType::size() );
}


/**
 * Clear the elements. The final size will be zero.
 */
template <typename TElementIdentifier, typename TElement>
void 
VectorContainer< TElementIdentifier , TElement >
::Initialize(void) 
{
  this->VectorType::clear();
}



/**
 * Tell the container to allocate enough memory to allow at least
 * as many elements as the size given to be stored.  This is NOT
 * guaranteed to actually allocate any memory, but is useful if the
 * implementation of the container allocates contiguous storage.
 */
template <typename TElementIdentifier, typename TElement>
void
VectorContainer< TElementIdentifier , TElement >
::Reserve(ElementIdentifier size)
{
  this->CreateIndex(size-1);
}


/**
 * Tell the container to try to minimize its memory usage for storage of
 * the current number of elements.  This is NOT guaranteed to decrease
 * memory usage.
 */
template <typename TElementIdentifier, typename TElement>
void
VectorContainer< TElementIdentifier , TElement >
::Squeeze(void)
{
}

} // end namespace itk

#endif
