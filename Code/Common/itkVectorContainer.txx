/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorContainer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
// #include "itkVectorContainer.h"

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
VectorContainer< TElementIdentifier , TElement >::Element&
VectorContainer< TElementIdentifier , TElement >
::ElementAt(ElementIdentifier id)
{
  this->Modified();
  return this->Vector::operator[](id);
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
VectorContainer< TElementIdentifier , TElement >::Element&
VectorContainer< TElementIdentifier , TElement >
::CreateElementAt(ElementIdentifier id)
{
  if(id >= this->Vector::size())
    {
    this->CreateIndex(id);
    }
  this->Modified();
  return this->Vector::operator[](id);
}


/**
 * Read the element from the given index.
 * It is assumed that the index exists.
 */
template <typename TElementIdentifier, typename TElement>
VectorContainer< TElementIdentifier , TElement >::Element
VectorContainer< TElementIdentifier , TElement >
::GetElement(ElementIdentifier id) const
{
  return this->Vector::operator[](id);
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
  this->Vector::operator[](id) = element;
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
  if(id >= this->Vector::size())
    {
    this->CreateIndex(id);
    }
  this->Vector::operator[](id) = element;
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
  return ((id >= 0) && (id < this->Vector::size()));
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
  if((id >= 0) && (id < this->Vector::size()))
    {
    if(element != NULL)
      {
      *element = this->Vector::operator[](id);
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
  if(id >= this->Vector::size())
    {
    /**
     * The vector must be expanded.  If doubling in size is enough to
     * allow the new index, do so.  Otherwise, expand just enough to
     * allow the new index.
     */
    if((id+1) < (2*this->Vector::size()))
      this->Vector::resize(2*this->Vector::size());
    else
      this->Vector::resize(id+1);
    this->Modified();
    }
  else if(id >= 0)
    {
    /**
     * No expansion was necessary.  Just overwrite the index's entry with
     * the default element.
     */
    this->Vector::operator[](id) = Element();
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
  this->Vector::operator[](id) = Element();
  this->Modified();
}


/**
 * Pass through calls for begin/end iterator requests to underlying
 * container and use the result to construct our own iterator.
 */
template <typename TElementIdentifier, typename TElement>
VectorContainer< TElementIdentifier , TElement >::ConstIterator
VectorContainer< TElementIdentifier , TElement >
::Begin(void) const
{
  return ConstIterator(0, this->Vector::begin());
}

template <typename TElementIdentifier, typename TElement>
VectorContainer< TElementIdentifier , TElement >::ConstIterator
VectorContainer< TElementIdentifier , TElement >
::End(void) const
{
  return ConstIterator(this->Vector::size()-1, this->Vector::end());
}

} // namespace itk
