/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAutoVectorContainer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
// #include "itkAutoVectorContainer.h"


/**
 *
 */
template <typename TElementIdentifier, typename TElement>
itkAutoVectorContainer< TElementIdentifier , TElement >::Pointer
itkAutoVectorContainer< TElementIdentifier , TElement >
::New(void)
{
  return new Self;
}


/**
 * Read the element from the given index.
 * It is assumed that the index exists.
 */
template <typename TElementIdentifier, typename TElement>
itkAutoVectorContainer< TElementIdentifier , TElement >::Element
itkAutoVectorContainer< TElementIdentifier , TElement >
::GetElement(ElementIdentifier id) const
{
  return this->Vector::operator[](id);
}


/**
 * This is an "auto" vector, so the underlying STL vector may need to be
 * expanded to allow the given index to be valid.
 *
 * Set the element value at the given index.
 * It is assumed that the index exists.
 */
template <typename TElementIdentifier, typename TElement>
void
itkAutoVectorContainer< TElementIdentifier , TElement >
::SetElement(ElementIdentifier id, Element element)
{
  if(id >= this->Vector::size()) CreateIndex(id);
  this->Vector::operator[](id) = element;
  this->Modified();
}


/**
 * Check if the index range of the STL vector is large enough to allow the
 * given index without expansion.
 */
template <typename TElementIdentifier, typename TElement>
bool
itkAutoVectorContainer< TElementIdentifier , TElement >
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
itkAutoVectorContainer< TElementIdentifier , TElement >
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
itkAutoVectorContainer< TElementIdentifier , TElement >
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
itkAutoVectorContainer< TElementIdentifier , TElement >
::DeleteIndex(ElementIdentifier id)
{
  this->Vector::operator[](id) = Element();
  this->Modified();
}

