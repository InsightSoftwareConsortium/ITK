/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMapContainer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
// #include "itkMapContainer.h"

/**
 *
 */
template <typename TElementIdentifier, typename TElement>
itkMapContainer< TElementIdentifier , TElement >::Pointer
itkMapContainer< TElementIdentifier , TElement >
::New(void)
{
  return new Self;
}


/**
 * Get the element at the specified index.  There is no check for
 * existence performed.
 */
template <typename TElementIdentifier, typename TElement>
itkMapContainer< TElementIdentifier , TElement >::Element
itkMapContainer< TElementIdentifier , TElement >
::GetElement(ElementIdentifier id) const
{
  return this->Map::find(id)->second;
}


/**
 * Set the given index value to the given element.  If the index doesn't
 * exist, it is automatically created.
 */
template <typename TElementIdentifier, typename TElement>
void
itkMapContainer< TElementIdentifier , TElement >
::SetElement(ElementIdentifier id, Element element)
{
  Map::operator[](id) = element;
  this->Modified();
}


/**
 * Check if the STL map has an entry corresponding to the given index.
 * The count will be either 1 or 0.
 */
template <typename TElementIdentifier, typename TElement>
bool
itkMapContainer< TElementIdentifier , TElement >
::IndexExists(ElementIdentifier id) const
{
  return (this->Map::count(id) > 0);
}


/**
 * If the given index doesn't exist in the map, return false.
 * Otherwise, set the element through the pointer (if it isn't null), and
 * return true.
 */
template <typename TElementIdentifier, typename TElement>
bool
itkMapContainer< TElementIdentifier , TElement >
::GetElementIfIndexExists(ElementIdentifier id, Element* element) const
{
  if(this->Map::count(id) > 0)
    {
    if(element != NULL)
      {
      *element = this->Map::find(id)->second;
      }
    return true;
    }
  return false;
}


/**
 * The STL map will create an entry for a given index through the indexing
 * operator.  Whether or not it is created, it will be assigned to the
 * default element.
 */
template <typename TElementIdentifier, typename TElement>
void
itkMapContainer< TElementIdentifier , TElement >
::CreateIndex(ElementIdentifier id)
{
  this->Map::operator[](id) = Element();
  this->Modified();
}


/**
 * Delete the entry in the STL map corresponding to the given identifier.
 * If the entry does not exist, nothing happens.
 */
template <typename TElementIdentifier, typename TElement>
void
itkMapContainer< TElementIdentifier , TElement >
::DeleteIndex(ElementIdentifier id)
{
  this->Map::erase(id);
  this->Modified();
}


/**
 * Pass through calls for begin/end iterator requests to underlying
 * container, and construct our own iterator type from the results.
 */
template <typename TElementIdentifier, typename TElement>
itkMapContainer< TElementIdentifier , TElement >::Iterator
itkMapContainer< TElementIdentifier , TElement >
::Begin(void)
{
  return Iterator(this->Map::begin());
}

template <typename TElementIdentifier, typename TElement>
itkMapContainer< TElementIdentifier , TElement >::ConstIterator
itkMapContainer< TElementIdentifier , TElement >
::Begin(void) const
{
  return ConstIterator(this->Map::begin());
}

template <typename TElementIdentifier, typename TElement>
itkMapContainer< TElementIdentifier , TElement >::Iterator
itkMapContainer< TElementIdentifier , TElement >
::End(void)
{
  return Iterator(this->Map::end());
}

template <typename TElementIdentifier, typename TElement>
itkMapContainer< TElementIdentifier , TElement >::ConstIterator
itkMapContainer< TElementIdentifier , TElement >
::End(void) const
{
  return ConstIterator(this->Map::end());
}

