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
 * Just pass the indexing operator through to the STL map's version.
 */
template <typename TElementIdentifier, typename TElement>
itkMapContainer< TElementIdentifier , TElement >::Element&
itkMapContainer< TElementIdentifier , TElement >
::operator[](const ElementIdentifier& id)
{
  return Map::operator[](id);
}


/**
 * Check if the STL map has an entry corresponding to the given index.
 * The count will be either 1 or 0.
 */
template <typename TElementIdentifier, typename TElement>
bool
itkMapContainer< TElementIdentifier , TElement >
::IndexExists(const ElementIdentifier& id)
{
  return (this->Map::count(id) > 0);
}


/**
 * The STL map will create an entry for a given index through the indexing
 * operator.  Whether or not it is created, it will be assigned to the
 * default element.
 */
template <typename TElementIdentifier, typename TElement>
void
itkMapContainer< TElementIdentifier , TElement >
::CreateIndex(const ElementIdentifier& id)
{
  this->Map::operator[](id) = Element();
}


/**
 * Delete the entry in the STL map corresponding to the given identifier.
 * If the entry does not exist, nothing happens.
 */
template <typename TElementIdentifier, typename TElement>
void
itkMapContainer< TElementIdentifier , TElement >
::DeleteIndex(const ElementIdentifier& id)
{
  this->Map::erase(id);
}
