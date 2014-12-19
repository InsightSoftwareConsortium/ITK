/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkMapContainer_hxx
#define itkMapContainer_hxx
#include "itkMapContainer.h"

namespace itk
{
/**
 * Get a reference to the element at the given index.
 * If the index does not exist, it is created automatically.
 *
 * It is assumed that the value of the element is modified through the
 * reference.
 */
template< typename TElementIdentifier, typename TElement >
typename MapContainer< TElementIdentifier, TElement >::Element &
MapContainer< TElementIdentifier, TElement >
::ElementAt(ElementIdentifier id)
{
  this->Modified();
  return this->MapType::operator[](id);
}

/**
 * Get a reference to the element at the given index.
 *
 */
template< typename TElementIdentifier, typename TElement >
const typename MapContainer< TElementIdentifier, TElement >::Element &
MapContainer< TElementIdentifier, TElement >
::ElementAt(ElementIdentifier id) const
{
  return this->MapType::find(id)->second;
}

/**
 * Get a reference to the element at the given index.
 * If the index does not exist, it is created automatically.
 *
 * It is assumed that the value of the element is modified through the
 * reference.
 */
template< typename TElementIdentifier, typename TElement >
typename MapContainer< TElementIdentifier, TElement >::Element &
MapContainer< TElementIdentifier, TElement >
::CreateElementAt(ElementIdentifier id)
{
  this->Modified();
  return this->MapType::operator[](id);
}

/**
 * Get the element at the specified index.  There is no check for
 * existence performed.
 */
template< typename TElementIdentifier, typename TElement >
typename MapContainer< TElementIdentifier, TElement >::Element
MapContainer< TElementIdentifier, TElement >
::GetElement(ElementIdentifier id) const
{
  return this->MapType::find(id)->second;
}

/**
 * Set the given index value to the given element.  If the index doesn't
 * exist, it is automatically created.
 */
template< typename TElementIdentifier, typename TElement >
void
MapContainer< TElementIdentifier, TElement >
::SetElement(ElementIdentifier id, Element element)
{
  MapType::operator[](id) = element;
  this->Modified();
}

/**
 * Set the given index value to the given element.  If the index doesn't
 * exist, it is automatically created.
 */
template< typename TElementIdentifier, typename TElement >
void
MapContainer< TElementIdentifier, TElement >
::InsertElement(ElementIdentifier id, Element element)
{
  this->MapType::operator[](id) = element;
  this->Modified();
}

/**
 * Check if the STL map has an entry corresponding to the given index.
 * The count will be either 1 or 0.
 */
template< typename TElementIdentifier, typename TElement >
bool
MapContainer< TElementIdentifier, TElement >
::IndexExists(ElementIdentifier id) const
{
  return ( this->MapType::find(id) != this->MapType::end() );
}

/**
 * If the given index doesn't exist in the map, return false.
 * Otherwise, set the element through the pointer (if it isn't null), and
 * return true.
 */
template< typename TElementIdentifier, typename TElement >
bool
MapContainer< TElementIdentifier, TElement >
::GetElementIfIndexExists(ElementIdentifier id, Element *element) const
{
  MapConstIterator it = this->MapType::find(id);
  if( it != this->MapType::end() )
    {
    if( element )
      {
      *element = it->second;
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
template< typename TElementIdentifier, typename TElement >
void
MapContainer< TElementIdentifier, TElement >
::CreateIndex(ElementIdentifier id)
{
  this->MapType::operator[](id) = Element();
  this->Modified();
}

/**
 * Delete the entry in the STL map corresponding to the given identifier.
 * If the entry does not exist, nothing happens.
 */
template< typename TElementIdentifier, typename TElement >
void
MapContainer< TElementIdentifier, TElement >
::DeleteIndex(ElementIdentifier id)
{
  this->MapType::erase(id);
  this->Modified();
}

/**
 * Get a begin const iterator for the map.
 */
template< typename TElementIdentifier, typename TElement >
typename MapContainer< TElementIdentifier, TElement >::ConstIterator
MapContainer< TElementIdentifier, TElement >
::Begin(void) const
{
  return ConstIterator( this->MapType::begin() );
}

/**
 * Get an end const iterator for the map.
 */
template< typename TElementIdentifier, typename TElement >
typename MapContainer< TElementIdentifier, TElement >::ConstIterator
MapContainer< TElementIdentifier, TElement >
::End(void) const
{
  return ConstIterator( this->MapType::end() );
}

/**
 * Get a begin const iterator for the map.
 */
template< typename TElementIdentifier, typename TElement >
typename MapContainer< TElementIdentifier, TElement >::Iterator
MapContainer< TElementIdentifier, TElement >
::Begin(void)
{
  return Iterator( this->MapType::begin() );
}

/**
 * Get an end const iterator for the map.
 */
template< typename TElementIdentifier, typename TElement >
typename MapContainer< TElementIdentifier, TElement >::Iterator
MapContainer< TElementIdentifier, TElement >
::End(void)
{
  return Iterator( this->MapType::end() );
}

/**
 * Get the number of elements currently stored in the map.
 */
template< typename TElementIdentifier, typename TElement >
typename MapContainer< TElementIdentifier, TElement >::ElementIdentifier
MapContainer< TElementIdentifier, TElement >
::Size(void) const
{
  return static_cast< ElementIdentifier >( this->MapType::size() );
}

/**
 * Tell the container to allocate enough memory to allow at least as many
 * elements as the size given to be stored.  This is NOT guaranteed to actually
 * allocate any memory, but is useful if the implementation of the container
 * allocates contiguous storage.
 *
 * \warning In the particular case of the MapContainer, this method only
 * allocates memory for one element, not for the ones with index between zero
 * and "size".
 */
template< typename TElementIdentifier, typename TElement >
void
MapContainer< TElementIdentifier, TElement >
::Reserve(ElementIdentifier sz)
{
  ElementIdentifier curSize = this->Size();

  while ( curSize < sz )
    {
    this->CreateIndex(curSize);
    curSize = this->Size();
    }
}

/**
 * Tell the container to try to minimize its memory usage for storage of
 * the current number of elements.  This is NOT guaranteed to decrease
 * memory usage.
 */
template< typename TElementIdentifier, typename TElement >
void
MapContainer< TElementIdentifier, TElement >
::Squeeze(void)
{}

/**
 * Tell the container to release any memory it may have allocated and
 * return itself to its initial state.
 */
template< typename TElementIdentifier, typename TElement >
void
MapContainer< TElementIdentifier, TElement >
::Initialize(void)
{
  this->MapType::clear();
}
} // end namespace itk

#endif
