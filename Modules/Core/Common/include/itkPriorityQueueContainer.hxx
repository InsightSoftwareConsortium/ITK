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

#ifndef itkPriorityQueueContainer_hxx
#define itkPriorityQueueContainer_hxx

#include "itkNumericTraits.h"
#include "itkPriorityQueueContainer.h"

namespace itk
{
// -----------------------------------------------------------------------------
// ElementWrapperInterface
// -----------------------------------------------------------------------------
template< typename TElement,
          typename TElementIdentifier >
ElementWrapperInterface< TElement, TElementIdentifier >::ElementWrapperInterface()
  {}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TElement,
          typename TElementIdentifier >
ElementWrapperInterface< TElement, TElementIdentifier >::~ElementWrapperInterface()
  {}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TElement,
          typename TElementIdentifier >
const
typename ElementWrapperInterface< TElement, TElementIdentifier >::ElementIdentifierType
ElementWrapperInterface< TElement, TElementIdentifier >::m_ElementNotFound
= NumericTraits< TElementIdentifier >::max();
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
// ElementWrapperPointerInterface
// -----------------------------------------------------------------------------
template< typename TElementWrapperPointer,
          typename TElementIdentifier >
ElementWrapperPointerInterface< TElementWrapperPointer, TElementIdentifier >::
ElementWrapperPointerInterface()
{}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TElementWrapperPointer,
          typename TElementIdentifier >
ElementWrapperPointerInterface< TElementWrapperPointer, TElementIdentifier >::
~ElementWrapperPointerInterface()
{}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TElementWrapperPointer,
          typename TElementIdentifier >
typename ElementWrapperPointerInterface< TElementWrapperPointer, TElementIdentifier >::
ElementIdentifierType
ElementWrapperPointerInterface< TElementWrapperPointer, TElementIdentifier >::
GetLocation(const ElementWrapperPointerType & element) const
{
  return ( ( *element ).GetLocation(*element) );
}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TElementWrapperPointer,
          typename TElementIdentifier >
void
ElementWrapperPointerInterface< TElementWrapperPointer, TElementIdentifier >::
SetLocation(ElementWrapperPointerType & element,
            const ElementIdentifierType & identifier)
{
  ( *element ).SetLocation(*element, identifier);
}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TElementWrapperPointer,
          typename TElementIdentifier >
bool
ElementWrapperPointerInterface< TElementWrapperPointer, TElementIdentifier >::
is_less(const ElementWrapperPointerType & element1,
        const ElementWrapperPointerType & element2) const
{
  return ( ( *element1 ).is_less( ( *element1 ), ( *element2 ) ) );
}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TElementWrapperPointer,
          typename TElementIdentifier >
bool
ElementWrapperPointerInterface< TElementWrapperPointer, TElementIdentifier >::
is_greater(const ElementWrapperPointerType & element1,
           const ElementWrapperPointerType & element2) const
{
  return ( ( *element1 ).is_greater( ( *element1 ), ( *element2 ) ) );
}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TElement,
          typename TElementIdentifier >
const TElementIdentifier
ElementWrapperPointerInterface< TElement, TElementIdentifier >::m_ElementNotFound
= NumericTraits< TElementIdentifier >::max();
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
// MinPriorityQueueElementWrapper
// -----------------------------------------------------------------------------
template<  typename TElement,  typename TElementPriority, typename TElementIdentifier > MinPriorityQueueElementWrapper< TElement, TElementPriority, TElementIdentifier >::
MinPriorityQueueElementWrapper() :
  m_Element(0),
  m_Priority(0),
  m_Location( Superclass::m_ElementNotFound )
{}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template<  typename TElement,  typename TElementPriority, typename TElementIdentifier > MinPriorityQueueElementWrapper< TElement, TElementPriority, TElementIdentifier >::
MinPriorityQueueElementWrapper(ElementType element, ElementPriorityType priority):
  m_Element(element), m_Priority(priority), m_Location( Superclass::m_ElementNotFound )
{}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TElement,  typename TElementPriority, typename TElementIdentifier > MinPriorityQueueElementWrapper< TElement, TElementPriority, TElementIdentifier >::
~MinPriorityQueueElementWrapper() {}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TElement,  typename TElementPriority, typename TElementIdentifier >
bool
MinPriorityQueueElementWrapper< TElement, TElementPriority, TElementIdentifier >::
operator>(const MinPriorityQueueElementWrapper & other) const
{
  return this->m_Priority > other.m_Priority;
}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TElement,  typename TElementPriority, typename TElementIdentifier >
bool
MinPriorityQueueElementWrapper< TElement, TElementPriority, TElementIdentifier >::
operator<(const MinPriorityQueueElementWrapper & other) const
{
  return this->m_Priority < other.m_Priority;
}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TElement,  typename TElementPriority, typename TElementIdentifier >
bool
MinPriorityQueueElementWrapper< TElement, TElementPriority, TElementIdentifier >::
operator==(const MinPriorityQueueElementWrapper & other) const
{
  return this->m_Priority == other.m_Priority;
}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TElement,  typename TElementPriority, typename TElementIdentifier >
typename
MinPriorityQueueElementWrapper< TElement, TElementPriority, TElementIdentifier >::
ElementIdentifierType
MinPriorityQueueElementWrapper< TElement, TElementPriority, TElementIdentifier >::
GetLocation(const MinPriorityQueueElementWrapper & element) const
{
  return element.m_Location;
}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TElement,  typename TElementPriority, typename TElementIdentifier >
void
MinPriorityQueueElementWrapper< TElement, TElementPriority, TElementIdentifier >::
SetLocation(MinPriorityQueueElementWrapper & element,
            const ElementIdentifierType & identifier)
{
  element.m_Location = identifier;
}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TElement,  typename TElementPriority, typename TElementIdentifier >
bool
MinPriorityQueueElementWrapper< TElement, TElementPriority, TElementIdentifier >::
is_less(const MinPriorityQueueElementWrapper & element1,
        const MinPriorityQueueElementWrapper & element2) const
{
  return ( element1 < element2 );
}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TElement,  typename TElementPriority, typename TElementIdentifier >
bool
MinPriorityQueueElementWrapper< TElement, TElementPriority, TElementIdentifier >::
is_greater(const MinPriorityQueueElementWrapper & element1,
           const MinPriorityQueueElementWrapper & element2) const
{
  return ( element1 > element2 );
}
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
// MaxPriorityQueueElementWrapper
// -----------------------------------------------------------------------------
template<  typename TElement,  typename TElementPriority, typename TElementIdentifier > MaxPriorityQueueElementWrapper< TElement, TElementPriority, TElementIdentifier >::
MaxPriorityQueueElementWrapper() : Superclass()
{}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template<  typename TElement,  typename TElementPriority, typename TElementIdentifier > MaxPriorityQueueElementWrapper< TElement, TElementPriority, TElementIdentifier >::
MaxPriorityQueueElementWrapper(ElementType element, ElementPriorityType priority):
  Superclass( element, priority )
{}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TElement,  typename TElementPriority, typename TElementIdentifier >
bool
MaxPriorityQueueElementWrapper< TElement, TElementPriority, TElementIdentifier >::
is_less(const MaxPriorityQueueElementWrapper & element1,
        const MaxPriorityQueueElementWrapper & element2) const
{
  return ( element1 > element2 );
}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TElement,  typename TElementPriority, typename TElementIdentifier >
bool
MaxPriorityQueueElementWrapper< TElement, TElementPriority, TElementIdentifier >::
is_less(const Superclass & element1,
        const Superclass & element2) const
{
  return Superclass::is_less(element1, element2);
}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TElement,  typename TElementPriority, typename TElementIdentifier >
bool
MaxPriorityQueueElementWrapper< TElement, TElementPriority, TElementIdentifier >::
is_greater(const MaxPriorityQueueElementWrapper & element1,
           const MaxPriorityQueueElementWrapper & element2) const
{
  return ( element1 < element2 );
}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TElement,  typename TElementPriority, typename TElementIdentifier >
bool
MaxPriorityQueueElementWrapper< TElement, TElementPriority, TElementIdentifier >::
is_greater(const Superclass & element1,
           const Superclass & element2) const
{
  return Superclass::is_greater(element1, element2);
}
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
// PriorityQueueContainer
// -----------------------------------------------------------------------------
template<
  typename TElementWrapper,
  typename TElementWrapperInterface,
  typename TElementPriority,
  typename TElementIdentifier
  >
const TElementIdentifier
PriorityQueueContainer< TElementWrapper, TElementWrapperInterface,
  TElementPriority, TElementIdentifier >::m_ElementNotFound
= NumericTraits< TElementIdentifier >::max();
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template<
  typename TElementWrapper,
  typename TElementWrapperInterface,
  typename TElementPriority,
  typename TElementIdentifier
  >
PriorityQueueContainer< TElementWrapper, TElementWrapperInterface,
  TElementPriority, TElementIdentifier >::
PriorityQueueContainer() : Superclass() {}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template<
  typename TElementWrapper,
  typename TElementWrapperInterface,
  typename TElementPriority,
  typename TElementIdentifier
  >
PriorityQueueContainer< TElementWrapper, TElementWrapperInterface,
  TElementPriority, TElementIdentifier >::
~PriorityQueueContainer()  {}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template<
  typename TElementWrapper,
  typename TElementWrapperInterface,
  typename TElementPriority,
  typename TElementIdentifier
  >
void
PriorityQueueContainer< TElementWrapper, TElementWrapperInterface,
  TElementPriority, TElementIdentifier >::
Clear()
{
  this->Initialize();
}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template<
  typename TElementWrapper,
  typename TElementWrapperInterface,
  typename TElementPriority,
  typename TElementIdentifier
  >
bool
PriorityQueueContainer< TElementWrapper, TElementWrapperInterface,
  TElementPriority, TElementIdentifier >::
Empty() const
{
  return ( this->empty() );
}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template<
  typename TElementWrapper,
  typename TElementWrapperInterface,
  typename TElementPriority,
  typename TElementIdentifier
  >
void
PriorityQueueContainer< TElementWrapper, TElementWrapperInterface,
  TElementPriority, TElementIdentifier >::
Push( ElementWrapperType element)
{
  this->push_back(element);
  this->UpdateUpTree( this->Size() - 1);
}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template<
  typename TElementWrapper,
  typename TElementWrapperInterface,
  typename TElementPriority,
  typename TElementIdentifier
  >
const typename
PriorityQueueContainer< TElementWrapper, TElementWrapperInterface,
  TElementPriority, TElementIdentifier >::
 ElementWrapperType &
PriorityQueueContainer< TElementWrapper, TElementWrapperInterface,
  TElementPriority, TElementIdentifier >::
Peek() const
{
  if( Empty() )
    {
    itkGenericExceptionMacro( <<"Empty PriorityQueueContainer" );
    }
  return ( GetElementAtLocation(0) );
}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template<
  typename TElementWrapper,
  typename TElementWrapperInterface,
  typename TElementPriority,
  typename TElementIdentifier
  >
void
PriorityQueueContainer< TElementWrapper, TElementWrapperInterface,
  TElementPriority, TElementIdentifier >::
Pop()
{
  m_Interface.SetLocation(this->front(), //GetElementAtLocation(0),
                          m_ElementNotFound);
  if ( this->Size() > 1 )
    {
    SetElementAtLocation( 0, this->back() );// GetElementAtLocation( this->Size() - 1 ) );
    this->pop_back();
    UpdateDownTree(0);
    }
  else
    {
    if ( this->Size() == 1 )
      {
      this->pop_back();
      }
    }
}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template<
  typename TElementWrapper,
  typename TElementWrapperInterface,
  typename TElementPriority,
  typename TElementIdentifier
  >
bool
PriorityQueueContainer< TElementWrapper, TElementWrapperInterface,
  TElementPriority, TElementIdentifier >::
Update( const ElementWrapperType& element)
{
  ElementIdentifierType location = m_Interface.GetLocation(element);

  if( location != m_ElementNotFound )
    {
    if( location >= this->Size() )
      {
      itkGenericExceptionMacro( <<" ElementWrapperType location is out of range" );
      }
    UpdateDownTree(location);
    UpdateUpTree(location);

    return true;
    }
  return false;
}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template<
  typename TElementWrapper,
  typename TElementWrapperInterface,
  typename TElementPriority,
  typename TElementIdentifier
  >
bool
PriorityQueueContainer< TElementWrapper, TElementWrapperInterface,
  TElementPriority, TElementIdentifier >::
DeleteElement(const  ElementWrapperType & element)
{
  ElementIdentifierType location = m_Interface.GetLocation(element);

  if( location != m_ElementNotFound )
    {
    // m_Interface.SetLocation(element, m_ElementNotFound);

    ElementIdentifierType tsize = this->Size();

    if( location >= tsize )
      {
      itkGenericExceptionMacro( <<" ElementWrapperType location is out of range" );
      }
    else
      {
      if ( location == tsize - 1 )
        {
        this->pop_back();
        }
      else
        {
        SetElementAtLocation( location, GetElementAtLocation( tsize - 1 ) );
        this->pop_back();
        UpdateDownTree(location);
        UpdateUpTree(location);
        }
      }
    return true;
    }
  return false;
}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template<
  typename TElementWrapper,
  typename TElementWrapperInterface,
  typename TElementPriority,
  typename TElementIdentifier
  >
void
PriorityQueueContainer< TElementWrapper, TElementWrapperInterface,
  TElementPriority, TElementIdentifier >::
UpdateUpTree(const ElementIdentifierType & identifier)
{
  if ( HasParent( identifier ) )
    {
    ElementIdentifierType id(identifier);
     ElementWrapperType           element = GetElementAtLocation(id);
    ElementIdentifierType parentIdentifier = GetParent(id);
     ElementWrapperType           parent_element = GetElementAtLocation(parentIdentifier);

    while ( HasParent( id )
            && m_Interface.is_less(element, parent_element) )
      {
      SetElementAtLocation(id, parent_element);
      id = parentIdentifier;
      if ( HasParent( id ) )
        {
        parentIdentifier = GetParent(id);
        parent_element = GetElementAtLocation(parentIdentifier);
        }
      }
    SetElementAtLocation(id, element);
    }
}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template<
  typename TElementWrapper,
  typename TElementWrapperInterface,
  typename TElementPriority,
  typename TElementIdentifier
  >
void
PriorityQueueContainer< TElementWrapper, TElementWrapperInterface,
  TElementPriority, TElementIdentifier >::
UpdateDownTree(const ElementIdentifierType & identifier)
{
  ElementIdentifierType id(identifier);
   ElementWrapperType           element = GetElementAtLocation(id);

  ElementIdentifierType queueSize = this->Size();

  while ( id < queueSize )
    {
    ElementIdentifierType childIdentifier = GetLeft(id);
    if ( childIdentifier >= queueSize )
      {
      break;
      }
    if ( ( childIdentifier + 1 < queueSize )
         && ( m_Interface.is_less( GetElementAtLocation(childIdentifier + 1),
                                   GetElementAtLocation(childIdentifier) ) ) )
      {
      ++childIdentifier;
      }
    ElementWrapperType temp = GetElementAtLocation(childIdentifier);
    if ( m_Interface.is_less(element, temp) )
      {
      break;
      }

    SetElementAtLocation(id, temp);
    id = childIdentifier;
    }

  SetElementAtLocation(id, element);
}
// -----------------------------------------------------------------------------
}

#endif // itkPriorityQueueContainer_hxx
