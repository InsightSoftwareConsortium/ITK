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
#ifndef itkPriorityQueueContainer_h
#define itkPriorityQueueContainer_h

#include "itkVectorContainer.h"
#include "itkIntTypes.h"
#include "itkNumericTraits.h"

#include <functional>
#include <queue>
#include <vector>

namespace itk
{
// first define a common interface all the wrapper will have to abide to
// this will let us define our own wrapper with different behavior.
// As an example we define below a wrapper for a min sorted or max sorted
// queue.
template< typename TElement,
          typename TElementIdentifier = IdentifierType >
class ITK_TEMPLATE_EXPORT ElementWrapperInterface
{
public:
  typedef TElement           ElementType;
  typedef TElementIdentifier ElementIdentifierType;

  static const ElementIdentifierType m_ElementNotFound;

  ElementWrapperInterface();
  virtual ~ElementWrapperInterface();

  virtual ElementIdentifierType GetLocation(const ElementType & element) const = 0;

  virtual void SetLocation(ElementType & element, const ElementIdentifierType & identifier) = 0;

  virtual bool is_less(const ElementType & element1,
                       const ElementType & element2) const = 0;

  virtual bool is_greater(const ElementType & element1,
                          const ElementType & element2) const = 0;
};
// ------------------------------------------------------------------------


// ------------------------------------------------------------------------
// If you want to manage the items outside the queue for example, if you don't
// want the queue to manage the items memory, then you can use this wrapper
// around pointers to items.  It follows the ElementWrapperInterface and thus
// can be used in the queue.
//
template< typename TElementWrapperPointer,
          typename TElementIdentifier = IdentifierType >
class ITK_TEMPLATE_EXPORT ElementWrapperPointerInterface
{
public:
  typedef TElementWrapperPointer ElementWrapperPointerType;
  typedef TElementIdentifier     ElementIdentifierType;

  static const ElementIdentifierType m_ElementNotFound;

  ElementWrapperPointerInterface();
  virtual ~ElementWrapperPointerInterface();

  TElementIdentifier GetLocation(const ElementWrapperPointerType & element) const;

  void SetLocation(ElementWrapperPointerType & element,
                   const ElementIdentifierType & identifier);

  virtual bool is_less(const ElementWrapperPointerType & element1,
               const ElementWrapperPointerType & element2) const;

  virtual bool is_greater(const ElementWrapperPointerType & element1,
                  const ElementWrapperPointerType & element2) const;
};
// ------------------------------------------------------------------------

// ------------------------------------------------------------------------
// To follow ITK rule, we template the ElementWrapperType priority and the element
// identifier type.
// For example, as we want to use this for decimation, the element will be some
// kind of cell or point pointer, the priority will be whatever you want it to
// be as long as you define the comparison operators, and the identifier will
// set according to the size of the vector you want to create.
//
// this implementation is used for min sorted priorityqueue
template<
  typename TElement,
  typename TElementPriority = double,
  typename TElementIdentifier = IdentifierType
  >
class ITK_TEMPLATE_EXPORT MinPriorityQueueElementWrapper:
  public ElementWrapperInterface<
    MinPriorityQueueElementWrapper< TElement,
                                    TElementPriority,
                                    TElementIdentifier >,
    TElementIdentifier
    >
{
public:
  typedef MinPriorityQueueElementWrapper< TElement,
    TElementPriority,
    TElementIdentifier >     Superclass;
  typedef TElement           ElementType;
  typedef TElementPriority   ElementPriorityType;
  typedef TElementIdentifier ElementIdentifierType;

  ElementType           m_Element;
  ElementPriorityType   m_Priority;
  ElementIdentifierType m_Location;

  MinPriorityQueueElementWrapper();

  MinPriorityQueueElementWrapper(ElementType element,
                                 ElementPriorityType priority);

  virtual ~MinPriorityQueueElementWrapper();

  bool operator>(const MinPriorityQueueElementWrapper & other) const;

  bool operator<(const MinPriorityQueueElementWrapper & other) const;

  bool operator==(const MinPriorityQueueElementWrapper & other) const;

  ElementIdentifierType GetLocation(const MinPriorityQueueElementWrapper & element) const;

  void SetLocation(MinPriorityQueueElementWrapper & element,
                   const ElementIdentifierType & identifier);

  // still virtual to be able to overload it in the Max flavor
  virtual bool is_less(const MinPriorityQueueElementWrapper & element1,
                       const MinPriorityQueueElementWrapper & element2) const;

  virtual bool is_greater(const MinPriorityQueueElementWrapper & element1,
                          const MinPriorityQueueElementWrapper & element2) const;

};
// ------------------------------------------------------------------------


// ------------------------------------------------------------------------
// this implementation is used for max sorted priorityqueue
// most of the job is already done, just need to overload the less
// and greater ops.
template<
  typename TElement,
  typename TElementPriority = double,
  typename TElementIdentifier = IdentifierType
  >
class ITK_TEMPLATE_EXPORT MaxPriorityQueueElementWrapper:
  public MinPriorityQueueElementWrapper< TElement,
                                         TElementPriority,
                                         TElementIdentifier >
{
public:
  typedef TElement           ElementType;
  typedef TElementPriority   ElementPriorityType;
  typedef TElementIdentifier ElementIdentifierType;

  typedef MinPriorityQueueElementWrapper< ElementType,
                                          ElementPriorityType,
                                          ElementIdentifierType > Superclass;
  MaxPriorityQueueElementWrapper();

  MaxPriorityQueueElementWrapper(ElementType element,
                                 ElementPriorityType priority);

  virtual ~MaxPriorityQueueElementWrapper() {}

  virtual bool is_less(const MaxPriorityQueueElementWrapper & element1,
               const MaxPriorityQueueElementWrapper & element2) const;

  virtual bool is_less(const Superclass & element1,
               const Superclass & element2) const;

  virtual bool is_greater(const MaxPriorityQueueElementWrapper & element1,
                  const MaxPriorityQueueElementWrapper & element2) const;

  virtual bool is_greater(const Superclass & element1,
                  const Superclass & element2) const;

};
// ------------------------------------------------------------------------


// ------------------------------------------------------------------------
// finally, implement the priority queue itself on top of an
// itk::VectorContainer
template<
  typename TElementWrapper,
  typename TElementWrapperInterface,
  typename TElementPriority = double,
  typename TElementIdentifier = IdentifierType
  >
class ITK_TEMPLATE_EXPORT PriorityQueueContainer:
  public VectorContainer< TElementIdentifier, TElementWrapper >
{
public:
  typedef PriorityQueueContainer                                 Self;
  typedef VectorContainer< TElementIdentifier, TElementWrapper > Superclass;
  typedef SmartPointer< Self >                                   Pointer;
  typedef SmartPointer< const Self >                             ConstPointer;

  typedef TElementIdentifier       ElementIdentifierType;
  typedef TElementWrapper          ElementWrapperType;
  typedef TElementWrapperInterface ElementInterfaceType;

  static const ElementIdentifierType m_ElementNotFound;

public:
  PriorityQueueContainer();
  ~PriorityQueueContainer() ITK_OVERRIDE;

  template< typename TInputIterator >
  PriorityQueueContainer(TInputIterator first, TInputIterator last):
    Superclass()
  {
    TInputIterator it = first;
    while( it != last )
      {
      this->Push( *it );
      ++it;
      }
  }

public:
  itkNewMacro(Self);
  itkTypeMacro(PriorityQueueContainer, VectorContainer);

  //void Reserve( ElementIdentifier NbOfElementsToStore )
  //{ this->Superclass->Reserve( NbOfElementsToStore ); }
  //void Squeeze( ) { this->Superclass->Squeeze( ); }
  void Clear();
  bool Empty() const;
  void Push(ElementWrapperType element);

  const ElementWrapperType & Peek() const;

  void Pop();

  /** Update element in container.
    \return true if the element is in the priority queue
    \return false else */
  bool Update( const ElementWrapperType& element);

  /** Delete element in the container.
    \return true if the element is in the priority queue
    \return false else */
  bool DeleteElement( const ElementWrapperType& element);

protected:

  // One instance of the interface to deal with the functions calls
  ElementInterfaceType m_Interface;

  inline ElementWrapperType & GetElementAtLocation( const ElementIdentifierType & identifier )
  {
    return this->operator[](identifier);
  }

  inline const ElementWrapperType & GetElementAtLocation(const ElementIdentifierType & identifier) const
  {
    return this->operator[](identifier);
  }

  inline void SetElementAtLocation(const ElementIdentifierType & identifier,
                                   ElementWrapperType& element)
  {
    this->operator[](identifier) = element;
    m_Interface.SetLocation(element, identifier);
  }

  inline ElementIdentifierType GetParent(const ElementIdentifierType & identifier) const
  {
    return ( ( identifier - 1 ) >> 1 );
  }

  inline ElementIdentifierType GetLeft(const ElementIdentifierType & identifier) const
  {
    return ( ( identifier << 1 ) + 1 );
  }

  inline ElementIdentifierType GetRight(const ElementIdentifierType & identifier) const
  {
    return ( ( identifier << 1 ) + 2 );
  }

  inline bool HasParent( const ElementIdentifierType& iId ) const
    {
    return ( iId > 0 );
    }

  void UpdateUpTree(const ElementIdentifierType & identifier);


  void UpdateDownTree(const ElementIdentifierType & identifier);

};
// ------------------------------------------------------------------------

}

#include "itkPriorityQueueContainer.hxx"
#endif
