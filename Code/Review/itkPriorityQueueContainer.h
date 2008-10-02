#ifndef __itkPriorityQueueContainer_h
#define __itkPriorityQueueContainer_h

#include <itkObject.h>
#include <itkObjectFactory.h>
#include <itkVectorContainer.h>
#include <functional>
#include <queue>
#include <assert.h>
#include <vector>

namespace itk
{

// first define a common interface all the wrapper will have to abide to
// this will let us define our own wrapper with different behavior.
// As an exemple we define below a wrapper for a min sorted or max sorted
// queue.
template< typename TElement, typename TElementIdentifier >
class ElementWrapperInterface
{
public:
  typedef TElement ElementType;
  typedef TElementIdentifier ElementIdentifierType;
  
  ElementWrapperInterface() {}
  virtual ~ElementWrapperInterface() {}
  virtual TElementIdentifier GetLocation( const ElementType& element) = 0;
  virtual void SetLocation( ElementType& element,
    const ElementIdentifierType& identifier) = 0;
  virtual bool is_less( const ElementType& element1,
    const ElementType& element2 ) = 0;
  virtual bool is_greater( const ElementType& element1,
    const ElementType& element2 ) = 0;
};

// if you want to manage the items outside the queue
// i.e. you don;t want the queue to manage the items memory
// you can use this wrapper around pointers to items.
// it follows the ElementWrapperInterface and thus
// can be used in the queue.
template< typename TElementWrapperPointer, typename TElementIdentifier = int >
class ElementWrapperPointerInterface
{
public:
  typedef TElementWrapperPointer ElementWrapperPointerType;
  typedef TElementIdentifier ElementIdentifierType;
  
  ElementWrapperPointerInterface( ) { }
  ~ElementWrapperPointerInterface( ) { }

  TElementIdentifier GetLocation( const ElementWrapperPointerType& element)
    {
    return( (*element).GetLocation(*element) );
    }
  void SetLocation( ElementWrapperPointerType element,
    const ElementIdentifierType& identifier)
    {
    (*element).SetLocation(*element, identifier);
    }
  bool is_less( const ElementWrapperPointerType& element1,
    const ElementWrapperPointerType& element2 )
    {
    return( (*element1).is_less( (*element1), (*element2) ) );
    }
  bool is_greater( const ElementWrapperPointerType& element1,
    const ElementWrapperPointerType& element2 )
    {
    return( (*element1).is_greater( (*element1), (*element2) ) );
    }
};


// To follow ITK rule, we template the Element priority and the element
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
  typename TElementIdentifier = int
 >
class MinPriorityQueueElementWrapper :
    public ElementWrapperInterface<
    MinPriorityQueueElementWrapper< TElement,
                                    TElementPriority,
                                    TElementIdentifier >,
    TElementIdentifier
    >
{
public:
  typedef TElement ElementType;
  typedef TElementPriority ElementPriorityType;
  typedef TElementIdentifier ElementIdentifierType;
  
  ElementType           m_Element;
  ElementPriorityType   m_Priority;
  ElementIdentifierType m_Location;

  MinPriorityQueueElementWrapper( ):
    m_Element(),
    m_Priority( 0 ),
    m_Location( -1 )
  {}

  MinPriorityQueueElementWrapper( ElementType element,
    ElementPriorityType priority ) :
    m_Element( element ),
    m_Priority( priority ),
    m_Location( -1 )
  {}

  virtual ~MinPriorityQueueElementWrapper() {}

  bool operator>( const MinPriorityQueueElementWrapper& other) const
    {
    return this->m_Priority > other.m_Priority;
    }

  bool operator<( const MinPriorityQueueElementWrapper& other) const
    {
    return this->m_Priority < other.m_Priority;
    }

  bool operator==( const MinPriorityQueueElementWrapper& other) const
    {
    return this->m_Priority == other.m_Priority;
    }

  ElementIdentifierType GetLocation(
    const MinPriorityQueueElementWrapper& element)
    {
    return element.m_Location;
    }

  void SetLocation( MinPriorityQueueElementWrapper& element,
    const TElementIdentifier& identifier)
    {
    element.m_Location = identifier;
    }

  // still virtual to be able to overload it in the Max flavor
  virtual bool is_less( const MinPriorityQueueElementWrapper& element1,
                     const MinPriorityQueueElementWrapper& element2 )
    {
    return( element1 < element2 );
    }

  virtual bool is_greater( const MinPriorityQueueElementWrapper& element1,
                        const MinPriorityQueueElementWrapper& element2 )
    {
    return( element1 > element2 );
    }

};

// this implementation is used for max sorted priorityqueue
// most of the job is already done, just need to overload the less
// and greater ops.
template<
  typename TElement,
  typename TElementPriority = double,
  typename TElementIdentifier = int
 >
class MaxPriorityQueueElementWrapper :
  public MinPriorityQueueElementWrapper< TElement,
                                         TElementPriority,
                                         TElementIdentifier >
{
public:
  typedef TElement ElementType;
  typedef TElementPriority ElementPriorityType;
  typedef TElementIdentifier ElementIdentifierType;
  
  MaxPriorityQueueElementWrapper( ) :
    MinPriorityQueueElementWrapper< ElementType,
                                    ElementPriorityType,
                                    ElementIdentifierType >( )
  {}

  MaxPriorityQueueElementWrapper( ElementType element,
    ElementPriorityType priority ) :
    MinPriorityQueueElementWrapper< ElementType,
                                    ElementPriorityType,
                                    ElementIdentifierType >( element, priority )
  {}
  
  virtual ~MaxPriorityQueueElementWrapper() {}

  bool is_less( const MaxPriorityQueueElementWrapper& element1,
             const MaxPriorityQueueElementWrapper& element2 )
    {
    return( element1 > element2 );
    }

  bool is_greater( const MaxPriorityQueueElementWrapper& element1,
                const MaxPriorityQueueElementWrapper& element2 )
    {
    return( element1 < element2 );
    }
};



// finally, implement the priority queue itself on top of an itk::VectorContainer
template<
  typename TElementWrapper,
  typename TElementWrapperInterface,
  typename TElementPriority = double,
  typename TElementIdentifier = int
  >
class PriorityQueueContainer :
  public VectorContainer< TElementIdentifier, TElementWrapper >
{

public:
  typedef PriorityQueueContainer                                Self;
  typedef VectorContainer< TElementIdentifier, TElementWrapper > Superclass;
  typedef SmartPointer<Self>                                    Pointer;
  typedef SmartPointer<const Self>                              ConstPointer;

  typedef TElementIdentifier          ElementIdentifier;
  typedef TElementWrapper             Element;
  typedef TElementWrapperInterface    ElementInterface;

private:
  typedef Superclass                                    VectorType;
 // typedef typename VectorType::size_type              size_type;
//  typedef typename VectorType::VectorIterator         VectorIterator;
//  typedef typename VectorType::VectorConstIterator    VectorConstIterator;

public:
 PriorityQueueContainer():
   VectorType() {}
 //PriorityQueueContainer(size_type n):
 //   VectorType(n) {}
 //PriorityQueueContainer(size_type n, const Element& x):
 //  VectorType(n, x) {}
 PriorityQueueContainer(const Self& r):
   VectorType(r) {}
 template <typename InputIterator>
 PriorityQueueContainer(InputIterator first, InputIterator last):
   VectorType(first, last) {}

public:
  itkNewMacro(Self);
  itkTypeMacro(PriorityQueueContainer, VectorContainer);

  //void Reserve( ElementIdentifier NbOfElementsToStore )
  //{ this->Superclass->Reserve( NbOfElementsToStore ); }
  //void Squeeze( ) { this->Superclass->Squeeze( ); }
  void Clear( ) { this->Initialize( );  } // do not release memory
  bool Empty( ) const { return( this->empty() ); }
  void Push( Element element )
    {
    this->push_back( element );
    this->UpdateUpTree( static_cast< ElementIdentifier >( this->Size( ) ) - 1 );
    }

  Element Peek( )
    {
    assert( !Empty( ) );
    return( GetElementAtLocation( 0 ) );
    }

  void Pop( )
    {
    m_Interface.SetLocation( GetElementAtLocation( 0 ), -1 );
    if( this->Size( ) > 1 )
      {
      SetElementAtLocation( 0,
        GetElementAtLocation(
          static_cast< ElementIdentifier >( this->Size( ) - 1 ) ) );
      this->resize( this->Size( ) - 1 );
      UpdateDownTree( 0 );
      }
    else
      {
      this->resize( this->Size( ) - 1 );
      }
    }

  void Update( Element element )
    {
    ElementIdentifier location = m_Interface.GetLocation( element );
    assert( location != -1 );
    assert( location < static_cast< ElementIdentifier >( this->Size( ) ) );
    UpdateDownTree( location );
    UpdateUpTree( location );
    }

  void DeleteElement( Element element )
    {
    ElementIdentifier location = m_Interface.GetLocation( element );
    m_Interface.SetLocation( element, -1);

    assert( location != -1 );
    assert( location < static_cast< ElementIdentifier >( this->Size( ) ) );

    if ( location == static_cast< ElementIdentifier >( this->Size( ) ) - 1 )
      {
      this->resize( this->Size( ) - 1 );
      }
    else
      {
      SetElementAtLocation( location,
        GetElementAtLocation( this->Size( ) - 1 ) );
      this->resize( this->Size( ) - 1 );
      UpdateDownTree( location );
      UpdateUpTree( location );
      }
    }

protected:

  // One instance of the interface to deal with the functions calls
  ElementInterface m_Interface;

  inline Element& GetElementAtLocation( const ElementIdentifier& identifier )
    {
    return this->operator[]( identifier );
    }

  inline void SetElementAtLocation( const ElementIdentifier& identifier,
    Element element )
    {
    this->operator[]( identifier ) = element;
    m_Interface.SetLocation( element, identifier );
    }

  inline ElementIdentifier GetParent( const ElementIdentifier& identifier ) const
    {
    return( (identifier - 1) >> 1 );
    }

  inline ElementIdentifier GetLeft( const ElementIdentifier& identifier ) const
    {
    return( (identifier << 1) + 1 );
    }

  inline ElementIdentifier GetRight( const ElementIdentifier& identifier ) const
    {
    return( (identifier << 1) + 2 );
    }

  void UpdateUpTree( const ElementIdentifier& identifier )
    {
    ElementIdentifier id( identifier );
    Element element = GetElementAtLocation( id );

    if( id > 0 )
      {
      ElementIdentifier parentIdentifier = GetParent( id );
      Element parent_element = GetElementAtLocation( parentIdentifier );

      while( ( id > 0 ) && m_Interface.is_less( element, parent_element ) )
        {
        parent_element = GetElementAtLocation( parentIdentifier );
        SetElementAtLocation( id,
          GetElementAtLocation( parentIdentifier ) );
        id = parentIdentifier;
        parentIdentifier = GetParent( id );
        }
      }

    SetElementAtLocation( id, element );
    }

  void UpdateDownTree( const ElementIdentifier& identifier )
    {
    ElementIdentifier id( identifier );
    ElementIdentifier childIdentifier = GetLeft( id );
    ElementIdentifier queueSize =
      static_cast< ElementIdentifier >( this->Size( ) );

    Element element = GetElementAtLocation( id );
    Element temp;

    while( ( id < queueSize ) && ( childIdentifier < queueSize ) )
      {
      if( ( childIdentifier + 1 < queueSize ) &&
          ( m_Interface.is_less( GetElementAtLocation( childIdentifier + 1 ),
                              GetElementAtLocation( childIdentifier ) ) ) )
        {
        ++childIdentifier;
        }
      temp = GetElementAtLocation( childIdentifier );
      if( m_Interface.is_less( element, temp ) )
        {
        break;
        }

      SetElementAtLocation( id, temp );
      id = childIdentifier;
      childIdentifier = GetLeft( id );
      }

    SetElementAtLocation( id, element );
    }
};

}

#endif

