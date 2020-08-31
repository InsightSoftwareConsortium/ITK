/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkVectorContainer_h
#define itkVectorContainer_h

#include "itkObject.h"
#include "itkObjectFactory.h"

#include <utility>
#include <vector>

namespace itk
{
/** \class VectorContainer
 *  \brief Define a front-end to the STL "vector" container that conforms to the
 *         IndexedContainerInterface.
 *
 * This is a full-fleged Object, so
 * there is modification time, debug, and reference count information.
 *
 * \tparam TElementIdentifier An INTEGRAL type for use in indexing the vector.
 *
 * \tparam TElement The element type stored in the container.
 *
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/IterateOnAVectorContainer,Iterate On A Vector Container}
 * \endsphinx
 */
template <typename TElementIdentifier, typename TElement>
class ITK_TEMPLATE_EXPORT VectorContainer
  : public Object
  , private std::vector<TElement>
{
public:
  /** Standard class type aliases. */
  using Self = VectorContainer;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Save the template parameters. */
  using ElementIdentifier = TElementIdentifier;
  using Element = TElement;

private:
  /** Quick access to the STL vector type that was inherited. */
  using VectorType = std::vector<Element>;
  using size_type = typename VectorType::size_type;
  using VectorIterator = typename VectorType::iterator;
  using VectorConstIterator = typename VectorType::const_iterator;

protected:
  /** Provide pass-through constructors corresponding to all the STL
   * vector constructors.  These are for internal use only since this is also
   * an Object which must be constructed through the "New()" routine. */
  VectorContainer()
    : Object()
    , VectorType()
  {}
  VectorContainer(size_type n)
    : Object()
    , VectorType(n)
  {}
  VectorContainer(size_type n, const Element & x)
    : Object()
    , VectorType(n, x)
  {}
  VectorContainer(const Self & r)
    : Object()
    , VectorType(r.CastToSTLConstContainer())
  {}
  template <typename TInputIterator>
  VectorContainer(TInputIterator first, TInputIterator last)
    : Object()
    , VectorType(first, last)
  {}

public:
  /** This type is provided to Adapt this container as an STL container */
  using STLContainerType = VectorType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Standard part of every itk Object. */
  itkTypeMacro(VectorContainer, Object);

  /** Convenient type alias for the iterator and const iterator. */
  class Iterator;
  class ConstIterator;

  /** Cast the container to a STL container type */
  STLContainerType &
  CastToSTLContainer() ITK_NOEXCEPT
  {
    return *this;
  }

  /** Cast the container to a const STL container type */
  const STLContainerType &
  CastToSTLConstContainer() const ITK_NOEXCEPT
  {
    return *this;
  }

  using STLContainerType::begin;
  using STLContainerType::end;
  using STLContainerType::rbegin;
  using STLContainerType::rend;
  using STLContainerType::cbegin;
  using STLContainerType::cend;
  using STLContainerType::crbegin;
  using STLContainerType::crend;

  using STLContainerType::size;
  using STLContainerType::max_size;
  using STLContainerType::resize;
  using STLContainerType::capacity;
  using STLContainerType::empty;
  using STLContainerType::reserve;
  using STLContainerType::shrink_to_fit;

  using STLContainerType::operator[];
  using STLContainerType::at;
  using STLContainerType::front;
  using STLContainerType::back;

  using STLContainerType::assign;
  using STLContainerType::push_back;
  using STLContainerType::pop_back;
  using STLContainerType::insert;
  using STLContainerType::erase;
  using STLContainerType::swap;
  using STLContainerType::clear;

  using STLContainerType::get_allocator;

  using typename STLContainerType::reference;
  using typename STLContainerType::const_reference;
  using typename STLContainerType::iterator;
  using typename STLContainerType::const_iterator;
  // already declared before
  // using STLContainerType::size_type;
  using typename STLContainerType::difference_type;
  using typename STLContainerType::value_type;
  using typename STLContainerType::allocator_type;
  using typename STLContainerType::pointer;
  using typename STLContainerType::const_pointer;
  using typename STLContainerType::reverse_iterator;
  using typename STLContainerType::const_reverse_iterator;

  /** Friends to this class. */
  friend class Iterator;
  friend class ConstIterator;

  /** \class Iterator
   * Simulate STL-map style iteration where dereferencing the iterator
   * gives access to both the index and the value.
   * \ingroup ITKCommon
   */
  class Iterator
  {
  public:
    using iterator_category = typename VectorIterator::iterator_category;
    using value_type = typename VectorIterator::value_type;
    using difference_type = typename VectorIterator::difference_type;
    using pointer = typename VectorIterator::pointer;
    using reference = typename VectorIterator::reference;

    Iterator()
      : m_Pos(0)
    {}
    Iterator(size_type d, const VectorIterator & i)
      : m_Pos(d)
      , m_Iter(i)
    {}
    Iterator & operator*() { return *this; }
    Iterator * operator->() { return this; }
    Iterator &
    operator++()
    {
      ++m_Pos;
      ++m_Iter;
      return *this;
    }
    Iterator
    operator++(int)
    {
      Iterator temp(*this);
      ++m_Pos;
      ++m_Iter;
      return temp;
    }
    Iterator &
    operator--()
    {
      --m_Pos;
      --m_Iter;
      return *this;
    }
    Iterator
    operator--(int)
    {
      Iterator temp(*this);
      --m_Pos;
      --m_Iter;
      return temp;
    }

    difference_type
    operator-(const Iterator & r) const
    {
      return static_cast<difference_type>(this->m_Pos) - static_cast<difference_type>(r.m_Pos);
    }

    bool
    operator==(const Iterator & r) const
    {
      return m_Iter == r.m_Iter;
    }
    bool
    operator!=(const Iterator & r) const
    {
      return m_Iter != r.m_Iter;
    }
    bool
    operator==(const ConstIterator & r) const
    {
      return m_Iter == r.m_Iter;
    }
    bool
    operator!=(const ConstIterator & r) const
    {
      return m_Iter != r.m_Iter;
    }
    bool
    operator<(const Iterator & r) const
    {
      return (this->operator-(r)) < 0;
    }
    bool
    operator>(const Iterator & r) const
    {
      return (r < *this);
    }
    bool
    operator>=(const Iterator & r) const
    {
      return !(*this < r);
    }
    bool
    operator<=(const Iterator & r) const
    {
      return !(*this < r);
    }

    Iterator &
    operator+=(difference_type n)
    {
      m_Pos += n;
      m_Iter += n;
      return *this;
    };

    /** Get the index into the VectorContainer associated with this iterator.
     */
    ElementIdentifier
    Index() const
    {
      return static_cast<ElementIdentifier>(m_Pos);
    }

    /** Get the value at this iterator's location in the VectorContainer.   */
    reference
    Value() const
    {
      return *m_Iter;
    }

  private:
    size_type      m_Pos;
    VectorIterator m_Iter;
    friend class ConstIterator;
  };

  /** \class ConstIterator
   * Simulate STL-map style const iteration where dereferencing the iterator
   * gives read access to both the index and the value.
   * \ingroup ITKCommon
   */
  class ConstIterator
  {
  public:
    using iterator_category = typename VectorConstIterator::iterator_category;
    using value_type = typename VectorConstIterator::value_type;
    using difference_type = typename VectorConstIterator::difference_type;
    using pointer = typename VectorConstIterator::pointer;
    using reference = typename VectorConstIterator::reference;

    ConstIterator()
      : m_Pos(0)
    {}
    ConstIterator(size_type d, const VectorConstIterator & i)
      : m_Pos(d)
      , m_Iter(i)
    {}
    ConstIterator(const Iterator & r)
      : m_Pos(r.m_Pos)
      , m_Iter(r.m_Iter)
    {}
    ConstIterator & operator*() { return *this; }
    ConstIterator * operator->() { return this; }
    ConstIterator &
    operator++()
    {
      ++m_Pos;
      ++m_Iter;
      return *this;
    }
    ConstIterator
    operator++(int)
    {
      ConstIterator temp(*this);
      ++m_Pos;
      ++m_Iter;
      return temp;
    }
    ConstIterator &
    operator--()
    {
      --m_Pos;
      --m_Iter;
      return *this;
    }
    ConstIterator
    operator--(int)
    {
      ConstIterator temp(*this);
      --m_Pos;
      --m_Iter;
      return temp;
    }
    ConstIterator &
    operator=(const Iterator & r)
    {
      m_Pos = r.m_Pos;
      m_Iter = r.m_Iter;
      return *this;
    }
    ConstIterator &
    operator+=(difference_type n)
    {
      m_Pos += n;
      m_Iter += n;
      return *this;
    };

    difference_type
    operator-(const ConstIterator & r) const
    {
      return static_cast<difference_type>(m_Pos) - static_cast<difference_type>(r.m_Pos);
    }

    bool
    operator==(const Iterator & r) const
    {
      return m_Iter == r.m_Iter;
    }
    bool
    operator!=(const Iterator & r) const
    {
      return m_Iter != r.m_Iter;
    }
    bool
    operator==(const ConstIterator & r) const
    {
      return m_Iter == r.m_Iter;
    }
    bool
    operator!=(const ConstIterator & r) const
    {
      return m_Iter != r.m_Iter;
    }
    bool
    operator<(const ConstIterator & r) const
    {
      return (this->operator-(r) < 0);
    }
    bool
    operator>(const ConstIterator & r) const
    {
      return (r < *this);
    }
    bool
    operator<=(const ConstIterator & r) const
    {
      return !(*this > r);
    }
    bool
    operator>=(const ConstIterator & r) const
    {
      return !(*this < r);
    }


    /** Get the index into the VectorContainer associated with this iterator.
     */
    ElementIdentifier
    Index() const
    {
      return static_cast<ElementIdentifier>(m_Pos);
    }
    /** Get the value at this iterator's location in the VectorContainer.   */
    const_reference
    Value() const
    {
      return *m_Iter;
    }

  private:
    size_type           m_Pos;
    VectorConstIterator m_Iter;
    friend class Iterator;
  };

  /* Declare the public interface routines. */

  /**
   * Get a reference to the element at the given index.
   * It is assumed that the index exists, and it will not automatically
   * be created.
   *
   * It is assumed that the value of the element is modified through the
   * reference.
   */
  reference ElementAt(ElementIdentifier);

  /**
   * Get a reference to the element at the given index.
   * It is assumed that the index exists, and it will not automatically
   * be created.
   *
   */
  const_reference ElementAt(ElementIdentifier) const;

  /**
   * Get a reference to the element at the given index.
   * If the element location does not exist, it will be created with a
   * default element value.
   *
   * It is assumed that the value of the element is modified through the
   * reference.
   */
  reference CreateElementAt(ElementIdentifier);

  /**
   * Read the element from the given index.
   * It is assumed that the index exists.
   */
  Element GetElement(ElementIdentifier) const;

  /**
   * Set the element value at the given index.
   * It is assumed that the index exists.
   */
  void SetElement(ElementIdentifier, Element);

  /**
   * Set the element value at the given index.
   * If the element location does not exist, it will be created with a
   * default element value.
   */
  void InsertElement(ElementIdentifier, Element);

  /**
   * Check if the index range of the vector is large enough to allow the
   * given index without expansion.
   */
  bool IndexExists(ElementIdentifier) const;

  /**
   * Check if the given index is in range of the vector.  If it is not, return
   * false.  Otherwise, set the element through the pointer (if it isn't nullptr),
   * and return true.
   */
  bool
  GetElementIfIndexExists(ElementIdentifier, Element *) const;

  /**
   * Make sure that the index range of the vector is large enough to allow
   * the given index, expanding it if necessary.  The index will contain
   * the default element regardless of whether expansion occurred.
   */
  void CreateIndex(ElementIdentifier);

  /**
   * Delete the element defined by the index identifier.  In practice, it
   * doesn't make sense to delete a vector index.  Instead, this method just
   * overwrite the index with the default element.
   */
  void DeleteIndex(ElementIdentifier);

  /**
   * Get a begin const iterator for the vector.
   */
  ConstIterator
  Begin() const;

  /**
   * Get an end const iterator for the vector.
   */
  ConstIterator
  End() const;

  /**
   * Get a begin iterator for the vector.
   */
  Iterator
  Begin();

  /**
   * Get an end iterator for the vector.
   */
  Iterator
  End();

  /**
   * Get the number of elements currently stored in the vector.
   */
  ElementIdentifier
  Size() const;

  /**
   * Tell the container to allocate enough memory to allow at least as many
   * elements as the size given to be stored.  In the generic case of ITK
   * containers this is NOT guaranteed to actually allocate any memory, but it
   * is useful if the implementation of the container allocates contiguous
   * storage. In the particular implementation of this VectorContainer the call
   * to this method actually allocates memory for the number of elements
   * defined by ElementIdentifier.
   */
  void Reserve(ElementIdentifier);

  /**
   * Tell the container to try to minimize its memory usage for storage of the
   * current number of elements.  This is NOT guaranteed to decrease memory
   * usage. This method is included here mainly for providing a unified API
   * with other containers in the toolkit.
   */
  void
  Squeeze();

  /**
   * Clear the elements. The final size will be zero.
   */
  void
  Initialize();
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVectorContainer.hxx"
#endif

#endif
