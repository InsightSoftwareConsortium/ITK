/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkArray.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkArray_h
#define __itkArray_h

#include "itkMacro.h"
#include "itkConceptChecking.h"

namespace itk
{

/**
 * Utility class for static range indexing support.
 */
template <unsigned long VFirst, unsigned long VLast>
class Range
{
public:
  enum { First = VFirst };
  enum { Last = VLast };
  enum { Length = Last-First+1 };
};

  
/** \class Array
 *
 * Simulates a standard C array, except that copy semantics are used instead
 * of reference semantics.  Also, arrays of different sizes cannot be
 * assigned to one another, and size information is known for function
 * returns.
 *
 * Template parameters for class Array:
 *
 * - TValueType = Element type stored at each location in the array.
 *
 * - VLength    = Length of the array.
 */
template <typename TValueType, unsigned long VLength>
class Array
{
  ITK_CLASS_REQUIRES(TValueType, AssignmentOperatorConcept);
public:
  /**
   * A class which behaves as a reference to this Array type.
   */
  class Reference;

  /**
   * A class which behaves as a const reference to this Array type.
   */
  class ConstReference;
  
  /**
   * The element type stored at each location in the Array.
   */
  typedef TValueType  ValueType;
  
  /**
   * The number of elements in the Array.
   */
  enum { Length = VLength };

  /**
   * A type representing the C-array version of this Array.
   */
  typedef ValueType         CArray[Length];
  
  /**
   * An iterator through the array.
   */
  typedef ValueType*        Iterator;

  /**
   * A const iterator through the array.
   */
  typedef const ValueType*  ConstIterator;

  /**
   * A reverse iterator through the array.
   */
  class ReverseIterator
  {
  public:
    explicit ReverseIterator(Iterator i): m_Iterator(i) {}
    Iterator operator++()        { return --m_Iterator; }
    Iterator operator++(int)     { return m_Iterator--; }
    Iterator operator--()        { return ++m_Iterator; }
    Iterator operator--(int)     { return m_Iterator++; }
    Iterator operator->() const  { return (m_Iterator-1); }
    ValueType& operator*() const { return *(m_Iterator-1); }
  private:
    Iterator m_Iterator;
  };
  
  
  /**
   * A const reverse iterator through the array.
   */
  class ConstReverseIterator
  {
  public:
    explicit ConstReverseIterator(ConstIterator i): m_Iterator(i) {}
    ConstIterator operator++()         { return --m_Iterator; }
    ConstIterator operator++(int)      { return m_Iterator--; }
    ConstIterator operator--()         { return ++m_Iterator; }
    ConstIterator operator--(int)      { return m_Iterator++; }
    ConstIterator operator->() const   { return (m_Iterator-1); }
    const ValueType& operator*() const { return *(m_Iterator-1); }
  private:
    ConstIterator m_Iterator;
  };  
  
  
  /**
   * A pointer to the ValueType.
   */
  typedef ValueType*        pointer;

  /**
   * A const pointer to the ValueType.
   */
  typedef const ValueType*  const_pointer;

  /**
   * A reference to the ValueType.
   */
  typedef ValueType&        reference;

  /**
   * A const reference to the ValueType.
   */
  typedef const ValueType&  const_reference;
  
  typedef unsigned long   SizeType;

protected:
  friend class Reference;
  /** \class ArrayCommaListCopier
   * Allows an Array to be assigned to a comma-separated list.
   * Array<int, 5> a;
   * a = 1, 2, 3, 4, 5;
   *
   * Note: Never directly used.  Only used internally by Array class.
   */
  class ArrayCommaListCopier
  {
  public:
    /**
     * Constructor takes an Iterator to beginning of array being assigned
     * and the first element in the list of assignments.
     */
    ArrayCommaListCopier(Iterator iter, const ValueType& elt): m_Iterator(iter)
      { *m_Iterator++ = elt; }

    /**
     * Each comma encountered increments the Iterator, and the next element
     * is assigned to the value after the comma.
     */
    ArrayCommaListCopier& operator,(const ValueType& elt)
      { *m_Iterator++ = elt; return *this; }
    
  private:
    /**
     * Stores current position in the array being assigned.
     */
    Iterator m_Iterator;
  };
  
public:
  Array();
  Array(const Array& r);
  Array(const Reference& r);
  Array(const ConstReference& r);
  Array(const ValueType r[Length]);
  ~Array();
  
  Array& operator= (const Array& r);
  Array& operator= (const Reference& r);
  Array& operator= (const ConstReference& r);
  Array& operator= (const ValueType r[Length]);
  ArrayCommaListCopier operator= (const ValueType& r);
  
  /*@{
   * Allow the Array to be indexed normally.
   * No bounds checking is done.
   * The separate versions are a work-around for an integer conversion
   * bug in Visual C++.
   */
        reference operator[](short index)                { return m_InternalArray[index]; }
  const_reference operator[](short index) const          { return m_InternalArray[index]; }
        reference operator[](unsigned short index)       { return m_InternalArray[index]; }
  const_reference operator[](unsigned short index) const { return m_InternalArray[index]; }
        reference operator[](int index)                  { return m_InternalArray[index]; }
  const_reference operator[](int index) const            { return m_InternalArray[index]; }
        reference operator[](unsigned int index)         { return m_InternalArray[index]; }
  const_reference operator[](unsigned int index) const   { return m_InternalArray[index]; }
        reference operator[](long index)                 { return m_InternalArray[index]; }
  const_reference operator[](long index) const           { return m_InternalArray[index]; }
        reference operator[](unsigned long index)        { return m_InternalArray[index]; }
  const_reference operator[](unsigned long index) const  { return m_InternalArray[index]; }
  //@}

  ValueType* GetDataPointer() { return m_InternalArray; }
  const ValueType* GetDataPointer() const { return m_InternalArray; }
  
  Iterator      Begin();
  ConstIterator Begin() const;
  Iterator      End();
  ConstIterator End() const;
  ReverseIterator      rBegin();
  ConstReverseIterator rBegin() const;
  ReverseIterator      rEnd();
  ConstReverseIterator rEnd() const;
  SizeType      Size() const;
  void Fill(const ValueType&);
  
  /**
   * Return a reference to a sub-range of the Array, specified by the
   * template parameters.
   */
  template <SizeType VFirst, SizeType VLast>
  typename Array<ValueType, (VLast-VFirst+1)>::Reference
  operator[](Range<VFirst, VLast>)
    {
    return Array<ValueType, (VLast-VFirst+1)>
      ::Reference(m_InternalArray+VFirst);
    }

  
  /**
   * Return a reference to a sub-range of the Array, specified by the
   * template parameters.
   */
  template <SizeType VFirst, SizeType VLast>
  typename Array<ValueType, (VLast-VFirst+1)>::ConstReference
  operator[](Range<VFirst, VLast>) const
    {
    return Array<ValueType, (VLast-VFirst+1)>
      ::ConstReference(m_InternalArray+VFirst);
    }
  
private:
  /**
   * Internal C array representation.
   */
  CArray  m_InternalArray;
  
public:
  /** \class Array<TValueType, VLength>::Reference
   * Identical to Array class, but uses reference semantics.  This can
   * reference either an Array, or a C-style array.
   */
  class Reference
  {
  public:
    /**
     * Constructor copies only the array pointer since this is a reference type.
     */
    Reference(Array& r) : m_InternalArray(r.Begin()) {}
    Reference(const Reference& r) : m_InternalArray(r.m_InternalArray) {}
    Reference(ValueType r[Length]) : m_InternalArray(r) {}

    /**
     * Assignment operator copies all Array values.
     * Values are copied individually instead of with a binary copy.  This
     * allows the ValueType's assignment operator to be executed.
     */
    Reference& operator= (const Array& r)
      {
      if(r.Begin() == m_InternalArray) return *this;
      ConstIterator input = r.Begin();
      for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
      return *this;
      }
      
    Reference& operator= (const Reference& r)
      {
      if(r.Begin() == m_InternalArray) return *this;
      ConstIterator input = r.Begin();
      for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
      return *this;
      }
    
    Reference& operator= (const ConstReference& r)
      {
      if(r.Begin() == m_InternalArray) return *this;
      ConstIterator input = r.Begin();
      for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
      return *this;
      }
    
    Reference& operator= (const ValueType r[Length])
      {
      if(r == m_InternalArray) return *this;
      ConstIterator input = r;
      for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
      return *this;
      }

    /**
     * Assignment operator to allow assignment via a comma-separated list.
     * It is assumed that the list is of the appropriate length.
     */
    ArrayCommaListCopier operator= (const ValueType& r) 
      { return ArrayCommaListCopier(this->Begin(), r); }
    
    /*@{
     * Allow the Array to be indexed normally.
     * No bounds checking is done.
     * The separate versions are a work-around for an integer conversion
     * bug in Visual C++.
     */
          reference operator[](short index)                { return m_InternalArray[index]; }
    const_reference operator[](short index) const          { return m_InternalArray[index]; }
          reference operator[](unsigned short index)       { return m_InternalArray[index]; }
    const_reference operator[](unsigned short index) const { return m_InternalArray[index]; }
          reference operator[](int index)                  { return m_InternalArray[index]; }
    const_reference operator[](int index) const            { return m_InternalArray[index]; }
          reference operator[](unsigned int index)         { return m_InternalArray[index]; }
    const_reference operator[](unsigned int index) const   { return m_InternalArray[index]; }
          reference operator[](long index)                 { return m_InternalArray[index]; }
    const_reference operator[](long index) const           { return m_InternalArray[index]; }
          reference operator[](unsigned long index)        { return m_InternalArray[index]; }
    const_reference operator[](unsigned long index) const  { return m_InternalArray[index]; }
    //@}
    
    ValueType* GetDataPointer() { return m_InternalArray; }
    const ValueType* GetDataPointer() const { return m_InternalArray; }

    /**
     * Get an Iterator for the beginning of the Array.
     */
    Iterator Begin()
      { return Iterator(m_InternalArray); }

    /**
     * Get a ConstIterator for the beginning of the Array.
     */
    ConstIterator Begin() const
      { return ConstIterator(m_InternalArray); }

    /**
     * Get an Iterator for the end of the Array.
     */
    Iterator End()
      { return Iterator(m_InternalArray+Length); }

    /**
     * Get a ConstIterator for the end of the Array.
     */
    ConstIterator End() const
      { return ConstIterator(m_InternalArray+Length); }

    /**
     * Get a begin ReverseIterator.
     */
    ReverseIterator rBegin()
      { return ReverseIterator(m_InternalArray+Length); }

    /**
     * Get a begin ConstReverseIterator.
     */
    ConstReverseIterator rBegin() const
      { return ConstReverseIterator(m_InternalArray+Length); }

    /**
     * Get an end ReverseIterator.
     */
    ReverseIterator rEnd()
      { return ReverseIterator(m_InternalArray); }

    /**
     * Get an end ConstReverseIterator.
     */
    ConstReverseIterator rEnd() const 
      { return ConstReverseIterator(m_InternalArray); }

    /**
     * Get the size of the Array.
     */
    SizeType      Size() const 
      { return Length; }

    /**
     * Fill all elements of the array with the given value.
     */
    void Fill(const ValueType& value)
      { for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = value; }
    
    /**
     * Return a reference to a sub-range of the Array, specified by the
     * template parameters.
     */
    template <SizeType VFirst, SizeType VLast>
    typename Array<ValueType, (VLast-VFirst+1)>::Reference
    operator[](Range<VFirst, VLast>)
      {
      return Array<ValueType, (VLast-VFirst+1)>
        ::Reference(m_InternalArray+VFirst);
      }    
    
    /**
     * Return a reference to a sub-range of the Array, specified by the
     * template parameters.
     */
    template <SizeType VFirst, SizeType VLast>
    typename Array<ValueType, (VLast-VFirst+1)>::ConstReference
    operator[](Range<VFirst, VLast>) const
      {
      return Array<ValueType, (VLast-VFirst+1)>
        ::ConstReference(m_InternalArray+VFirst);
      }
    
  private:
    /**
     * Store a pointer to the real memory.
     */
    ValueType * const m_InternalArray;
  };
  
  
  /** \class Array<TValueType, VLength>::ConstReference
   * Identical to Array<TValueType, VLength>::Reference class, but serves
   * as a const reference.
   */
  class ConstReference
  {
  public:
    /**
     * Constructor copies only the array pointer since this is a reference type.
     */
    ConstReference(const Array& r) : m_InternalArray(r.Begin()) {}
    ConstReference(const Reference& r) : m_InternalArray(r.Begin()) {}
    ConstReference(const ConstReference& r) : m_InternalArray(r.Begin()) {}
    ConstReference(const ValueType r[Length]) : m_InternalArray(r) {}
    
    /*@{
     * Allow the Array to be indexed normally.
     * No bounds checking is done.
     * The separate versions are a work-around for an integer conversion
     * bug in Visual C++.
     */
    const_reference operator[](short index) const          { return m_InternalArray[index]; }
    const_reference operator[](unsigned short index) const { return m_InternalArray[index]; }
    const_reference operator[](int index) const            { return m_InternalArray[index]; }
    const_reference operator[](unsigned int index) const   { return m_InternalArray[index]; }
    const_reference operator[](long index) const           { return m_InternalArray[index]; }
    const_reference operator[](unsigned long index) const  { return m_InternalArray[index]; }
    //@}
    
    const ValueType* GetDataPointer() const { return m_InternalArray; }
    
    /**
     * Get a ConstIterator for the beginning of the Array.
     */
    ConstIterator  Begin() const 
      { return ConstIterator(m_InternalArray); }
    
    /**
     * Get a ConstIterator for the end of the Array.
     */
    ConstIterator  End() const
      { return ConstIterator(m_InternalArray+Length); }
    
    /**
     * Get a begin ConstReverseIterator.
     */
    ConstReverseIterator  rBegin() const 
      { return ConstReverseIterator(m_InternalArray+Length); }
    
    /**
     * Get an end ConstReverseIterator.
     */
    ConstReverseIterator  rEnd() const
      { return ConstReverseIterator(m_InternalArray); }
    
    /**
     * Get the size of the Array.
     */
    SizeType       Size() const
      { return Length; }

    /**
     * Return a reference to a sub-range of the Array, specified by the
     * template parameters.
     */
    template <SizeType VFirst, SizeType VLast>
    typename Array<ValueType, (VLast-VFirst+1)>::ConstReference
    operator[](Range<VFirst, VLast>) const
      {
      return Array<ValueType, (VLast-VFirst+1)>
        ::ConstReference(m_InternalArray+VFirst);
      }

  private:
    /**
     * Store a pointer to the real memory.
     */
    const ValueType * const m_InternalArray;
  };
  
  static Array Filled(const ValueType&);
};
  
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkArray.txx"
#endif

#endif
