/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkArray.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkArray_h
#define __itkArray_h

#include "itkMacro.h"

namespace itk
{

/** \brief Utility class for static range indexing support. */
template <unsigned long VFirst, unsigned long VLast>
class Range
{
public:
  enum { First = VFirst };
  enum { Last = VLast };
  enum { Length = Last-First+1 };
};
  
/** \class FixedArray
 *  \brief Simulate a standard C array with copy semnatics.
 *
 * Simulates a standard C array, except that copy semantics are used instead
 * of reference semantics.  Also, arrays of different sizes cannot be
 * assigned to one another, and size information is known for function
 * returns.
 *
 * Template parameters for class FixedArray:
 * - TValueType = Element type stored at each location in the array.
 * - VLength    = Length of the array.
 *
 * The length of the array is fixed at compile time. If you wish to
 * specify the length of the array at run-time, use the class itk::Array.
 * If you wish to change to change the length of the array at run-time,
 * you're best off using std::vector<>.
 *
 * \ingroup DataRepresentation 
 */
template <typename TValueType, unsigned long VLength>
class FixedArray
{
public:
  /** A class which behaves as a reference to this FixedArray type. */
  class Reference;

  /** A class which behaves as a const reference to this FixedArray type. */
  class ConstReference;
  
  /** The element type stored at each location in the FixedArray. */
  typedef TValueType  ValueType;
  
  /** The number of elements in the FixedArray. */
  enum { Length = VLength };

  /** A type representing the C-array version of this FixedArray. */
  typedef ValueType         CArray[Length];
  
  /** An iterator through the array. */
  typedef ValueType*        Iterator;

  /** A const iterator through the array. */
  typedef const ValueType*  ConstIterator;

  /** \brief A reverse iterator through the array. */
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
  
  /** \brief A const reverse iterator through the array. */
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
  
  /** A pointer to the ValueType. */
  typedef ValueType*        pointer;

  /** A const pointer to the ValueType. */
  typedef const ValueType*  const_pointer;

  /** A reference to the ValueType. */
  typedef ValueType&        reference;

  /** A const reference to the ValueType. */
  typedef const ValueType&  const_reference;
  
  typedef unsigned long   SizeType;
  
public:
  FixedArray();
  FixedArray(const FixedArray& r);
  FixedArray(const Reference& r);
  FixedArray(const ConstReference& r);
  FixedArray(const ValueType r[Length]);

  /** This destructor is not virtual for performance reasons. However, this
   * means that subclasses cannot allocate memory. */
  ~FixedArray();
  
  /** Operator= defined for a variety of types. */
  FixedArray& operator= (const FixedArray& r);
  FixedArray& operator= (const Reference& r);
  FixedArray& operator= (const ConstReference& r);
  FixedArray& operator= (const ValueType r[Length]);
    
  /** Operators == and != are used to compare whether two arrays are equal.
   * Note that arrays are equal when the number of components (size) is the
   * same, and each component value is equal. */
  bool operator==(const FixedArray& r ) const;
  bool operator==(const Reference& r ) const;
  bool operator==(const ConstReference& r ) const;
  bool operator!=(const FixedArray& r ) const
    { return !operator==(r); }
  bool operator!=(const Reference& r ) const
    { return !operator==(r); }
  bool operator!=(const ConstReference& r ) const
    { return !operator==(r); }
  
  /** Allow the FixedArray to be indexed normally.  No bounds checking is done.
   * The separate versions are a work-around for an integer conversion bug in
   * Visual C++. */
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
  
  /** Return a pointer to the data. */
  ValueType* GetDataPointer() { return m_InternalArray; }
  const ValueType* GetDataPointer() const { return m_InternalArray; }
    
  /** Get various iterators to the array. */
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
    
  /** Return a reference to a sub-range of the FixedArray, specified by the
   * template parameters. */
  template <SizeType VFirst, SizeType VLast>
  typename FixedArray<ValueType, (VLast-VFirst+1)>::Reference
  operator[](Range<VFirst, VLast>)
    {
    return FixedArray<ValueType, (VLast-VFirst+1)>
      ::Reference(m_InternalArray+VFirst);
    }

  /** Return a reference to a sub-range of the FixedArray, specified by the
   * template parameters. */
  template <SizeType VFirst, SizeType VLast>
  typename FixedArray<ValueType, (VLast-VFirst+1)>::ConstReference
  operator[](Range<VFirst, VLast>) const
    {
    return FixedArray<ValueType, (VLast-VFirst+1)>
      ::ConstReference(m_InternalArray+VFirst);
    }
  
private:
  /** Internal C array representation. */
  CArray  m_InternalArray;
  
public:
  /** \brief Provide reference semantics for the array.
   *
   * Identical to FixedArray class, but uses reference semantics.  This can
   * reference either an FixedArray, or a C-style array. */
  class Reference
  {
  public:
    /** Constructor copies only the array pointer since this is a reference
     *  type.  */
    Reference(FixedArray& r) : m_InternalArray(r.Begin()) {}
    Reference(const Reference& r) : m_InternalArray(r.m_InternalArray) {}
    Reference(ValueType r[Length]) : m_InternalArray(r) {}
    
    /** Assignment operator copies all FixedArray values.
     * Values are copied individually instead of with a binary copy.  This
     * allows the ValueType's assignment operator to be executed.   */
    Reference& operator= (const FixedArray& r)
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
    
    /** Allow the FixedArray to be indexed normally.
     * No bounds checking is done.
     * The separate versions are a work-around for an integer conversion
     * bug in Visual C++. */
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
        
    /** Return a pointer to the data. */
    ValueType* GetDataPointer() { return m_InternalArray; }
    const ValueType* GetDataPointer() const { return m_InternalArray; }
    
    /** Get an Iterator for the beginning of the FixedArray.   */
    Iterator Begin()
      { return Iterator(m_InternalArray); }

    /** Get a ConstIterator for the beginning of the FixedArray.   */
    ConstIterator Begin() const
      { return ConstIterator(m_InternalArray); }

    /** Get an Iterator for the end of the FixedArray.   */
    Iterator End()
      { return Iterator(m_InternalArray+Length); }

    /** Get a ConstIterator for the end of the FixedArray.   */
    ConstIterator End() const
      { return ConstIterator(m_InternalArray+Length); }

    /** Get a begin ReverseIterator.   */
    ReverseIterator rBegin()
      { return ReverseIterator(m_InternalArray+Length); }

    /** Get a begin ConstReverseIterator.   */
    ConstReverseIterator rBegin() const
      { return ConstReverseIterator(m_InternalArray+Length); }

    /** Get an end ReverseIterator.   */
    ReverseIterator rEnd()
      { return ReverseIterator(m_InternalArray); }

    /** Get an end ConstReverseIterator.   */
    ConstReverseIterator rEnd() const 
      { return ConstReverseIterator(m_InternalArray); }

    /** Get the size of the FixedArray.   */
    SizeType      Size() const 
      { return Length; }

    /** Fill all elements of the array with the given value.   */
    void Fill(const ValueType& value)
      { for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = value; }
    
    /** Return a reference to a sub-range of the FixedArray, specified by the
     * template parameters.   */
    template <SizeType VFirst, SizeType VLast>
    typename FixedArray<ValueType, (VLast-VFirst+1)>::Reference
    operator[](Range<VFirst, VLast>)
      {
      return FixedArray<ValueType, (VLast-VFirst+1)>
        ::Reference(m_InternalArray+VFirst);
      }    
    
    /** Return a reference to a sub-range of the FixedArray, specified by the
     * template parameters.   */
    template <SizeType VFirst, SizeType VLast>
    typename FixedArray<ValueType, (VLast-VFirst+1)>::ConstReference
    operator[](Range<VFirst, VLast>) const
      {
      return FixedArray<ValueType, (VLast-VFirst+1)>
        ::ConstReference(m_InternalArray+VFirst);
      }
    
  private:
    /** Store a pointer to the real memory.   */
    ValueType * const m_InternalArray;
  };
  
  /** \brief Provide const reference to an array.
   *
   * Identical to FixedArray<TValueType, VLength>::Reference class, but serves
   * as a const reference. */
  class ConstReference
  {
  public:
    /** Constructor copies only the array pointer since this is a reference
     *  type.  */
    ConstReference(const FixedArray& r) : m_InternalArray(r.Begin()) {}
    ConstReference(const Reference& r) : m_InternalArray(r.Begin()) {}
    ConstReference(const ConstReference& r) : m_InternalArray(r.Begin()) {}
    ConstReference(const ValueType r[Length]) : m_InternalArray(r) {}
        
    /** Allow the FixedArray to be indexed normally.
     * No bounds checking is done.
     * The separate versions are a work-around for an integer conversion
     * bug in Visual C++. */
    const_reference operator[](short index) const          { return m_InternalArray[index]; }
    const_reference operator[](unsigned short index) const { return m_InternalArray[index]; }
    const_reference operator[](int index) const            { return m_InternalArray[index]; }
    const_reference operator[](unsigned int index) const   { return m_InternalArray[index]; }
    const_reference operator[](long index) const           { return m_InternalArray[index]; }
    const_reference operator[](unsigned long index) const  { return m_InternalArray[index]; }
        
    /** Return a pointer to the data. */
    const ValueType* GetDataPointer() const { return m_InternalArray; }
    
    /** Get a ConstIterator for the beginning of the FixedArray.   */
    ConstIterator  Begin() const 
      { return ConstIterator(m_InternalArray); }
    
    /** Get a ConstIterator for the end of the FixedArray.   */
    ConstIterator  End() const
      { return ConstIterator(m_InternalArray+Length); }
    
    /** Get a begin ConstReverseIterator.   */
    ConstReverseIterator  rBegin() const 
      { return ConstReverseIterator(m_InternalArray+Length); }
    
    /** Get an end ConstReverseIterator.   */
    ConstReverseIterator  rEnd() const
      { return ConstReverseIterator(m_InternalArray); }
    
    /** Get the size of the FixedArray.   */
    SizeType       Size() const
      { return Length; }

    /** Return a reference to a sub-range of the FixedArray, specified by the
     * template parameters.   */
    template <SizeType VFirst, SizeType VLast>
    typename FixedArray<ValueType, (VLast-VFirst+1)>::ConstReference
    operator[](Range<VFirst, VLast>) const
      {
      return FixedArray<ValueType, (VLast-VFirst+1)>
        ::ConstReference(m_InternalArray+VFirst);
      }

  private:
    /** Store a pointer to the real memory.   */
    const ValueType * const m_InternalArray;
  };
  
  static FixedArray Filled(const ValueType&);
};
  
template <typename TValueType, unsigned long VLength>
std::ostream & operator<<(std::ostream &os, const FixedArray<TValueType,VLength> &arr)
{
  os << "[";
  for (unsigned int i=0; i < VLength - 1; ++i)
    {
    os << arr[i] << ", ";
    }
  if (VLength >= 1)
    {
    os << arr[VLength-1];
    }
  os << "]" << std::endl;
  return os;
}

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkArray.txx"
#endif

#endif
