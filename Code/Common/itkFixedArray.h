/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFixedArray.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFixedArray_h
#define __itkFixedArray_h

#include "itkMacro.h"

namespace itk
{

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
template <typename TValueType, unsigned int VLength=3>
class FixedArray
{
public:
  /** Length constant */
  itkStaticConstMacro(Length, unsigned int, VLength);
  
  /** Dimension constant */
  itkStaticConstMacro(Dimension, unsigned int, VLength);
  
  /** The element type stored at each location in the FixedArray. */
  typedef TValueType  ValueType;
  
  /** A type representing the C-array version of this FixedArray. */
  typedef ValueType         CArray[VLength];
  
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
  
  typedef unsigned int   SizeType;
  
public:
  /** Constructors */
  FixedArray();
  FixedArray(const FixedArray& r);
  FixedArray(const ValueType r[VLength]);

  /** This destructor is not virtual for performance reasons. However, this
   * means that subclasses cannot allocate memory. */
  ~FixedArray();
  
  /** Operator= defined for a variety of types. */
  FixedArray& operator= (const FixedArray& r);
  FixedArray& operator= (const ValueType r[VLength]);
    
  /** Operators == and != are used to compare whether two arrays are equal.
   * Note that arrays are equal when the number of components (size) is the
   * same, and each component value is equal. */
  bool operator==(const FixedArray& r ) const;
  bool operator!=(const FixedArray& r ) const
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
    
private:
  /** Internal C array representation. */
  CArray  m_InternalArray;
  
public:
 
  static FixedArray Filled(const ValueType&);
};
  
template <typename TValueType, unsigned int VLength>
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
#include "itkFixedArray.txx"
#endif

#endif
