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

#include <vnl/vnl_vector_ref.h>

namespace itk
{

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
public:
  /**
   * A class which behaves as a reference to this Array type.
   */
  class Reference;

  /**
   * A class which behaves as a const reference to this Array type.
   */
  class ConstReference;
  
  /** \enum
   * The element type stored at each location in the Array.
   */
  typedef TValueType  ValueType;
  
  /** \enum
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

private:
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
    ArrayCommaListCopier(Iterator iter, const ValueType& elt);
    ArrayCommaListCopier& operator,(const ValueType& elt);
    
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
  
  inline Array& operator= (const Array& r);
  inline Array& operator= (const Reference& r);
  inline Array& operator= (const ValueType r[Length]);
  inline ArrayCommaListCopier operator= (const ValueType& r);
  
  /*@{
   * Allow the Array to be indexed normally.
   * No bounds checking is done.
   * The separate versions are a work-around for an integer conversion
   * bug in Visual C++.
   */
  inline       reference operator[](short index)                { return m_InternalArray[index]; }
  inline const_reference operator[](short index) const          { return m_InternalArray[index]; }
  inline       reference operator[](unsigned short index)       { return m_InternalArray[index]; }
  inline const_reference operator[](unsigned short index) const { return m_InternalArray[index]; }
  inline       reference operator[](int index)                  { return m_InternalArray[index]; }
  inline const_reference operator[](int index) const            { return m_InternalArray[index]; }
  inline       reference operator[](unsigned int index)         { return m_InternalArray[index]; }
  inline const_reference operator[](unsigned int index) const   { return m_InternalArray[index]; }
  inline       reference operator[](long index)                 { return m_InternalArray[index]; }
  inline const_reference operator[](long index) const           { return m_InternalArray[index]; }
  inline       reference operator[](unsigned long index)        { return m_InternalArray[index]; }
  inline const_reference operator[](unsigned long index) const  { return m_InternalArray[index]; }
  //@}

  inline operator ValueType* ();
  inline operator const ValueType* () const;
  inline Iterator      Begin();
  inline ConstIterator Begin() const;
  inline Iterator      End();
  inline ConstIterator End() const;
  inline SizeType      Size() const;
  
  inline ::vnl_vector_ref<ValueType> Get_vnl_vector();
  inline ::vnl_vector_ref<const ValueType> Get_vnl_vector() const;
  
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
    Reference(Array& r);
    Reference(Reference& r);
    Reference(ValueType r[Length]);
    inline Reference& operator= (const Array& r);
    inline Reference& operator= (const Reference& r);
    inline Reference& operator= (const ValueType r[Length]);
    inline ArrayCommaListCopier operator= (const ValueType& r);
    
    /*@{
     * Allow the Array to be indexed normally.
     * No bounds checking is done.
     * The separate versions are a work-around for an integer conversion
     * bug in Visual C++.
     */
    inline       reference operator[](short index)                { return m_InternalArray[index]; }
    inline const_reference operator[](short index) const          { return m_InternalArray[index]; }
    inline       reference operator[](unsigned short index)       { return m_InternalArray[index]; }
    inline const_reference operator[](unsigned short index) const { return m_InternalArray[index]; }
    inline       reference operator[](int index)                  { return m_InternalArray[index]; }
    inline const_reference operator[](int index) const            { return m_InternalArray[index]; }
    inline       reference operator[](unsigned int index)         { return m_InternalArray[index]; }
    inline const_reference operator[](unsigned int index) const   { return m_InternalArray[index]; }
    inline       reference operator[](long index)                 { return m_InternalArray[index]; }
    inline const_reference operator[](long index) const           { return m_InternalArray[index]; }
    inline       reference operator[](unsigned long index)        { return m_InternalArray[index]; }
    inline const_reference operator[](unsigned long index) const  { return m_InternalArray[index]; }
    //@}
    
    inline operator ValueType* () const;
    inline operator const ValueType* () const;
    inline Iterator      Begin();
    inline ConstIterator Begin() const;
    inline Iterator      End();
    inline ConstIterator End() const;
    inline SizeType      Size() const;

    inline ::vnl_vector_ref<ValueType> Get_vnl_vector();
    inline ::vnl_vector_ref<const ValueType> Get_vnl_vector() const;
    
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
    ConstReference(const Array& r);
    ConstReference(const Reference& r);
    ConstReference(const ConstReference& r);
    ConstReference(const ValueType r[Length]);
    
    /*@{
     * Allow the Array to be indexed normally.
     * No bounds checking is done.
     * The separate versions are a work-around for an integer conversion
     * bug in Visual C++.
     */
    inline const_reference operator[](short index) const          { return m_InternalArray[index]; }
    inline const_reference operator[](unsigned short index) const { return m_InternalArray[index]; }
    inline const_reference operator[](int index) const            { return m_InternalArray[index]; }
    inline const_reference operator[](unsigned int index) const   { return m_InternalArray[index]; }
    inline const_reference operator[](long index) const           { return m_InternalArray[index]; }
    inline const_reference operator[](unsigned long index) const  { return m_InternalArray[index]; }
    //@}
    
    inline operator const ValueType* () const;
    inline ConstIterator  Begin() const;
    inline ConstIterator  End() const;
    inline SizeType       Size() const;

    inline ::vnl_vector_ref<const ValueType> Get_vnl_vector() const;
    
  private:
    /**
     * Store a pointer to the real memory.
     */
    const ValueType * const m_InternalArray;
  };  
};
  
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkArray.txx"
#endif

#endif
