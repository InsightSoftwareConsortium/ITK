// #include "itkArray.h"

namespace itk
{


/**
 * Constructor takes an Iterator to beginning of array being assigned
 * and the first element in the list of assignments.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::ArrayCommaListCopier
::ArrayCommaListCopier(Iterator iter, const ValueType& elt):
  m_Iterator(iter)
{
  *m_Iterator++ = elt;
}


/**
 * Each comma encountered increments the Iterator, and the next element
 * is assigned to the value after the comma.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::ArrayCommaListCopier&
Array<TValueType, VLength>::ArrayCommaListCopier
::operator,(const ValueType& elt) 
{
  *m_Iterator++ = elt;
  return *this;
}


/**
 * Default constructor uses compiler's default initialization of memory.
 * For efficiency, no initialization to zero is done.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>
::Array()
{
}


/**
 * Copy constructor copies all Array values.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>
::Array(const Array& r)
{
  ConstIterator input = r.Begin();
  for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
}


/**
 * Copy constructor copies all Array values.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>
::Array(const Reference& r)
{
  ConstIterator input = r.Begin();
  for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
}


/**
 * Copy constructor copies all Array values.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>
::Array(const ConstReference& r)
{
  ConstIterator input = r.Begin();
  for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
}


/**
 * Constructor assumes input points to array of correct size.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>
::Array(const ValueType r[Length])
{
  ConstIterator input = r;
  for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
}


/**
 * Destructor does nothing special.  Here for completeness.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>
::~Array()
{
}


/**
 * Assignment operator copies all Array values.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::Array&
Array<TValueType, VLength>
::operator= (const Array& r)
{
  if(r.Begin() == m_InternalArray) return *this;
  ConstIterator input = r.Begin();
  for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
  return *this;
}


/**
 * Assignment operator copies all Array values.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::Array&
Array<TValueType, VLength>
::operator= (const Reference& r)
{
  if(r.Begin() == m_InternalArray) return *this;
  ConstIterator input = r.Begin();
  for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
  return *this;
}


/**
 * Assignment operator assumes input points to array of correct size.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::Array&
Array<TValueType, VLength>
::operator= (const ValueType r[Length])
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
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::ArrayCommaListCopier
Array<TValueType, VLength>
::operator= (const ValueType& r)
{
  return ArrayCommaListCopier(this->Begin(), r);
}


/**
 * Allow the Array to look like a standard C array.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>
::operator TValueType* ()
{
  return m_InternalArray;
}


/**
 * Allow the Array to look like a standard C array.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>
::operator const TValueType* () const
{
  return m_InternalArray;
}


/**
 * Get an Iterator for the beginning of the Array.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::Iterator
Array<TValueType, VLength>
::Begin()
{
  return (Iterator)m_InternalArray;
}


/**
 * Get a ConstIterator for the beginning of the Array.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::ConstIterator
Array<TValueType, VLength>
::Begin() const
{
  return (ConstIterator)m_InternalArray;
}


/**
 * Get an Iterator for the end of the Array.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::Iterator
Array<TValueType, VLength>
::End()
{
  return (Iterator)(m_InternalArray+Length);
}


/**
 * Get a ConstIterator for the end of the Array.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::ConstIterator
Array<TValueType, VLength>
::End() const
{
  return (ConstIterator)(m_InternalArray+Length);
}


/**
 * Get the size of the Array.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::SizeType
Array<TValueType, VLength>
::Size() const
{
  return Length; 
}


/**
 * Get a reference to the array that can be treated as a vnl vector.
 */
template <typename TValueType, unsigned long VLength>
::vnl_vector_ref<Array<TValueType, VLength>::ValueType> 
Array<TValueType, VLength>
::Get_vnl_vector()
{
  return ::vnl_vector_ref<ValueType>(Length, m_InternalArray);
}


/**
 * Get a reference to the array that can be treated as a vnl vector.
 */
template <typename TValueType, unsigned long VLength>
::vnl_vector_ref<const Array<TValueType, VLength>::ValueType>
Array<TValueType, VLength>
::Get_vnl_vector() const
{
  return ::vnl_vector_ref<const ValueType>(Length, m_InternalArray);
}


/**
 * Constructor copies only the array pointer since this is a reference type.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::Reference
::Reference(Array& r):
  m_InternalArray(r.Begin())
{
}


/**
 * Constructor copies only the array pointer since this is a reference type.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::Reference
::Reference(Reference& r):
  m_InternalArray(r.Begin())
{
}


/**
 * Constructor copies only the array pointer since this is a reference type.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::Reference
::Reference(ValueType r[Length]):
  m_InternalArray(r)
{
}


/**
 * Assignment operator copies all Array values.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::Reference&
Array<TValueType, VLength>::Reference
::operator= (const Array& r)
{
  if(r.Begin() == m_InternalArray) return *this;
  ConstIterator input = r.Begin();
  for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
  return *this;
}


/**
 * Assignment operator copies all Array values.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::Reference&
Array<TValueType, VLength>::Reference
::operator= (const Reference& r)
{
  if(r.Begin() == m_InternalArray) return *this;
  ConstIterator input = r.Begin();
  for(Iterator i = this->Begin() ; i != this->End() ;) *i++ = *input++;
  return *this;
}


/**
 * Assignment operator assumes input points to array of correct size.
 * Values are copied individually instead of with a binary copy.  This
 * allows the ValueType's assignment operator to be executed.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::Reference&
Array<TValueType, VLength>::Reference
::operator= (const ValueType r[Length])
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
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::ArrayCommaListCopier
Array<TValueType, VLength>::Reference
::operator= (const ValueType& r)
{
  return ArrayCommaListCopier(this->Begin(), r);
}



/**
 * Allow the Array to look like a standard C array.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::Reference
::operator TValueType* () const
{
  return m_InternalArray;
}


/**
 * Allow the Array to look like a standard C array.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::Reference
::operator const TValueType* () const
{
  return m_InternalArray;
}


/**
 * Get an Iterator for the beginning of the Array.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::Iterator
Array<TValueType, VLength>::Reference
::Begin()
{
  return (Iterator)m_InternalArray;
}


/**
 * Get a ConstIterator for the beginning of the Array.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::ConstIterator
Array<TValueType, VLength>::Reference
::Begin() const
{
  return (ConstIterator)m_InternalArray;
}


/**
 * Get an Iterator for the end of the Array.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::Iterator
Array<TValueType, VLength>::Reference
::End()
{
  return (Iterator)(m_InternalArray+Length);
}


/**
 * Get a ConstIterator for the end of the Array.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::ConstIterator
Array<TValueType, VLength>::Reference
::End() const
{
  return (ConstIterator)(m_InternalArray+Length);
}


/**
 * Get the size of the Array.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::SizeType
Array<TValueType, VLength>::Reference
::Size() const
{
  return Length; 
}



/**
 * Get a reference to the array that can be treated as a vnl vector.
 */
template <typename TValueType, unsigned long VLength>
::vnl_vector_ref<Array<TValueType, VLength>::ValueType> 
Array<TValueType, VLength>::Reference
::Get_vnl_vector()
{
  return ::vnl_vector_ref<ValueType>(Length, m_InternalArray);
}


/**
 * Get a reference to the array that can be treated as a vnl vector.
 */
template <typename TValueType, unsigned long VLength>
::vnl_vector_ref<const Array<TValueType, VLength>::ValueType>
Array<TValueType, VLength>::Reference
::Get_vnl_vector() const
{
  return ::vnl_vector_ref<const ValueType>(Length, m_InternalArray);
}


/**
 * Constructor copies only the array pointer since this is a reference type.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::ConstReference
::ConstReference(const Array& r):
  m_InternalArray(r.Begin())
{
}


/**
 * Constructor copies only the array pointer since this is a reference type.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::ConstReference
::ConstReference(const Reference& r):
  m_InternalArray(r.Begin())
{
}


/**
 * Constructor copies only the array pointer since this is a reference type.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::ConstReference
::ConstReference(const ConstReference& r):
  m_InternalArray(r.Begin())
{
}


/**
 * Constructor copies only the array pointer since this is a reference type.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::ConstReference
::ConstReference(const ValueType r[Length]):
  m_InternalArray(r)
{
}


/**
 * Allow the Array to look like a standard C array.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::ConstReference
::operator const TValueType* () const
{
  return m_InternalArray;
}


/**
 * Get a ConstIterator for the beginning of the Array.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::ConstIterator
Array<TValueType, VLength>::ConstReference
::Begin() const
{
  return (ConstIterator)m_InternalArray;
}


/**
 * Get a ConstIterator for the end of the Array.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::ConstIterator
Array<TValueType, VLength>::ConstReference
::End() const
{
  return (ConstIterator)(m_InternalArray+Length);
}


/**
 * Get the size of the Array.
 */
template <typename TValueType, unsigned long VLength>
Array<TValueType, VLength>::SizeType
Array<TValueType, VLength>::ConstReference
::Size() const
{
  return Length;
}


/**
 * Get a reference to the array that can be treated as a vnl vector.
 */
template <typename TValueType, unsigned long VLength>
::vnl_vector_ref<const Array<TValueType, VLength>::ValueType>
Array<TValueType, VLength>::ConstReference
::Get_vnl_vector() const
{
  return ::vnl_vector_ref<const ValueType>(Length, m_InternalArray);
}


} // namespace itk
