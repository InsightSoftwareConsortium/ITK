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
#ifndef itkVariableLengthVector_h
#define itkVariableLengthVector_h

#include <cassert>
#include <algorithm>
#include "itkNumericTraits.h"

namespace itk
{
/** \class VariableLengthVector
 * \brief Represents an array whose length can be defined at run-time.
 *
 * This class is templated over the data type. This data-type is meant
 * to be a scalar, such as float, double etc...
 *
 * \note
 * ITK itself provides several classes that can serve as \c Arrays.
 * \li FixedArray - Compile time fixed length arrays that's intended to
 * represent an enumerated collection of \c n entities.
 *
 * \li Array - Run time resizeable array that is intended to hold a
 * collection of \c n entities
 *
 * \li Vector - Compile time fixed length array that is intended to hold
 * a collection of \c n data types. A vector usually has a mathematical meaning.
 * It should only be used when mathematical operations such as addition,
 * multiplication by a scalar, product etc make sense.
 *
 * \li VariableLengthVector - Run time array that is intended to hold a collection
 * of scalar data types. Again, it should be used only when mathematical
 * operations on it are relevant. If not, use an Array.
 *
 * \li Point - Represents the spatial coordinates of a spatial location. Operators
 * on Point reflect geometrical concepts.
 *
 * \par For the reasons listed above, you cannot instantiate
 * \code VariableLengthVector< bool > \endcode.
 *
 * \par
 * Design Considerations: We do not derive from \c vnl_vector to avoid being
 * limited by the explicit template instantiations of vnl_vector and other
 * hacks that vnl folks have been forced to use.
 *
 * \note
 * This work is part of the National Alliance for Medical Image Computing
 * (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
 * for Medical Research, Grant U54 EB005149.
 *
 * \sa CovariantVector
 * \sa SymmetricSecondRankTensor
 * \sa RGBPixel
 * \sa DiffusionTensor3D
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{SimpleOperations/VariableLengthVector,Variable length vector}
 * \endwiki
 */
template< typename TValue >
class VariableLengthVector
{
public:
  /**\name Policies
   * The following Policies will be used by \c itk::VariableLengthVector::SetSize
   */
  //@{
  /** \c VariableLengthVector empty base-class for allocation policies.
   * All Allocation Policies are expected to inherit from this empty base
   * class.
   *
   * \sa \c itk::VariableLengthVector::SetSize
   * \sa \c NeverReallocate
   * \sa \c ShrinkToFit
   * \sa \c DontShrinkToFit
   * \ingroup ITKCommon
   * \ingroup DataRepresentation
   */
  struct AllocateRootPolicy {};

  /** \c VariableLengthVector Allocation Policy: Always reallocate memory.
   * This policy, when used from \c VariableLengthVector::SetSize(), always
   * implies that the previous internal buffer will be reallocated. Even if
   * enough memory was available.
   * \return true (always)
   *
   * \sa \c itk::VariableLengthVector::SetSize
   * \sa \c NeverReallocate
   * \sa \c ShrinkToFit
   * \sa \c DontShrinkToFit
   * \ingroup ITKCommon
   * \ingroup DataRepresentation
   */
  struct AlwaysReallocate : AllocateRootPolicy
    {
    bool operator()(unsigned int itkNotUsed(newSize), unsigned int itkNotUsed(oldSize)) const ITK_NOEXCEPT
      {
      return true;
      }
    };

  /** \c VariableLengthVector Allocation Policy: Never reallocate memory.
   * This policy, when used from \c VariableLengthVector::SetSize(), always
   * implies that the previous internal buffer will be kept. Even if not enough
   * memory was available.
   * \return false (always)
   *
   * \pre <tt>oldSize == newSize</tt>, checked by assertion
   * This policy is expected to be used when we know by construction that the
   * size of a \c VariableLengthVector never changes within a loop. For
   * instance, a typical scenario would be:
   * \code
   * VariableLengthVector<...> v;
   * v.SetSize(someFixedSize);
   * for (auto && pixel : image) {
   *     itkAssertInDebugAndIgnoreInReleaseMacro(expression.size() == someFixedSize);
   *     v.FastAssign( expression ); // <-- NeverReallocate is used here
   * }
   * \endcode
   *
   * \sa \c itk::VariableLengthVector::SetSize
   * \sa \c AlwaysReallocate
   * \sa \c ShrinkToFit
   * \sa \c DontShrinkToFit
   * \ingroup ITKCommon
   * \ingroup DataRepresentation
   */
  struct NeverReallocate : AllocateRootPolicy
    {
    bool operator()(unsigned int newSize, unsigned int oldSize) const ITK_NOEXCEPT
      {
      (void) newSize;
      (void) oldSize;
      itkAssertInDebugAndIgnoreInReleaseMacro(newSize == oldSize && "SetSize is expected to never change the VariableLengthVector size...");
      return true;
      }
    };

  /** \c VariableLengthVector Allocation Policy: reallocate memory only when
   * size changes.
   * This policy, when used from \c VariableLengthVector::SetSize(), will
   * reallocate the internal buffer only if the size of the \c
   * VariableLengthVector changes.
   * \return whether \c newSize differs from \c oldSize
   *
   * \note The name is related to \c DontShrinkToFit reallocation policy that
   * will avoid reallocating when enough memory has already been allocated.
   *
   * \sa \c itk::VariableLengthVector::SetSize
   * \sa \c AlwaysReallocate
   * \sa \c NeverReallocate
   * \sa \c DontShrinkToFit
   * \ingroup ITKCommon
   * \ingroup DataRepresentation
   */
  struct ShrinkToFit : AllocateRootPolicy
    {
    bool operator()(unsigned int newSize, unsigned int oldSize) const ITK_NOEXCEPT
      { return newSize != oldSize; }
    };

  /** \c VariableLengthVector Allocation Policy: reallocate memory only when
   * size increases.
   * This policy, when used from \c VariableLengthVector::SetSize(), will
   * reallocate the internal buffer only if the new size requested for the \c
   * VariableLengthVector increases.
   * \return whether \c newSize is bigger than \c oldSize
   *
   * \warning Unlike classes like \c std::vector<>, \c VariableLengthVector has
   * no capacity concept: the size of the \c VariableLengthVector is its
   * capacity. However, this will help a class without capacity to emulate one.
   * The consequence is that reallocations will occur with scenarios such as
   * the following:
   * \code
   * VariableLengthVector<...> v;
   * v.SetSize(42);
   * v.SetSize(12); // no reallocation
   * v.SetSize(42); // pointless reallocation (given this policy)
   * \endcode
   *
   * \sa \c itk::VariableLengthVector::SetSize
   * \sa \c AlwaysReallocate
   * \sa \c NeverReallocate
   * \sa \c ShrinkToFit
   * \ingroup ITKCommon
   * \ingroup DataRepresentation
   */
  struct DontShrinkToFit : AllocateRootPolicy
    {
    bool operator()(unsigned int newSize, unsigned int oldSize) const ITK_NOEXCEPT
      { return newSize > oldSize; }
    };

  /** \c VariableLengthVector empty base-class for values Keeping policies.
   * All Values Keeping Policies are expected to inherit from this empty base
   * class.
   *
   * \sa \c itk::VariableLengthVector::SetSize
   * \sa \c KeepOldValues
   * \sa \c DumpOldValues
   * \ingroup ITKCommon
   * \ingroup DataRepresentation
   */
  struct KeepValuesRootPolicy {};

  /** \c VariableLengthVector Invariability Policy: Always keep old values.
   * This policy, when used from \c VariableLengthVector::SetSize(), always
   * copies <tt>min(newSize,oldSize)</tt> previous values from the previous
   * internal buffer to the new one
   *
   * \pre This policy is only meant to be used in case of reallocation, i.e. \c
   * oldBuffer and \c newBuffer are expected to differ (unchecked).
   * \pre This presumes \c TValue assignment is a \c noexcept operation.
   *
   * This behaviour mimics \c std::vector<>::resize() behaviour. However, it
   * makes to sense from \c VariableLengthVector::operator=()
   *
   * \sa \c itk::VariableLengthVector::SetSize
   * \sa \c KeepValuesRootPolicy
   * \sa \c DumpOldValues
   * \ingroup ITKCommon
   * \ingroup DataRepresentation
   */
  struct KeepOldValues : KeepValuesRootPolicy
    {
    template <typename TValue2>
      void operator()(
        unsigned int newSize, unsigned int oldSize,
        TValue2 * oldBuffer, TValue2 * newBuffer) const ITK_NOEXCEPT
        {
        const std::size_t nb = std::min(newSize, oldSize);
        std::copy(oldBuffer, oldBuffer+nb, newBuffer);
        }
    };

  /** \c VariableLengthVector Invariability Policy: Never keep old values.
   * This policy, when used from \c VariableLengthVector::SetSize(), is a no-op.
   * It won't try to copy previous values from the previous internal buffer to
   * the new one.
   *
   * \pre This policy is only meant to be used in case of reallocation, i.e. \c
   * oldBuffer and \c newBuffer are expected to differ (unchecked).
   *
   * This behaviour particularly fits \c VariableLengthVector::operator=()
   *
   * \sa \c itk::VariableLengthVector::SetSize
   * \sa \c KeepValuesRootPolicy
   * \sa \c DumpOldValues
   * \ingroup ITKCommon
   * \ingroup DataRepresentation
   */
  struct DumpOldValues : KeepValuesRootPolicy
    {
    template <typename TValue2>
      void operator()(
        unsigned int itkNotUsed(newSize), unsigned int itkNotUsed(oldSize),
        TValue2 * itkNotUsed(oldBuffer), TValue2 * itkNotUsed(newBuffer)) const ITK_NOEXCEPT
        {
        }
    };
  //@}


  /** The element type stored at each location in the Array. */
  typedef TValue                                        ValueType;
  typedef TValue                                        ComponentType;
  typedef typename NumericTraits< ValueType >::RealType RealValueType;
  typedef VariableLengthVector                          Self;

  /** Typedef used to indicate the number of elements in the vector */
  typedef unsigned int ElementIdentifier;

  /** Default constructor. It is created with an empty array
   *  it has to be allocated later by assignment              */
  VariableLengthVector();

  /** Constructor with size. Size can only be changed by assignment */
  explicit VariableLengthVector(unsigned int dimension);

  /** Constructor that initializes array with contents from a user supplied
   * buffer. The pointer to the buffer and the length is specified. By default,
   * the array does not manage the memory of the buffer. It merely points to
   * that location and it is the user's responsibility to delete it.
   * If "LetArrayManageMemory" is true, then this class will free the
   * memory when this object is destroyed. */
  VariableLengthVector(ValueType *data, unsigned int sz,
                       bool LetArrayManageMemory = false);

  /** Constructor that initializes array with contents from a user supplied
   * buffer. The pointer to the buffer and the length is specified. By default,
   * the array does not manage the memory of the buffer. It merely points to
   * that location and it is the user's responsibility to delete it.
   * If "LetArrayManageMemory" is true, then this class will free the
   * memory when this object is destroyed. */
  VariableLengthVector(const ValueType *data, unsigned int sz,
                       bool LetArrayManageMemory = false);

  /** Copy constructor. The reason why the copy constructor and the assignment
   * operator are templated is that it will allow implicit casts to be
   * performed. For instance
   * \code
   * VariableLengthVector< int > vI;
   * VariableLengthVector< float > vF( vI );
   * or for instance vF = static_cast< VariableLengthVector< float > >( vI );
   * \endcode
   */
  template< typename T >
  VariableLengthVector(const VariableLengthVector< T > & v)
  {
    m_NumElements = v.Size();
    m_Data = this->AllocateElements(m_NumElements);
    m_LetArrayManageMemory = true;
    for ( ElementIdentifier i = 0; i < m_NumElements; ++i )
      {
      this->m_Data[i] = static_cast< ValueType >( v[i] );
      }
  }

  /** Copy constructor. Overrides the default non-templated copy constructor
   * that the compiler provides */
  VariableLengthVector(const VariableLengthVector< TValue > & v);

  /** Set the all the elements of the array to the specified value */
  void Fill(TValue const & v) ITK_NOEXCEPT;

  /** Converting assignment operator.
   * \note Ensures a <em>String Exception Guarantee</em>: resists to
   * self-assignment, and no changed are made if memory cannot be allocated to
   * hold the new elements. This presumes \c TValue assignment is a \c
   * noexcept operation.
   *
   * \post if called on a \c VariableLengthVector proxy, the referenced values
   * are left unchanged.
   * \post \c m_LetArrayManageMemory is true
   * \post <tt>GetSize() == v.GetSize()</tt>, modulo precision
   * \post <tt>*this == v</tt>
   */
  template< typename T >
  Self & operator=(const VariableLengthVector< T > & v)
    {
    // No self assignment test is done. Indeed:
    // - the operator already resists self assignment through a strong exception
    // guarantee
    // - the test becomes a pessimization as we never write
    //    VLV<const TValue> vcref(v.GetDataPointer(), v.GetSize());
    //    ...;
    //    v = vcref;
    ElementIdentifier const N = v.Size();
    this->SetSize( N, DontShrinkToFit(), DumpOldValues() );
    for ( ElementIdentifier i = 0; i < N; ++i )
      {
      this->m_Data[i] = static_cast< ValueType >( v[i] );
      }
    return *this;
  }

  /** Assignment operator.
   * \note Ensures a <em>String Exception Guarantee</em>: resists to
   * self-assignment, and no changed are made is memory cannot be allocated to
   * hold the new elements. This is excepting \c TValue assignment is a \c
   * noexcept operation.
   *
   * \post if called on a \c VariableLengthVector proxy, the referenced values
   * are left unchanged.
   * \post \c m_LetArrayManageMemory is true
   * \post <tt>GetSize() == v.GetSize()</tt>, modulo precision
   * \post <tt>*this == v</tt>
   */
  Self & operator=(const Self & v);

  /** Fast Assignment.
   * \pre \c m_LetArrayManageMemory is true: the \c VariableLengthVector is not
   * a proxy, checked with an assertion. Call <tt>SetSize(GetSize(), NeverReallocate(),
   * DumpOldValues())</tt> to ensure a vector is not a proxy anymore.
   * \pre current size is identical to the one from the right hand side
   * operand, checked with an assertion.
   */
  Self & FastAssign(const Self & v) ITK_NOEXCEPT;

  /** Assignment operator from a numeric value.
   * \pre This assumes \c m_LetArrayManageMemory is true, but it is unchecked.
   * If this operator is called on a \c VariableLengthVector proxy, referenced
   * values will be overwritten.
   */
  Self & operator=(TValue const & v) ITK_NOEXCEPT;

  /** Return the number of elements in the Array  */
  inline unsigned int Size(void) const ITK_NOEXCEPT { return m_NumElements; }
  inline unsigned int GetNumberOfElements(void) const ITK_NOEXCEPT { return m_NumElements; }

  /** Return reference to the element at specified index. No range checking. */
  TValue       & operator[](unsigned int i) ITK_NOEXCEPT { return this->m_Data[i]; }
  /** Return reference to the element at specified index. No range checking. */
  TValue const & operator[](unsigned int i) const ITK_NOEXCEPT { return this->m_Data[i]; }

  /** Get one element */
  inline const TValue & GetElement(unsigned int i) const ITK_NOEXCEPT { return m_Data[i]; }

  /** Set one element */
  void SetElement(unsigned int i, const TValue & value) ITK_NOEXCEPT { m_Data[i] = value; }

  /** Resizes the vector.
   * \tparam TReallocatePolicy Policy that determines precisely the conditions
   * under which the internal buffer shall be reallocated. It shall inherit
   * from \c AllocateRootPolicy.
   * \tparam TKeepValuesPolicy Policy that determines whether old elements
   * shall be kept. It shall inherit from \c KeepValuesRootPolicy.
   *
   * \internal
   * The purpose of this overload is to fine tune what \c SetSize() does. Some
   * users seem to need to always reallocate, or to maintain old elements.
   * However, some usages require fast resizing. In the assignment operators
   * cases, we don't need to reallocate anything if we have enough memory, and
   * we certainly do not need to maintain previous values as they'll get
   * overridden with new ones.
   * \internal
   * If we could assert that \c VariableLengthVector proxies would (shall!)
   * never be assigned anything, we could benefit for a version that won't
   * check \c m_LetArrayManageMemory.
   *
   * \post \c m_LetArrayManageMemory is true
   * \sa \c AlwaysReallocate
   * \sa \c NeverReallocate
   * \sa \c ShrinkToFit
   * \sa \c DontShrinkToFit
   * \sa \c KeepOldValues
   * \sa \c DumpOldValues
   */
  template <typename TReallocatePolicy, typename TKeepValuesPolicy>
  void SetSize(unsigned int sz,
          TReallocatePolicy reallocatePolicy,
          TKeepValuesPolicy keepValues);

  /** Set the size to that given.
   *
   * If \c destroyExistingData is \c false:
   * If the array already contains data, the existing data is copied over and
   * new space is allocated, if necessary. If the length to reserve is less
   * than the current number of elements, then an appropriate number of elements
   * are discarded.
   *    If \c true, the size is set destructively to the length given. If the
   * length is different from the current length, existing data will be lost.
   * The default is \c true. */
  void SetSize(unsigned int sz, bool destroyExistingData = true)
    {
    // Stays compatible with previous code version
    // And works around the fact C++03 template functions can't have default
    // arguments on template types.
    if (destroyExistingData)
      {
      SetSize(sz, AlwaysReallocate(), KeepOldValues());
      }
    else
      {
      SetSize(sz, ShrinkToFit(), KeepOldValues());
      }
    }

  /** Destroy data that is allocated internally, if LetArrayManageMemory is
   * true. */
  void DestroyExistingData() ITK_NOEXCEPT;

  inline unsigned int GetSize(void) const ITK_NOEXCEPT { return m_NumElements; }

  /** Set the pointer from which the data is imported.
   * If "LetArrayManageMemory" is false, then the application retains
   * the responsibility of freeing the memory for this data.  If
   * "LetArrayManageMemory" is true, then this class will free the
   * memory when this object is destroyed. */
  void SetData(TValue *data, bool LetArrayManageMemory = false) ITK_NOEXCEPT;

  /** Similar to the previous method. In the above method, the size must be
   * separately set prior to using user-supplied data. This introduces an
   * unnecessary allocation step to be performed. This method avoids it
   * and should be used to import data wherever possible to avoid this.
   * Set the pointer from which the data is imported.
   * If "LetArrayManageMemory" is false, then the application retains
   * the responsibility of freeing the memory for this data.  If
   * "LetArrayManageMemory" is true, then this class will free the
   * memory when this object is destroyed. */
  void SetData(TValue *data, unsigned int sz, bool LetArrayManageMemory = false) ITK_NOEXCEPT;

  /** This destructor is not virtual for performance reasons. However, this
   * means that subclasses cannot allocate memory. */
  ~VariableLengthVector();

  /** Reserves memory of a certain length.
   *
   * If the array already contains data, the existing data is copied over and
   * new space is allocated, if necessary. If the length to reserve is less
   * than the current number of elements, then an appropriate number of elements
   * are discarded. */
  void Reserve(ElementIdentifier);

  /** Allocate memory of certain size and return it.  */
  TValue * AllocateElements(ElementIdentifier size) const;

  const TValue * GetDataPointer() const ITK_NOEXCEPT { return m_Data; }

  /** Element-wise vector addition. The vectors do not have to have
   * the same element type. The input vector elements are cast to the
   * output vector element type before the addition is performed.
   *
   * \note For efficiency, the length of the vectors is not checked;
   * they are assumed to have the same length. */
  template< typename T >
  inline Self operator+(const VariableLengthVector< T > & v) const
  {
    itkAssertInDebugAndIgnoreInReleaseMacro( m_NumElements == v.GetSize() );
    const ElementIdentifier length = v.Size();
    Self                    result(length);

    for ( ElementIdentifier i = 0; i < length; i++ )
      {
      result[i] = ( *this )[i] + static_cast< ValueType >( v[i] );
      }
    return result;
  }

  /** Element-wise subtraction of vectors. The vectors do not have to
   * have the same element type. The input vector elements are cast to
   * the output vector element type before the subtraction is
   * performed.
   *
   * \note For efficiency, the length of the vectors is not checked;
   * they are assumed to have the same length. */
  template< typename T >
  inline Self operator-(const VariableLengthVector< T > & v) const
  {
    itkAssertInDebugAndIgnoreInReleaseMacro( m_NumElements == v.GetSize() );
    const ElementIdentifier length = v.Size();
    Self                    result(length);

    for ( ElementIdentifier i = 0; i < length; i++ )
      {
      result[i] = ( *this )[i] - static_cast< ValueType >( v[i] );
      }
    return result;
  }

  /** Multiply vector elements by a scalar 's'. The vector does not
   * have to have the same element type as the scalar type. The scalar
   * is cast to the output vector element type before the
   * multiplication is performed. */
  template< typename T >
  inline Self operator*(T s) const
  {
    Self result(m_NumElements);

    for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
      {
      result[i] = m_Data[i] * static_cast< ValueType >( s );
      }
    return result;
  }

  /** Divide vector elements by a scalar 's'. The vector does not
   * have to have the same element type as the scalar type. Both the
   * scalar and vector elements are cast to the RealValueType prior to
   * division, and the result is cast to the ValueType. */
  template< typename T >
  inline Self operator/(T s) const
  {
    Self result(m_NumElements);

    for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
      {
      result[i] = static_cast< ValueType >(
        static_cast< RealValueType >( m_Data[i] )
        / static_cast< RealValueType >( s ) );
      }
    return result;
  }

  /** Add scalar 's' to each element of the vector.*/
  inline Self operator+(TValue s) const
  {
    Self result(m_NumElements);

    for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
      {
      result[i] = m_Data[i] + s;
      }
    return result;
  }

  /** Subtract scalar 's' from each element of the vector.*/
  inline Self operator-(TValue s) const
  {
    Self result(m_NumElements);

    for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
      {
      result[i] = m_Data[i] - s;
      }
    return result;
  }

  /** Prefix operator that subtracts 1 from each element of the
   * vector. */
  inline Self & operator--() ITK_NOEXCEPT
  {
    for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
      {
      this->m_Data[i] -= static_cast< ValueType >( 1.0 );
      }
    return *this;
  }

  /** Prefix operator that adds 1 to each element of the vector. */
  inline Self & operator++() ITK_NOEXCEPT // prefix operator ++v;
  {
    for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
      {
      this->m_Data[i] += static_cast< ValueType >( 1.0 );
      }
    return *this;
  }

  /** Postfix operator that subtracts 1 from each element of the
   * vector. */
  inline Self operator--(int) // postfix operator v--;
  {
    Self tmp(*this);

    --tmp;
    return tmp;
  }

  /** Postfix operator that adds 1 to each element of the vector. */
  inline Self operator++(int) // postfix operator v++;
  {
    Self tmp(*this);

    ++tmp;
    return tmp;
  }

  /** Element-wise subtraction of vector 'v' from the current
   * vector. The vectors do not have to have the same element
   * type. The input vector elements are cast to the current vector
   * element type before the subtraction is performed.
   *
   * \note For efficiency, the length of the vectors is not checked;
   * they are assumed to have the same length. */
  template< typename T >
  inline Self & operator-=
    (const VariableLengthVector< T > & v) ITK_NOEXCEPT
  {
    for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
      {
      m_Data[i] -= static_cast< ValueType >( v[i] );
      }
    return *this;
  }

  /** Subtract scalar 's' from each element of the current vector. */
  inline Self & operator-=(TValue s) ITK_NOEXCEPT
  {
    for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
      {
      m_Data[i] -= s;
      }
    return *this;
  }

  /** Element-wise addition of vector 'v' to the current vector. The
   * vectors do not have to have the same element type. The input
   * vector elements are cast to the current vector element type
   * before the addition is performed.
   *
   * \note For efficiency, the length of the vectors is not checked;
   * they are assumed to have the same length. */
  template< typename T >
  inline Self & operator+=
    (const VariableLengthVector< T > & v) ITK_NOEXCEPT
  {
    for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
      {
      m_Data[i] += static_cast< ValueType >( v[i] );
      }
    return *this;
  }

  /** Add scalar 's' to each element of the vector. */
  inline Self & operator+=(TValue s) ITK_NOEXCEPT
  {
    for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
      {
      m_Data[i] += s;
      }
    return *this;
  }

  /** Multiply each element of the vector by a scalar 's'. The scalar
   * value is cast to the current vector element type prior to
   * multiplication. */
  template< typename T >
  inline Self & operator*=(T s) ITK_NOEXCEPT
  {
    for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
      {
      m_Data[i] *= ( static_cast< ValueType >( s ) );
      }
    return *this;
  }

  /** Divide vector elements by a scalar 's'. The vector does not
   * have to have the same element type as the scalar type. Both the
   * scalar and vector elements are cast to the RealValueType prior to
   * division, and the result is cast to the ValueType. */
  template< typename T >
  inline Self & operator/=(T s) ITK_NOEXCEPT
  {
    for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
      {
      m_Data[i] = static_cast< ValueType >(
        static_cast< RealValueType >( m_Data[i] )
        / static_cast< RealValueType >( s ) );
      }
    return *this;
  }

  /** Negates each vector element.
   * \warning This operator has a non standard semantics. Instead of returning
   * a new \c VariableLengthVector, it modifies the current object.
   */
  Self & operator-();  // negation operator

  bool operator==(const Self & v) const ITK_NOEXCEPT;

  bool operator!=(const Self & v) const ITK_NOEXCEPT;

  /** Returns vector's Euclidean Norm  */
  RealValueType GetNorm() const ITK_NOEXCEPT;

  /** Returns vector's squared Euclidean Norm  */
  RealValueType GetSquaredNorm() const ITK_NOEXCEPT;

  /** letArrayManageMemory getter. */
  bool IsAProxy() const ITK_NOEXCEPT { return ! m_LetArrayManageMemory;}
private:

  bool              m_LetArrayManageMemory; // if true, the array is responsible
                                            // for memory of data
  TValue *          m_Data;                 // Array to hold data
  ElementIdentifier m_NumElements;
};

/** Premultiply Operator for product of a VariableLengthVector and a scalar.
 *  VariableLengthVector< TValue >  =  T * VariableLengthVector< TValue >
 */
template< typename TValue, typename T >
inline
VariableLengthVector< TValue >
operator*(const T & scalar, const VariableLengthVector< TValue > & v)
{
  return v.operator*(scalar);
}


template< typename TValue >
std::ostream & operator<<(std::ostream & os, const VariableLengthVector< TValue > & arr)
{
  const unsigned int length = arr.Size();
  const signed int   last   = (unsigned int)length - 1;

  os << "[";
  for ( signed int i = 0; i < last; ++i )
    {
    os << arr[i] << ", ";
    }
  if ( length >= 1 )
    {
    os << arr[last];
    }
  os << "]";
  return os;
}
} // namespace itk

#include "itkNumericTraitsVariableLengthVectorPixel.h"

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVariableLengthVector.hxx"
#endif

#endif
