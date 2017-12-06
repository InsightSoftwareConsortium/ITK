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
#include "itkStaticAssert.h"
#include "itkMetaProgrammingLibrary.h"
#include "itkEnableIf.h"
#include "itkIsBaseOf.h"
#include "itkIsNumber.h"
#include "itkPromoteType.h"
#include "itkBinaryOperationConcept.h"

namespace itk
{

template <typename TExpr1, typename TExpr2, typename  TBinaryOp>
struct VariableLengthVectorExpression;

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
 *
 * \invariant If \c m_LetArrayManageMemory is true, \c m_Data is deletable
 * (whether it's null or pointing to something with no elements. i.e. \c
 * m_NumElements may be 0 and yet \c m_Data may be not null.)
 */
template< typename TValue >
class ITK_TEMPLATE_EXPORT VariableLengthVector
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
    bool operator()(unsigned int itkNotUsed(newSize), unsigned int itkNotUsed(oldSize)) const
      {
      return true;
      }
    };

  /** \c VariableLengthVector Allocation Policy: Never reallocate memory.
   * This policy, when used from \c VariableLengthVector::SetSize(), always
   * implies that the previous internal buffer will be kept. Even if not enough
   * memory was available.
   *
   * The typical use case of this policy is to make sure a \c
   * VariableLengthVector is not a proxy object.
   * \return false (always)
   *
   * \pre <tt>oldSize == newSize</tt>, checked by assertion
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
    bool operator()(unsigned int newSize, unsigned int oldSize) const
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
    bool operator()(unsigned int newSize, unsigned int oldSize) const
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
   \code
   VariableLengthVector<...> v;
   v.SetSize(42);
   v.SetSize(12); // no reallocation
   v.SetSize(42); // pointless reallocation (given this policy)
   \endcode
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
    bool operator()(unsigned int newSize, unsigned int oldSize) const
      { return newSize > oldSize; }
    };

  /** \c VariableLengthVector empty base-class for values Keeping policies.
   * All Values Keeping Policies are expected to inherit from this empty base
   * class.
   *
   * The preconditions common to all sub classes are:
   * \pre This policy is only meant to be executed in case of reallocation,
   * i.e. \c oldBuffer and \c newBuffer are expected to differ (unchecked).
   * \pre This presumes \c TValue assignment is a \c noexcept operation.
   * \pre \c newBuffer is not null (pre-conditions imposed by some
   * implementations of \c std::copy())
   * \pre `[oldBuffer, oldBuffer+oldSize)` is a valid range
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
   * \pre This policy is only meant to be executed in case of reallocation,
   * i.e. \c oldBuffer and \c newBuffer are expected to differ (unchecked).
   * \pre This presumes \c TValue assignment is a \c noexcept operation.
   * \pre \c newBuffer is not null (pre-conditions imposed by some
   * implementations of \c std::copy())
   * \pre `[oldBuffer, oldBuffer+oldSize)` is a valid range
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
        TValue2 * oldBuffer, TValue2 * newBuffer) const
        {
        itkAssertInDebugAndIgnoreInReleaseMacro(newBuffer);
        const std::size_t nb = std::min(newSize, oldSize);
        itkAssertInDebugAndIgnoreInReleaseMacro(nb == 0 || (nb > 0  && oldBuffer != ITK_NULLPTR));
        std::copy(oldBuffer, oldBuffer+nb, newBuffer);
        }
    };

  /** \c VariableLengthVector Invariability Policy: Never keep old values.
   * This policy, when used from \c VariableLengthVector::SetSize(), is a no-op.
   * It won't try to copy previous values from the previous internal buffer to
   * the new one.
   *
   * \pre This policy is only meant to be executed in case of reallocation,
   * i.e. \c oldBuffer and \c newBuffer are expected to differ (unchecked).
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
        TValue2 * itkNotUsed(oldBuffer), TValue2 * itkNotUsed(newBuffer)) const
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
   *  it has to be allocated later by assignment, \c SetSize() or \c Reserve().
   * \post \c m_Data is null
   * \post \c m_NumElements is 0
   * \post \c m_LetArrayManageMemory is true
   */
  VariableLengthVector();

  /** Constructor with size.
   * Size can only be changed by assignment, \c SetSize() or \c Reserve().
   * \post \c m_Data is not null and points to an array of \c m_NumElements,
   * even if \c m_NumElements is 0
   * \post values are left uninitialized.
   * \post \c m_NumElements is \c dimension
   * \post \c m_LetArrayManageMemory is true
   */
  explicit VariableLengthVector(unsigned int dimension);

  /** Constructor that initializes array with contents from a user supplied
   * buffer.
   * The pointer to the buffer and the length is specified. By default, the
   * array does not manage the memory of the buffer. It merely points to that
   * location and it is the user's responsibility to delete it.
   * If \c LetArrayManageMemory is true, then this class will free the
   * memory when this object is destroyed.
   *
   * \post `m_Data == data`
   * \post values are left unmodified
   * \post `m_NumElements == sz`
   * \post `m_LetArrayManageMemory == LetArrayManageMemory`
   */
  VariableLengthVector(ValueType *data, unsigned int sz,
                       bool LetArrayManageMemory = false);

  /** Constructor that initializes array with contents from a user supplied
   * buffer.
   * The pointer to the buffer and the length is specified. By default, the
   * array does not manage the memory of the buffer. It merely points to that
   * location and it is the user's responsibility to delete it.
   * If \c LetArrayManageMemory is true, then this class will free the
   * memory when this object is destroyed.
   *
   * \warning This overload receives a non-modiable array, and yet it will let
   * the end-user try to modify it through \c VariableLengthVector interface.
   * Use this constructor with care as this may lead to undefined behaviour.
   * Prefer using `VariableLengthVector<const TValue>` instead of
   * `VariableLengthVector<TValue>` in case we which to use this constructor.
   *
   * \post `m_Data == data`
   * \post values are left unmodified
   * \post `m_NumElements == sz`
   * \post `m_LetArrayManageMemory == LetArrayManageMemory`
   */
  VariableLengthVector(const ValueType *data, unsigned int sz,
                       bool LetArrayManageMemory = false);

  /** Copy constructor. The reason why the copy constructor and the assignment
   * operator are templated is that it will allow implicit casts to be
   * performed. For instance:
   \code
   VariableLengthVector< int > vI;
   VariableLengthVector< float > vF( vI );
   or for instance vF = static_cast< VariableLengthVector< float > >( vI );
   \endcode
   * \note However that static casting in this way will imply the allocation of
   * a temporary \c VariableLengthVector. Prefer to directly use the assignment
   * converting operator in code where uses of \c static_cast<> would be
   * required.
   *
   * \post \c m_Data is not null and points to an array of \c m_NumElements,
   * if \c m_NumElements is 0, otherwise it's null.
   * \post values are left uninitialized.
   * \post \c m_NumElements is \c v.GetSize()
   * \post \c m_LetArrayManageMemory is true
   */
  template< typename T >
  VariableLengthVector(const VariableLengthVector< T > & v)
    {
    m_NumElements = v.Size();
    m_LetArrayManageMemory = true;
    if (m_NumElements != 0)
      {
      m_Data = this->AllocateElements(m_NumElements);
      itkAssertInDebugAndIgnoreInReleaseMacro(m_Data != ITK_NULLPTR);
      for ( ElementIdentifier i = 0; i < m_NumElements; ++i )
        {
        this->m_Data[i] = static_cast< ValueType >( v[i] );
        }
      }
    else
      {
      m_Data = ITK_NULLPTR;
      }
    }

  /** Copy constructor. Overrides the default non-templated copy constructor
   * that the compiler provides.
   * \post \c m_Data is not null and points to an array of \c m_NumElements,
   * if \c m_NumElements is 0, otherwise it's null.
   * \post values are left uninitialized.
   * \post \c m_NumElements is \c v.GetSize()
   * \post \c m_LetArrayManageMemory is true
   */
  VariableLengthVector(const VariableLengthVector< TValue > & v);

  /** Swaps two \c VariableLengthVector 's.
   * \pre Expects either none of the \c VariableLengthVector to act as a proxy,
   * or both, checked with an assertion.
   * \post \c *this and \c old contents are swapped.
   * \param[in,out] v  other \c VariableLengthVector to be swapped with.
   * \throw None
   * \sa \c itk::swap()
   */
  void Swap(Self & v) ITK_NOEXCEPT
    {
    itkAssertInDebugAndIgnoreInReleaseMacro(m_LetArrayManageMemory == v.m_LetArrayManageMemory);
    using std::swap;
    swap(v.m_Data       , m_Data);
    swap(v.m_NumElements, m_NumElements);
    }

#if defined(ITK_HAS_CXX11_RVREF)
  /** C++11 Move Constructor.
   * \post \c v is destructible and assignable.
   * \post `m_NumElements == 0`
   * \post `m_LetArrayManageMemory == true`
   * \post `m_Data == nullptr`
   * \post Built object contains old \c v data.
   */
  VariableLengthVector(Self && v) ITK_NOEXCEPT;

  /** C++11 Move assignement operator.
   * \pre \c v shall not be the same as the current object
   * \post \c v is destructible and assignable.
   * \post `m_NumElements == 0`
   * \post `m_LetArrayManageMemory == true`
   * \post `m_Data == nullptr`
   * \post Current object contains old \c v data.
   */
  Self & operator=(Self && v) ITK_NOEXCEPT;
#endif

  /** Constructor from an Expression Template vector.
   * \tparam TExpr1 Type of the left sub-expression
   * \tparam TExpr2 Type of the right sub-expression
   * \tparam TBinaryOp Binary Operation to apply to both sub-expressions.
   * \param[in] rhs Non evaluated Expression Template.
   *
   * Builds the new \c VariableLengthVector with an expression template. The
   * code loops over all components from the template expression, and evaluates
   * them on the fly to fill the content of the new vector.
   *
   * \post \c m_Data is not null and points to an array of \c m_NumElements,
   * even if \c m_NumElements is 0
   * \post `*this == rhs`
   * \post \c m_NumElements is \c rhs.GetSize()
   * \post \c m_LetArrayManageMemory is true
   */
  template <typename TExpr1, typename TExpr2, typename  TBinaryOp>
      VariableLengthVector(VariableLengthVectorExpression<TExpr1, TExpr2, TBinaryOp> const& rhs);
  /** Assignment from an Expression Template vector.
   * \tparam TExpr1 Type of the left sub-expression
   * \tparam TExpr2 Type of the right sub-expression
   * \tparam TBinaryOp Binary Operation to apply to both sub-expressions.
   * \param[in] rhs Non evaluated Expression Template.
   *
   * Resets the new \c VariableLengthVector with an expression template. The
   * code loops over all components from the template expression, and evaluates
   * them on the fly to fill the content of the current vector.
   *
   * \post if called on a \c VariableLengthVector proxy, the referenced values
   * are left unchanged.
   * \post \c m_Data is not null and points to an array of \c m_NumElements,
   * if \c m_NumElements is not 0. \c m_Data may be null otherwise (an empty
   * vector is assigned into another empty vector)
   * \post \c m_LetArrayManageMemory is true
   * \post `GetSize() == rhs.GetSize()`
   * \post `*this == rhs`
   */
  template <typename TExpr1, typename TExpr2, typename  TBinaryOp>
  Self & operator=(VariableLengthVectorExpression<TExpr1, TExpr2, TBinaryOp> const& rhs);

  /** Set the all the elements of the array to the specified value.
   * \pre This function may be called on empty vectors, it's a no-op.
   */
  void Fill(TValue const & v);

  /** Converting assignment operator.
   * \note Ensures a <em>String Exception Guarantee</em>: resists to
   * self-assignment, and no changes are made if memory cannot be allocated to
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

  /** Copy-Assignment operator.
   * \note Ensures a <em>String Exception Guarantee</em>: resists to
   * self-assignment, and no changes are made if memory cannot be allocated to
   * hold the new elements. This is expecting \c TValue assignment is a \c
   * noexcept operation.
   *
   * \post if called on a \c VariableLengthVector proxy, the referenced values
   * are left unchanged.
   * \post \c m_Data is not null and points to an array of \c m_NumElements,
   * if \c m_NumElements is not 0. \c m_Data may be null otherwise (an empty
   * vector is assigned into another empty vector)
   * \post \c m_LetArrayManageMemory is true
   * \post <tt>GetSize() == v.GetSize()</tt>
   * \post <tt>*this == v</tt>
   */
  Self & operator=(const Self & v);

  /** Fast Assignment.
   * \pre \c m_LetArrayManageMemory is true: the \c VariableLengthVector is not
   * a proxy, checked with an assertion. Call <tt>SetSize(GetSize(), NeverReallocate(),
   * DumpOldValues())</tt> to ensure a vector is not a proxy anymore.
   * \pre current size is identical to the one from the right hand side
   * operand, checked with an assertion.
   * \pre Doesn't not support empty vectors.
   */
  Self & FastAssign(const Self & v);

  /** Assignment operator from a numeric value.
   * \pre This assumes \c m_LetArrayManageMemory is true, but it is unchecked.
   * If this operator is called on a \c VariableLengthVector proxy, referenced
   * values will be overwritten.
   * \post Elements in `[m_Data, m_Data+GetSize())` will be equal to \c v, modulo
   * precision
   */
  Self & operator=(TValue const & v);

  /** Return the number of elements in the Array  */
  unsigned int Size(void) const { return m_NumElements; }
  unsigned int GetSize(void) const { return m_NumElements; }
  unsigned int GetNumberOfElements(void) const { return m_NumElements; }

  /** Return reference to the element at specified index. No range checking. */
  TValue       & operator[](unsigned int i) { return this->m_Data[i]; }
  /** Return reference to the element at specified index. No range checking. */
  TValue const & operator[](unsigned int i) const { return this->m_Data[i]; }

  /** Get one element */
  const TValue & GetElement(unsigned int i) const { return m_Data[i]; }

  /** Set one element */
  void SetElement(unsigned int i, const TValue & value) { m_Data[i] = value; }

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
   * never be assigned anything, we could benefit from a version that won't
   * check \c m_LetArrayManageMemory.
   *
   * \pre `m_NumElements == sz` if \c TReallocatePolicy is \c NeverReallocate
   * \post `m_NumElements == sz`
   * \post \c m_LetArrayManageMemory is true
   * \post In case of reallocation, old \c m_Data buffer is deleted.
   * \post If \c TKeepValuesPolicy is \c KeepOldValues, old values are
   * garanteed to be kept, otherwise, it'll depend on the reallocation policy
   * and the old and new vector size.
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

  /** Destroy data that is allocated internally, if \c LetArrayManageMemory is
   * true. */
  void DestroyExistingData();

  /** Set the pointer from which the data is imported.
   * If "LetArrayManageMemory" is false, then the application retains
   * the responsibility of freeing the memory for this data.  If
   * "LetArrayManageMemory" is true, then this class will free the
   * memory when this object is destroyed.
   * \warning The size of the new \c data shall match vector current size.
   * Prefer the other overload.
   * \post old \c m_Data is deleted iff \c m_LetArrayManageMemory is true
   * \post `m_Data == data`
   * \post `m_LetArrayManageMemory ==LetArrayManageMemory`
   * \post \c Size() is left unmodified.
   */
  void SetData(TValue *data, bool LetArrayManageMemory = false);

  /** Similar to the previous method. In the above method, the size must be
   * separately set prior to using user-supplied data. This introduces an
   * unnecessary allocation step to be performed. This method avoids it
   * and should be used to import data wherever possible to avoid this.
   * Set the pointer from which the data is imported.
   * If "LetArrayManageMemory" is false, then the application retains
   * the responsibility of freeing the memory for this data.  If
   * "LetArrayManageMemory" is true, then this class will free the
   * memory when this object is destroyed.
   * \post old \c m_Data is deleted iff \c m_LetArrayManageMemory is true
   * \post `m_Data == data`
   * \post `m_LetArrayManageMemory ==LetArrayManageMemory`
   * \post `m_NumElements == sz`
   */
  void SetData(TValue *data, unsigned int sz, bool LetArrayManageMemory = false);

  /** This destructor is not virtual for performance reasons. However, this
   * means that subclasses cannot allocate memory.
   *
   * \internal
   * More precisally, this class has value semantics (copiable, assignable,
   * comparable). It's hardly compatible with public inheritance: slicing would
   * always be there somewhere to annoy us if we try to inherit publicaly from
   * such a class.
   * As a consequence, having the destructor virtual makes hardly any sense.
   */
  ~VariableLengthVector();

  /** Reserves memory of a certain length.
   *
   * If the array already contains data, the existing data is copied over and
   * new space is allocated, if necessary. If the length to reserve is less
   * than the current number of elements, then an appropriate number of elements
   * are discarded.
   * \post \c m_Data is not null and can hold \c size elements.
   * \post \c m_LetArrayManageMemory may be left unchanged if there already are
   * enough elements.
   *
   * \note You may prefer instead
   * `SetSize(N, DontShrinkToFit(), KeepOldValues());` that ensures that the
   * array is not a proxy at the end of the operation.
   */
  void Reserve(ElementIdentifier size);

  /** Allocate memory of certain size and return it.
   * \return a non-null pointer to an array of \c size elements (0 is a valid
   * parameter).
   */
  TValue * AllocateElements(ElementIdentifier size) const;

  const TValue * GetDataPointer() const { return m_Data; }

  /** Prefix operator that subtracts 1 from each element of the
   * vector. */
  Self & operator--()
    {
    for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
      {
      this->m_Data[i] -= static_cast< ValueType >( 1.0 );
      }
    return *this;
    }

  /** Prefix operator that adds 1 to each element of the vector. */
  Self & operator++() // prefix operator ++v;
    {
    for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
      {
      this->m_Data[i] += static_cast< ValueType >( 1.0 );
      }
    return *this;
    }

  /** Postfix operator that subtracts 1 from each element of the
   * vector. */
  Self operator--(int) // postfix operator v--;
    {
    Self tmp(*this);

    --tmp;
    return tmp;
    }

  /** Postfix operator that adds 1 to each element of the vector. */
  Self operator++(int) // postfix operator v++;
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
   * \throw None
   * \note For efficiency, the length of the vectors is not checked;
   * they are assumed to have the same length. */
  template< typename T >
  Self & operator-=
    (const VariableLengthVector< T > & v)
    {
    itkAssertInDebugAndIgnoreInReleaseMacro( m_NumElements == v.GetSize() );
    for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
      {
      m_Data[i] -= static_cast< ValueType >( v[i] );
      }
    return *this;
    }

  /** Subtract scalar 's' from each element of the current vector. */
  Self & operator-=(TValue s)
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
   * \throw None
   * \note For efficiency, the length of the vectors is not checked;
   * they are assumed to have the same length. */
  template< typename T >
  Self & operator+=
    (const VariableLengthVector< T > & v)
    {
    itkAssertInDebugAndIgnoreInReleaseMacro( m_NumElements == v.GetSize() );
    for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
      {
      m_Data[i] += static_cast< ValueType >( v[i] );
      }
    return *this;
    }

  /** Add scalar 's' to each element of the vector. */
  Self & operator+=(TValue s)
    {
    for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
      {
      m_Data[i] += s;
      }
    return *this;
    }

  /** Compound addition operator with a expression template vector.
   * \tparam TExpr1 Type of the left sub-expression
   * \tparam TExpr2 Type of the right sub-expression
   * \tparam TBinaryOp Binary Operation to apply to both sub-expressions.
   * \param[in] rhs Non evaluated Expression Template.
   *
   * \pre `Size() == rhs.Size()`, checked with an assertion
   * \note The elements of the expression template are evaluated one by one.
   */
  template <typename TExpr1, typename TExpr2, typename TBinaryOp>
  Self& operator+=(VariableLengthVectorExpression<TExpr1,TExpr2,TBinaryOp> const& rhs)
    {
    itkAssertInDebugAndIgnoreInReleaseMacro(rhs.Size() == Size());
    for ( ElementIdentifier i = 0; i < m_NumElements; ++i )
      {
      m_Data[i] += static_cast<ValueType>(rhs[i]);
      }
    return *this;
    }

  /** Compound substraction operator with a expression template vector.
   * \tparam TExpr1 Type of the left sub-expression
   * \tparam TExpr2 Type of the right sub-expression
   * \tparam TBinaryOp Binary Operation to apply to both sub-expressions.
   * \param[in] rhs Non evaluated Expression Template.
   *
   * \pre `Size() == rhs.Size()`, checked with an assertion
   * \note The elements of the expression template are evaluated one by one.
   */
  template <typename TExpr1, typename TExpr2, typename TBinaryOp>
  Self& operator-=(VariableLengthVectorExpression<TExpr1,TExpr2,TBinaryOp> const& rhs)
    {
    itkAssertInDebugAndIgnoreInReleaseMacro(rhs.Size() == Size());
    for ( ElementIdentifier i = 0; i < m_NumElements; ++i )
      {
      m_Data[i] -= static_cast<ValueType>(rhs[i]);
      }
    return *this;
    }

  /** Multiply each element of the vector by a scalar 's'. The scalar
   * value is cast to the current vector element type prior to
   * multiplication.
   * \throw None
   */
  template< typename T >
  Self & operator*=(T s)
    {
    const ValueType & sc = static_cast<ValueType>(s);
    for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
      {
      m_Data[i] *= sc;
      }
    return *this;
    }

  /** Multiply each element of the vector by a scalar 's'.
   * \throw None
   */
  Self & operator*=(TValue s)
    {
    for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
      {
      m_Data[i] *= s;
      }
    return *this;
    }

  /** Divide vector elements by a scalar 's'. The vector does not
   * have to have the same element type as the scalar type. Both the
   * scalar and vector elements are cast to the RealValueType prior to
   * division, and the result is cast to the ValueType.
   * \throw None
   */
  template< typename T >
  Self & operator/=(T s)
    {
    const RealValueType sc = s;
    for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
      {
      m_Data[i] = static_cast< ValueType >(
        static_cast< RealValueType >( m_Data[i] )
        / sc );
      }
    return *this;
    }

  /** Negates each vector element.
   * \warning This operator has a non standard semantics. Instead of returning
   * a new \c VariableLengthVector, it modifies the current object.
   */
  Self & operator-();  // negation operator

  bool operator==(const Self & v) const;

  bool operator!=(const Self & v) const;

  /** Returns vector's Euclidean Norm  */
  RealValueType GetNorm() const;

  /** Returns vector's squared Euclidean Norm  */
  RealValueType GetSquaredNorm() const;

  /** letArrayManageMemory getter. */
  bool IsAProxy() const { return ! m_LetArrayManageMemory;}

private:
  bool              m_LetArrayManageMemory; // if true, the array is responsible
                                            // for memory of data
  TValue *          m_Data;                 // Array to hold data
  ElementIdentifier m_NumElements;
};

/// \cond HIDE_META_PROGRAMMING
namespace mpl {
/** Tells whether a type is an array type for which the support of arithmetic
 * operations is done with Expression Template.
 * \note For the moment, only \c itk::VariableLengthVector<> is supported. It
 * could be extented to other types of ITK arrays.
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 * \sa \c VariableLengthVector
 * \sa \c VariableLengthVectorExpression
 */
template <typename T>
struct IsArray : FalseType {};

/// \cond SPECIALIZATION_IMPLEMENTATION
template <typename T>
struct IsArray<itk::VariableLengthVector<T> > : TrueType {};

template <typename TExpr1, typename TExpr2, typename TBinaryOp>
struct IsArray<VariableLengthVectorExpression<TExpr1, TExpr2,TBinaryOp> > : TrueType {};
/// \endcond
} // namespace mpl
/// \endcond

namespace Details
{
/// \cond HIDE_META_PROGRAMMING
/** Helper Trait for VLV expression template: returns the value type.
 * \tparam TExpr Expression type
 * \return \c Type The value type behind \c TExpr (\c TExpr in case of a
 * numerical type, \c TExpr::ValueType in case of the \c VariableLengthVector,
 * etc.)
 *
 * Also defines \c Load() that permits to fetch the i-th element in case of an
 * array, array expression, or just the number in case of a number.
 * \ingroup ITKCommon
 * \sa \c VariableLengthVector
 * \sa \c VariableLengthVectorExpression
 */
template <typename TExpr> struct GetType
  {
  typedef TExpr Type;
  /** Fetches the i-th element from an array (expression).
   * \note the default unspecialized behaviour returns the input number \c v.
   */
  static Type Load(Type const& v, unsigned int idx)
    { (void)idx; return v; }
  };

/** Helper function for VLV expression templates: returns the common size.
 * \param[in] lhs left hand side expression
 * \param[in] rhs right hand side expression
 * \note The default overload assumes both operands are \c VariableLengthVector
 * (or expression) arrays
 * \pre asserts both arrays have the same size.
 * \ingroup ITKCommon
 * \sa \c VariableLengthVector
 * \sa \c VariableLengthVectorExpression
 */
template <typename TExpr1, typename TExpr2>
inline
typename mpl::EnableIf<mpl::And<mpl::IsArray<TExpr1>, mpl::IsArray<TExpr2> >, unsigned int>::Type
GetSize(TExpr1 const& lhs, TExpr2 const& rhs)
  {
  (void)rhs;
  itkAssertInDebugAndIgnoreInReleaseMacro(lhs.Size() == rhs.Size());
  return lhs.Size();
  }

/// \cond SPECIALIZATION_IMPLEMENTATION
/** Helper function for VLV expression templates: returns the common size.
 * \param[in] lhs left hand side expression
 * \param[in] rhs right hand side expression
 * \note This overload assumes that only the first operand is a \c
 * VariableLengthVector (or expression) array.
 * \ingroup ITKCommon
 * \sa \c VariableLengthVector
 * \sa \c VariableLengthVectorExpression
 */
template <typename TExpr1, typename TExpr2>
inline
typename mpl::EnableIf<mpl::And<mpl::IsArray<TExpr1>, mpl::Not<mpl::IsArray<TExpr2> > >, unsigned int>::Type
GetSize(TExpr1 const& lhs, TExpr2 const& itkNotUsed(rhs))
  {
  return lhs.Size();
  }

/** Helper function for VLV expression templates: returns the common size.
 * \param[in] lhs left hand side expression
 * \param[in] rhs right hand side expression
 * \note This overload assumes that only the second operand is a \c
 * VariableLengthVector (or expression) array.
 * \ingroup ITKCommon
 * \sa \c VariableLengthVector
 * \sa \c VariableLengthVectorExpression
 */
template <typename TExpr1, typename TExpr2>
inline
typename mpl::EnableIf<mpl::And<mpl::IsArray<TExpr2>, mpl::Not<mpl::IsArray<TExpr1> > >, unsigned int>::Type
GetSize(TExpr1 const& itkNotUsed(lhs), TExpr2 const& rhs)
  {
  return rhs.Size();
  }

template <typename T>
struct GetType<VariableLengthVector<T> >
  {
  typedef T Type;
  static Type Load(VariableLengthVector<T> const& v, unsigned int idx)
    { return v[idx]; }
  };
template <typename TExpr1, typename TExpr2, typename TBinaryOp>
struct GetType<VariableLengthVectorExpression<TExpr1, TExpr2, TBinaryOp> >
  {
  typedef typename VariableLengthVectorExpression<TExpr1, TExpr2, TBinaryOp>::ResType Type;
  static Type Load(VariableLengthVectorExpression<TExpr1, TExpr2, TBinaryOp> const& v, unsigned int idx)
    { return v[idx]; }
  };
/// \endcond

namespace op
{
/** Tells whether objects from two types can be added or substracted.
 * The operation is authorized if and only if:
 * - both are arrays,
 * - or one operand is an array while the second is a number.
 * \note As this traits is dedicated to help overload binary operators, it
 * shall not be used to help overload `operator+()` between floats for instance.
 * Hence, the case where both operands are numbers is rejected.
 *
 * \sa \c mpl::IsArray<> to know the exact array types recognized as \em array by this traits
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 */
template <typename TExpr1, typename TExpr2>
struct CanBeAddedOrSubstracted
: mpl::Or< mpl::And<mpl::IsArray<TExpr1>, mpl::IsArray<TExpr2> >,
            mpl::And<mpl::IsArray<TExpr1>, mpl::IsNumber<TExpr2> >,
            mpl::And<mpl::IsNumber<TExpr1>, mpl::IsArray<TExpr2> >
  >
{};

/** Tells whether objects from two types can be multiplied.
 * The operation is authorized if and only if:
 * - one operand is an array while the second is a number.
 * \note As this traits is dedicated to help overload `operator*()`, it
 * shall not be used to help overload the operator between floats for instance.
 * Hence, the case where both operands are numbers is rejected.
 *
 * \sa \c mpl::IsArray<> to know the exact array types recognized as \em array by this traits
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 */
template <typename TExpr1, typename TExpr2>
struct CanBeMultiplied
: mpl::Or< mpl::And<mpl::IsArray<TExpr1>, mpl::IsNumber<TExpr2> >,
            mpl::And<mpl::IsNumber<TExpr1>, mpl::IsArray<TExpr2> >
  >
{};

/** Tells whether objects from two types can be multiplied.
 * The operation is authorized if and only if:
 * - the first operand is an array while the second is a number.
 * \note As this traits is dedicated to help overload `operator/()`, it
 * shall not be used to help overload the operator between floats for instance.
 * Hence, the case where both operands are numbers is rejected.
 *
 * \sa \c mpl::IsArray<> to know the exact array types recognized as \em array by this traits
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 */
template <typename TExpr1, typename TExpr2>
struct CanBeDivided
: mpl::And<mpl::IsArray<TExpr1>, mpl::IsNumber<TExpr2> >
{};

} // op namespace
} // Details namespace
/// \endcond

/** Expression Template for \c VariableLengthVector.
 * Contains an expression template that models a binary operation between two
 * sub expressions (of type \c VariableLengthVector, or \c VariableLengthVectorExpression)
 * \tparam TExpr1 Type of the left sub-expression
 * \tparam TExpr2 Type of the right sub-expression
 * \tparam TBinaryOp Binary Operation to apply to both sub-expressions.
 *
 * \note We permit to add a `VariableLengthVector<float>` with a
 * `VariableLengthVector<double>`, the result will be of type
 * `VariableLengthVector<double>`.
 *
 * \warning Explicitly static casting an expression to a
 * \c VariableLengthVector<> will defeat the purpose of the optimization
 * implemented here. It's thus best to let the expression automatically adjust
 * to the type with the most precision.
 * Eventually, when assigning to the final destination (a
 * \c VariableLengthVector<>), a casting on-the-fly could be realized by the
 * assignment operator, or by the copy constructor.
 *
 * \todo Add support for unary operations like `operator-()`.
 *
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template <typename TExpr1, typename TExpr2, typename TBinaryOp>
struct VariableLengthVectorExpression
{
  VariableLengthVectorExpression(TExpr1 const& lhs, TExpr2 const& rhs)
    : m_lhs(lhs), m_rhs(rhs)
    {
    // Not neccessary actually as end-user/developper is not expected to
    // provide new BinaryOperations
    itkStaticAssert(
      (itk::mpl::IsBaseOf<Details::op::BinaryOperationConcept, TBinaryOp>::Value),
      "The Binary Operation shall inherit from BinaryOperationConcept");
    }

  /// Returns the size of the vector expression.
  unsigned int Size() const{ return Details::GetSize(m_lhs, m_rhs); }

  /// Vector type of the Result Expression
  typedef typename mpl::PromoteType<
    typename Details::GetType<TExpr1>::Type,
    typename Details::GetType<TExpr2>::Type>::Type     ResType;
  /// Real type of the elements
  typedef typename NumericTraits< ResType > ::RealType RealValueType;

  /** Element access operator.
   * \pre `idx < Size()`
   * \internal
   * This is where the magic happens. Instead of building a new vector based on
   * the two input vectors, we compute each element on-the-fly when
   * requested(by a \c VariableLengthVector constructor or an assignment
   * operator).
   *
   * \c Load() is in charge of fetching the i-th element of the sub-expressions
   */
  ResType operator[](unsigned int idx) const
    {
    itkAssertInDebugAndIgnoreInReleaseMacro(idx < Size());
    return TBinaryOp::Apply(
      Details::GetType<TExpr1>::Load(m_lhs, idx),
      Details::GetType<TExpr2>::Load(m_rhs, idx));
    }

  /** Returns vector's Euclidean Norm  */
  RealValueType GetNorm() const;

  /** Returns vector's squared Euclidean Norm  */
  RealValueType GetSquaredNorm() const;

private:
  TExpr1 const& m_lhs;
  TExpr2 const& m_rhs;
};

/** Addition involving a \c VariableLengthVector.
 * This operation is generic and takes:
 * - two arrays,
 * - or one array and one number (on either side)
 * \return an expression template proxy object.
 * \throw None As no allocation will be performed.
 * \relates itk::VariableLengthVector
 * \sa \c mpl::IsArray<> to know the exact array types recognized as \em array by this traits
 */
template <typename TExpr1, typename TExpr2>
inline
typename mpl::EnableIf<Details::op::CanBeAddedOrSubstracted<TExpr1,TExpr2>, VariableLengthVectorExpression<TExpr1, TExpr2, Details::op::Plus> >::Type
operator+(TExpr1 const& lhs, TExpr2 const& rhs)
{ return VariableLengthVectorExpression<TExpr1, TExpr2, Details::op::Plus>(lhs, rhs); }

/** Substraction involving a \c VariableLengthVector.
 * This operation is generic and takes:
 * - two arrays,
 * - or one array and one number (on either side)
 * \return an expression template proxy object.
 * \throw None As no allocation will be performed.
 * \relates itk::VariableLengthVector
 * \sa \c mpl::IsArray<> to know the exact array types recognized as \em array by this traits
 */
template <typename TExpr1, typename TExpr2>
inline
typename mpl::EnableIf<Details::op::CanBeAddedOrSubstracted<TExpr1,TExpr2>, VariableLengthVectorExpression<TExpr1, TExpr2, Details::op::Sub> >::Type
operator-(TExpr1 const& lhs, TExpr2 const& rhs)
{ return VariableLengthVectorExpression<TExpr1, TExpr2, Details::op::Sub>(lhs, rhs); }

/** Multiplication between a \c VariableLengthVector and a scalar.
 * This operation is generic and takes one array and one number (on either
 * side).
 * \return an expression template proxy object.
 * \throw None As no allocation will be performed.
 * \relates itk::VariableLengthVector
 * \sa \c mpl::IsArray<> to know the exact array types recognized as \em array by this traits
 */
template <typename TExpr1, typename TExpr2>
inline
typename mpl::EnableIf<Details::op::CanBeMultiplied<TExpr1,TExpr2>, VariableLengthVectorExpression<TExpr1, TExpr2, Details::op::Mult> >::Type
operator*(TExpr1 const& lhs, TExpr2 const& rhs)
{ return VariableLengthVectorExpression<TExpr1, TExpr2, Details::op::Mult>(lhs, rhs); }

/** Division of a \c VariableLengthVector by a scalar.
 * This operation is generic and takes one array and one number.
 * \return an expression template proxy object.
 * \throw None As no allocation will be performed.
 * \relates itk::VariableLengthVector
 * \sa \c mpl::IsArray<> to know the exact array types recognized as \em array by this traits
 */
template <typename TExpr1, typename TExpr2>
inline
typename mpl::EnableIf<Details::op::CanBeDivided<TExpr1,TExpr2>, VariableLengthVectorExpression<TExpr1, TExpr2, Details::op::Div> >::Type
operator/(TExpr1 const& lhs, TExpr2 const& rhs)
{ return VariableLengthVectorExpression<TExpr1, TExpr2, Details::op::Div>(lhs, rhs); }

/** Serialization of \c VariableLengthVectorExpression
 * \relates itk::VariableLengthVectorExpression
 */
template <typename TExpr1, typename TExpr2, typename  TBinaryOp>
std::ostream & operator<<(std::ostream &os, VariableLengthVectorExpression<TExpr1, TExpr2, TBinaryOp> const& v)
{
  os << "[";
  if (v.Size() != 0)
    {
    os << v[0];
    for (unsigned int i = 1, N = v.Size(); i != N; ++i)
      {
      os << ", " << v[i];
      }
    }
  return os << "]";
}

/** Returns vector's Euclidean Norm.
 * \tparam TExpr must be an array
 * \sa \c mpl::IsArray<> to know the exact array types recognized as \em array by this traits
 * \relates itk::VariableLengthVectorExpression
 */
template <typename TExpr>
inline
typename mpl::EnableIf<mpl::IsArray<TExpr>, typename TExpr::RealValueType>::Type
GetNorm(TExpr const& v)
{
  return static_cast<typename TExpr::RealValueType>(
    std::sqrt(static_cast<double>(GetSquaredNorm(v))));
}

/** Returns vector's squared Euclidean Norm.
 * \tparam TExpr must be an array
 * \sa \c mpl::IsArray<> to know the exact array types recognized as \em array by this traits
 * \relates itk::VariableLengthVectorExpression
 */
template <typename TExpr>
inline
typename mpl::EnableIf<mpl::IsArray<TExpr>, typename TExpr::RealValueType>::Type
GetSquaredNorm(TExpr const& v)
{
  typedef typename TExpr::RealValueType RealValueType;
  RealValueType sum = 0.0;
  for ( unsigned int i = 0, N=v.Size(); i < N; ++i )
    {
    const RealValueType value = v[i];
    sum += value * value;
    }
  return sum;
}

/**\name Serialization */
//@{
/** Serialization of \c VariableLengthVector
 * \relates itk::VariableLengthVector
 */
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
//@}

/**\name Standard compliance functions */
//@{
/** \c swap() overload for \c VariableLengthVector
 * \throw None
 * \relates itk::VariableLengthVector
 * \internal
 * This overload follows C++ standard naming convention. This is required to
 * permit \c VariableLengthVector to be exchanged by standard algorithms that
 * take advantage of Koening Namespace Lookup (a.k.a. Argument Dependant
 * Lookup). e.g.
 \code
 template <typename T> f(T & l, T & r)
 {
     using std::swap;
     swap(l,r);
     ...
 }
 * \endcode
 */
template <typename T>
inline
void swap(VariableLengthVector<T> &l_, VariableLengthVector<T> &r_) ITK_NOEXCEPT
{
  l_.Swap(r_);
}
//@}

} // namespace itk

#include "itkNumericTraitsVariableLengthVectorPixel.h"

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVariableLengthVector.hxx"
#endif

#endif
