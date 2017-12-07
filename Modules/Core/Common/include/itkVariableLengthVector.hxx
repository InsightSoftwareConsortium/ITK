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
#ifndef itkVariableLengthVector_hxx
#define itkVariableLengthVector_hxx

#include "itkNumericTraitsVariableLengthVectorPixel.h"
#include "itkMath.h"
#include <cstring>
#include <cstdlib>
#include "itkIsBaseOf.h"
#include "itkStaticAssert.h"
#include "itkMath.h"

namespace itk
{
/** Default constructor  */
template< typename TValue >
VariableLengthVector< TValue >
::VariableLengthVector():m_LetArrayManageMemory(true),
  m_Data(ITK_NULLPTR),
  m_NumElements(0)
{}

/** Constructor with size */
template< typename TValue >
VariableLengthVector< TValue >
::VariableLengthVector(unsigned int length):
  m_Data(ITK_NULLPTR)
{
  Reserve(length);
  // postcondition(s)
  itkAssertInDebugAndIgnoreInReleaseMacro(m_Data != ITK_NULLPTR);
}

/** Constructor with user specified data */
template< typename TValue >
VariableLengthVector< TValue >
::VariableLengthVector(ValueType *datain, unsigned int sz, bool LetArrayManageMemory):
  m_LetArrayManageMemory(LetArrayManageMemory),
  m_Data(datain),
  m_NumElements(sz)
{}

/** Constructor with user specified data */
template< typename TValue >
VariableLengthVector< TValue >
::VariableLengthVector(const ValueType *datain, unsigned int sz, bool LetArrayManageMemory):
  m_LetArrayManageMemory(LetArrayManageMemory)
{
  m_Data = const_cast< ValueType * >( datain );
  m_NumElements = sz;
}

/** Copy constructor. Overrides the default non-templated copy constructor
 * that the compiler provides */
template< typename TValue >
VariableLengthVector< TValue >
::VariableLengthVector(const VariableLengthVector< TValue > & v)
{
  m_NumElements = v.Size();
  m_LetArrayManageMemory = true;
  if (m_NumElements != 0)
    {
    m_Data = this->AllocateElements(m_NumElements);
    itkAssertInDebugAndIgnoreInReleaseMacro(m_Data != ITK_NULLPTR);
    itkAssertInDebugAndIgnoreInReleaseMacro(v.m_Data != ITK_NULLPTR);
    std::copy(&v.m_Data[0], &v.m_Data[m_NumElements], &this->m_Data[0]);
    }
  else
    {
    m_Data = ITK_NULLPTR;
    }
}

#if defined(ITK_HAS_CXX11_RVREF)
template< typename TValue >
VariableLengthVector< TValue >
::VariableLengthVector(Self && v) ITK_NOEXCEPT
: m_LetArrayManageMemory(v.m_LetArrayManageMemory)
, m_Data                (v.m_Data)
, m_NumElements         (v.m_NumElements)
{
  v.m_LetArrayManageMemory = true;
  v.m_Data                 = ITK_NULLPTR;
  v.m_NumElements          = 0;
}

template< typename TValue >
VariableLengthVector< TValue > &
VariableLengthVector< TValue >
::operator=(Self && v) ITK_NOEXCEPT
{
  itkAssertInDebugAndIgnoreInReleaseMacro(&v != this);

  // Possible cases:
  // - both are proxy
  //   => a shallow assignment is enough
  // - none are proxy
  //   => this->m_Data is released, v content is stolen by *this
  // - v is a proxy, but not *this
  //   => Fall back to usual copy-assignement
  // - *this is a proxy, but not v
  //   => v content is stolen by *this, nothing to delete[]

  if (!IsAProxy() && v.IsAProxy())
    { // Fall back to usual copy-assignment
    return *this = v;
    }

  // Delete old data, when data is stolen
  if (!IsAProxy() && !v.IsAProxy())
    {
    delete[] m_Data;
    }

  // Shallow copy of the information
  m_LetArrayManageMemory = v.m_LetArrayManageMemory;
  m_Data                 = v.m_Data;
  m_NumElements          = v.m_NumElements;

  // Reset v to something assignable and destructible
  // NB: It's not necessary to always reset v. The choice made is to avoid a
  // test
  v.m_LetArrayManageMemory = true;
  v.m_Data                 = ITK_NULLPTR;
  v.m_NumElements          = 0;

  return *this;
}

#endif

template< typename TValue >
template <typename VariableLengthVectorExpression1, typename VariableLengthVectorExpression2, typename  TBinaryOp>
VariableLengthVector< TValue >
::VariableLengthVector(VariableLengthVectorExpression<VariableLengthVectorExpression1, VariableLengthVectorExpression2, TBinaryOp> const& rhs)
{
  m_NumElements = rhs.Size();
  m_LetArrayManageMemory = true;
  m_Data = this->AllocateElements(m_NumElements);
  // allocate Elements post-condition
  itkAssertInDebugAndIgnoreInReleaseMacro(m_Data != ITK_NULLPTR);
  for ( ElementIdentifier i = 0; i < m_NumElements; ++i )
    {
    this->m_Data[i] = static_cast<TValue>(rhs[i]);
    }
}

template< typename TValue >
template <typename VariableLengthVectorExpression1, typename VariableLengthVectorExpression2, typename  TBinaryOp>
VariableLengthVector< TValue > &
VariableLengthVector< TValue >
::operator=(VariableLengthVectorExpression<VariableLengthVectorExpression1, VariableLengthVectorExpression2, TBinaryOp> const& rhs)
{
  ElementIdentifier const N = rhs.Size();
  this->SetSize( N, DontShrinkToFit(), DumpOldValues() );
  for ( ElementIdentifier i = 0; i < N; ++i )
    {
    this->m_Data[i] = static_cast<TValue>(rhs[i]);
    }
  return *this;
}

/** Destructor */
template< typename TValue >
VariableLengthVector< TValue >
::~VariableLengthVector()
{
  // if data exists and we are responsible for its memory, get rid of it..
  if ( m_LetArrayManageMemory )
    {
    delete[] m_Data;
    }
}

/** Reserve memory of certain size for m_Data */
template< typename TValue >
void VariableLengthVector< TValue >
::Reserve(ElementIdentifier size)
{
  if ( m_Data )
    {
    if ( size > m_NumElements )
      {
      TValue *temp = this->AllocateElements(size);
      itkAssertInDebugAndIgnoreInReleaseMacro(temp);
      itkAssertInDebugAndIgnoreInReleaseMacro(m_NumElements == 0 || (m_NumElements>0 && m_Data != ITK_NULLPTR));
      // only copy the portion of the data used in the old buffer
      std::copy(m_Data,
                m_Data+m_NumElements,
                temp);
      if ( m_LetArrayManageMemory )
        {
        delete[] m_Data;
        }
      m_Data = temp;
      m_LetArrayManageMemory = true;
      m_NumElements = size;
      }
    }
  else
    {
    m_Data = this->AllocateElements(size);
    m_NumElements = size;
    m_LetArrayManageMemory = true;
    }
  itkAssertInDebugAndIgnoreInReleaseMacro(m_Data != ITK_NULLPTR);
}

/** Allocate memory of certain size and return it */
template< typename TValue >
TValue *VariableLengthVector< TValue >
::AllocateElements(ElementIdentifier size) const
{
  try
    {
    return new TValue[size];
    }
  catch (...)
    {
    // Intercept std::bad_alloc and any exception thrown from TValue
    // default constructor.
    itkGenericExceptionMacro(<< "Failed to allocate memory of length " << size
                             << " for VariableLengthVector.");
    }
}

/** Set the pointer from which the data is imported.
 * If "LetArrayManageMemory" is false, then the application retains
 * the responsibility of freeing the memory for this data.  If
 * "LetArrayManageMemory" is true, then this class will free the
 * memory when this object is destroyed. Note that you need to explicitly
 * set the number of elements. */
template< typename TValue >
void
VariableLengthVector< TValue >
::SetData(TValue *datain, bool LetArrayManageMemory)
{
  // Free any existing data if we manage its memory
  if ( m_LetArrayManageMemory )
    {
    delete[] m_Data;
    }

  m_LetArrayManageMemory = LetArrayManageMemory;
  m_Data = datain;
}

/** Similar to the previous method. In the above method, the size must be
 * separately set prior to using user-supplied data. This introduces an
 * unnecessary allocation step to be performed. This method avoids it
 * and should be used to import data wherever possible to avoid this.
 * Set the pointer from which the data is imported.
 * If "LetArrayManageMemory" is false, then the application retains
 * the responsibility of freeing the memory for this data.  If
 * "LetArrayManageMemory" is true, then this class will free the
 * memory when this object is destroyed. */
template< typename TValue >
void
VariableLengthVector< TValue >
::SetData(TValue *datain, unsigned int sz, bool LetArrayManageMemory)
{
  // Free any existing data if we manage its memory
  if ( m_LetArrayManageMemory )
    {
    delete[] m_Data;
    }

  m_LetArrayManageMemory = LetArrayManageMemory;
  m_Data = datain;
  m_NumElements = sz;
}


template< typename TValue >
void VariableLengthVector< TValue >
::DestroyExistingData()
{
  // Free any existing data if we manage its memory.
  if ( m_LetArrayManageMemory )
    {
    delete[] m_Data;
    }

  m_Data = ITK_NULLPTR;
  m_NumElements = 0;
}

template< typename TValue >
template <typename TReallocatePolicy, typename TKeepValuesPolicy>
void VariableLengthVector< TValue >
::SetSize(unsigned int sz, TReallocatePolicy reallocatePolicy, TKeepValuesPolicy keepValues)
{
  itkStaticAssert(
    (itk::mpl::IsBaseOf<AllocateRootPolicy, TReallocatePolicy>::Value),
    "The allocation policy does not inherit from itk::VariableLengthVector::AllocateRootPolicy as expected");
  itkStaticAssert(
    (itk::mpl::IsBaseOf<KeepValuesRootPolicy, TKeepValuesPolicy>::Value),
    "The old values keeping policy does not inherit from itk::VariableLengthVector::KeepValuesRootPolicy as expected");

  if (reallocatePolicy(sz, m_NumElements) || ! m_LetArrayManageMemory)
    {
    TValue * temp = this->AllocateElements(sz); // may throw
    itkAssertInDebugAndIgnoreInReleaseMacro(temp);
    itkAssertInDebugAndIgnoreInReleaseMacro(m_NumElements == 0 || (m_NumElements > 0  && m_Data != ITK_NULLPTR));
    keepValues(sz, m_NumElements, m_Data, temp); // possible leak if TValue copy may throw
    // commit changes
    if (m_LetArrayManageMemory)
      {
      delete[] m_Data;
      }
    m_Data = temp;
    m_LetArrayManageMemory = true;
    }
  m_NumElements = sz;
}

/** Set the all the elements of the array to the specified value */
template< typename TValue >
void VariableLengthVector< TValue >
::Fill(TValue const & v)
{
  itkAssertInDebugAndIgnoreInReleaseMacro(m_NumElements == 0 || (m_NumElements>0 && m_Data!=ITK_NULLPTR));
  // VC++ version of std::fill_n() expects the output iterator to be valid
  // instead of expecting the range [OutIt, OutIt+n) to be valid.
  std::fill(&this->m_Data[0], &this->m_Data[m_NumElements], v);
}

/** Copy-Assignment operator */
template< typename TValue >
VariableLengthVector< TValue > &
VariableLengthVector< TValue >
::operator=(const Self & v)
{
  // No self assignment test is done. Indeed:
  // - the operator already resists self assignment through a strong exception
  // guarantee
  // - the test becomes a pessimization as we never write "v = v;".
  ElementIdentifier const N = v.Size();
  this->SetSize( N, DontShrinkToFit(), DumpOldValues() );

  // VC++ version of std::copy expects the input range to be valid, and the
  // output iterator as well (as it's a pointer, it's expected non null)
  // Hence the manual loop instead
  itkAssertInDebugAndIgnoreInReleaseMacro(N==0 || this->m_Data != ITK_NULLPTR);
  itkAssertInDebugAndIgnoreInReleaseMacro(N==0 || v.m_Data     != ITK_NULLPTR);
  for (ElementIdentifier i=0; i!=N; ++i)
    {
    this->m_Data[i] = v.m_Data[i];
    }

  itkAssertInDebugAndIgnoreInReleaseMacro(m_LetArrayManageMemory);
  return *this;
}

/** Fast Assignment */
template< typename TValue >
inline
VariableLengthVector< TValue > &
VariableLengthVector< TValue >
::FastAssign(const Self & v)
{
  itkAssertInDebugAndIgnoreInReleaseMacro(this->m_LetArrayManageMemory);
  ElementIdentifier const N = v.Size();
  itkAssertInDebugAndIgnoreInReleaseMacro(N > 0);
  itkAssertInDebugAndIgnoreInReleaseMacro(N == this->Size());
  // Redundant precondition checks
  itkAssertInDebugAndIgnoreInReleaseMacro(v.m_Data     != ITK_NULLPTR);
  itkAssertInDebugAndIgnoreInReleaseMacro(this->m_Data != ITK_NULLPTR);

  std::copy(&v.m_Data[0], &v.m_Data[N], &this->m_Data[0]);

  return *this;
}

/** Assignment operator */
template< typename TValue >
VariableLengthVector< TValue > &
VariableLengthVector< TValue >
::operator=(TValue const & v)
{
  this->Fill(v);
  return *this;
}

template< typename TValue >
VariableLengthVector< TValue > &
VariableLengthVector< TValue >
::operator-()
{
  for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
    {
    m_Data[i] = -m_Data[i];
    }
  return *this;
}

template< typename TValue >
bool
VariableLengthVector< TValue >
::operator==(const Self & v) const
{
  if ( m_NumElements != v.Size() )
    {
    return false;
    }
  for ( ElementIdentifier i = 0; i < m_NumElements; ++i )
    {
    if ( Math::NotExactlyEquals(m_Data[i], v[i]) )
      {
      return false;
      }
    }
  return true;
}

template< typename TValue >
bool
VariableLengthVector< TValue >
::operator!=(const Self & v) const
{
  return ! operator==( v );
}

/**
 * Returns vector's Euclidean Norm
 */
template< typename TValue >
typename VariableLengthVector< TValue >::RealValueType
VariableLengthVector< TValue >
::GetNorm(void) const
{
  using std::sqrt;
  return static_cast<RealValueType>(sqrt( this->GetSquaredNorm() ) );
}

/**
 * Returns vector's Squared Euclidean Norm
 */
template< typename TValue >
typename VariableLengthVector< TValue >::RealValueType
VariableLengthVector< TValue >
::GetSquaredNorm(void) const
{
  RealValueType sum = 0.0;

  for ( ElementIdentifier i = 0; i < this->m_NumElements; ++i )
    {
    const RealValueType value = ( *this )[i];
    sum += value * value;
    }
  return sum;
}

template <typename TExpr1, typename TExpr2, typename  TBinaryOp>
typename VariableLengthVectorExpression<TExpr1, TExpr2, TBinaryOp>::RealValueType
VariableLengthVectorExpression<TExpr1, TExpr2, TBinaryOp>
::GetNorm() const
{
  return itk::GetNorm(*this);
}

template <typename TExpr1, typename TExpr2, typename  TBinaryOp>
typename VariableLengthVectorExpression<TExpr1, TExpr2, TBinaryOp>::RealValueType
VariableLengthVectorExpression<TExpr1, TExpr2, TBinaryOp>
::GetSquaredNorm() const
{
  return itk::GetSquaredNorm(*this);
}

} // namespace itk

#endif
