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
#include "vnl/vnl_math.h"
#include <cstring>
#include <cstdlib>

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
  m_LetArrayManageMemory(true),
  m_Data(ITK_NULLPTR)
{
  Reserve(length);
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

/** Copy constructer.. Override the default non-templated copy constructor
 * that the compiler provides */
template< typename TValue >
VariableLengthVector< TValue >
::VariableLengthVector(const VariableLengthVector< TValue > & v)
{
  m_NumElements = v.Size();
  m_Data = this->AllocateElements(m_NumElements);
  m_LetArrayManageMemory = true;
  for ( ElementIdentifier i = 0; i < v.Size(); i++ )
    {
    this->m_Data[i] = v[i];
    }
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
}

/** Allocate memory of certain size and return it */
template< typename TValue >
TValue *VariableLengthVector< TValue >
::AllocateElements(ElementIdentifier size) const
{
  TValue *data;

  try
    {
    data = new TValue[size];
    }
  catch ( ... )
    {
    data = ITK_NULLPTR;
    }
  if ( !data )
    {
    itkGenericExceptionMacro(<< "Failed to allocate memory of length " << size
                             << " for VariableLengthVector.");
    }
  return data;
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
  if ( !m_LetArrayManageMemory )
    {
    m_Data = ITK_NULLPTR;
    m_NumElements = 0;
    return;
    }

  if ( m_Data )
    {
    if ( m_NumElements > 0 )
      {
      delete[] m_Data;
      m_Data = ITK_NULLPTR;
      m_NumElements = 0;
      }
    }
}

template< typename TValue >
void VariableLengthVector< TValue >
::SetSize(unsigned int sz, bool destroyExistingData)
{
  if ( destroyExistingData )
    {
    this->DestroyExistingData();
    }

  if ( !m_Data )
    {
    m_Data = this->AllocateElements(sz);
    m_NumElements = sz;
    m_LetArrayManageMemory = true;
    return;
    }

  TValue *temp = this->AllocateElements(sz);

  if ( sz > m_NumElements )
    {
    // only copy the portion of the data used in the old buffer
    std::copy(m_Data,
              m_Data+m_NumElements,
              temp);
    }
  else
    {
    // only copy elements 0...size-1
    std::copy(m_Data,
              m_Data+sz,
              temp);
    }

  if ( m_LetArrayManageMemory )
    {
    delete[] m_Data;
    }

  m_Data = temp;
  m_LetArrayManageMemory = true;
  m_NumElements = sz;
}

/** Set the all the elements of the array to the specified value */
template< typename TValue >
void VariableLengthVector< TValue >
::Fill(TValue const & v)
{
  for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
    {
    this->m_Data[i] = v;
    }
}

/** Assignment operator */
template< typename TValue >
const VariableLengthVector< TValue > &
VariableLengthVector< TValue >
::operator=(const Self & v)
{
  if ( this != &v )
    {
    this->SetSize( v.Size() );
    for ( ElementIdentifier i = 0; i < v.Size(); i++ )
      {
      this->m_Data[i] = v[i];
      }
    }
  return *this;
}

/** Assignment operator */
template< typename TValue >
const VariableLengthVector< TValue > &
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
  for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
    {
    if ( m_Data[i] != v[i] )
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
  if ( m_NumElements != v.Size() )
    {
    return true;
    }
  for ( ElementIdentifier i = 0; i < m_NumElements; i++ )
    {
    if ( m_Data[i] != v[i] )
      {
      return true;
      }
    }
  return false;
}

/**
 * Returns vector's Euclidean Norm
 */
template< typename TValue >
typename VariableLengthVector< TValue >::RealValueType
VariableLengthVector< TValue >
::GetNorm(void) const
{
  return (RealValueType)( std::sqrt( double( this->GetSquaredNorm() ) ) );
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

  for ( unsigned int i = 0; i < this->m_NumElements; i++ )
    {
    const RealValueType value = ( *this )[i];
    sum += value * value;
    }
  return sum;
}
} // namespace itk

#endif
