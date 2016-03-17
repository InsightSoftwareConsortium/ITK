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
#ifndef itkArray_hxx
#define itkArray_hxx

#include "itkArray.h"
#include "itkNumericTraitsArrayPixel.h"

namespace itk
{
/** Default constructor */
template< typename TValue >
Array< TValue >
::Array():vnl_vector< TValue >()
{
  m_LetArrayManageMemory = true;
}

/** Copy constructor */
template < typename TValue >
Array<TValue>
::Array(const Self & rhs)
  : vnl_vector<TValue>(rhs),
    // The vnl vector copy constructor creates new memory
    // no matter the setting of let array manage memory of rhs
    m_LetArrayManageMemory(true)
{
}

/** Constructor with size */
template< typename TValue >
Array< TValue >
::Array(SizeValueType dimension)
  : vnl_vector< TValue >(dimension),
    // The vnl vector copy constructor creates new memory
    // no matter the setting of let array manage memory of rhs
    m_LetArrayManageMemory(true)
{
}

/** Constructor with user specified data */
template< typename TValue >
Array< TValue >
::Array(ValueType *datain, SizeValueType sz, bool LetArrayManageMemory):
  m_LetArrayManageMemory(LetArrayManageMemory)
{
  vnl_vector< TValue >::data = datain;
  vnl_vector< TValue >::num_elmts = sz;
}

#if defined ( ITK_FUTURE_LEGACY_REMOVE )
/** Constructor with user specified const data */
template< typename TValue >
Array< TValue >
::Array(const ValueType *datain, SizeValueType sz):
  vnl_vector< TValue >( datain, sz),
  // The vnl vector copy constructor creates new memory
  // no matter the setting of let array manage memory of rhs
  m_LetArrayManageMemory(true)
{
}

#else // defined ( ITK_FUTURE_LEGACY_REMOVE )
/** Constructor with user specified const data */
template< typename TValue >
Array< TValue >
::Array(const ValueType *datain, SizeValueType sz, bool /* LetArrayManageMemory */):
  /* NOTE: The 3rd argument "LetArrayManageMemory, was never valid to use, but is
   * preserved to maintain backwards compatibility*/
  vnl_vector< TValue >( datain, sz),
  // The vnl vector copy constructor creates new memory
  // no matter the setting of let array manage memory of rhs
  m_LetArrayManageMemory(true)
{
}
#endif


/** Destructor */
template< typename TValue >
Array< TValue >
::~Array()
{
  if ( !m_LetArrayManageMemory )
    {
    vnl_vector< TValue >::data = ITK_NULLPTR;
    }
}

template< typename TValue >
void
Array< TValue >
::SetDataSameSize(TValue *datain, bool LetArrayManageMemory)
{
  if ( m_LetArrayManageMemory )
    {
    vnl_vector< TValue >::destroy();
    }
  vnl_vector< TValue >::data = datain;
  // NOTE: Required to have same size vnl_vector< TValue >::num_elmts = sz;
  m_LetArrayManageMemory = LetArrayManageMemory;
}

template< typename TValue >
void
Array< TValue >
::SetData(TValue *datain, SizeValueType sz, bool LetArrayManageMemory)
{
  if ( m_LetArrayManageMemory )
    {
    vnl_vector< TValue >::destroy();
    }
  vnl_vector< TValue >::data = datain;
  vnl_vector< TValue >::num_elmts = sz;
  m_LetArrayManageMemory = LetArrayManageMemory;
}

template< typename TValue >
void Array< TValue >
::SetSize(SizeValueType sz)
{
  if ( this->size() != sz )
    {
    // If the array doesn't own the data we do not want to erase it
    // on a resize
    if ( !m_LetArrayManageMemory )
      {
      vnl_vector< TValue >::data = ITK_NULLPTR;
      }

    // Call the superclass's set_size
    this->set_size(sz);

    // Size we have allocated new data we need to take
    // responsibility for deleting it
    m_LetArrayManageMemory = true;
    }
}

template< typename TValue >
const typename Array< TValue >
::Self &
Array< TValue >
::operator=(const Self & rhs)
{
  if ( this != &rhs )
    {

    // Set the size the same as rhs.
    // The SetSize method takes care of who is responsible
    // for memory management
    //
    this->SetSize( rhs.GetSize() );

    // Call the superclass implementation
    this->VnlVectorType::operator=(rhs);
    }
  return *this;
}

template< typename TValue >
const typename Array< TValue >
::Self &
Array< TValue >
::operator=(const VnlVectorType & rhs)
{
  if ( this != &rhs )
    {

    // Set the size the same as rhs.
    // The SetSize method takes care of who is responsible
    // for memory management
    //
    this->SetSize( rhs.size() );

    // Call the superclass implementation
    this->VnlVectorType::operator=(rhs);
    }
  return *this;
}
} // namespace itk

#endif
