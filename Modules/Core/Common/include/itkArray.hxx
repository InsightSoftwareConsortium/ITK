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
#ifndef __itkArray_hxx
#define __itkArray_hxx

#include "itkArray.h"
#include "itkNumericTraitsArrayPixel.h"

namespace itk
{
/** Default constructor */
template< typename TValueType >
Array< TValueType >
::Array():vnl_vector< TValueType >()
{
  m_LetArrayManageMemory = true;
}

/** Copy constructor */
template < typename TValueType >
Array<TValueType>
::Array(const Self & rhs)
  : vnl_vector<TValueType>(rhs),
    // The vnl vector copy constructor creates new memory
    // no matter the setting of let array manage memory of rhs
    m_LetArrayManageMemory(true)
{
}

/** Constructor with size */
template< typename TValueType >
Array< TValueType >
::Array(SizeValueType dimension):vnl_vector< TValueType >(dimension)
{
  m_LetArrayManageMemory = true;
}

/** Constructor with user specified data */
template< typename TValueType >
Array< TValueType >
::Array(ValueType *datain, SizeValueType sz, bool LetArrayManageMemory)
{
  vnl_vector< TValueType >::data = datain;
  vnl_vector< TValueType >::num_elmts = sz;
  m_LetArrayManageMemory = LetArrayManageMemory;
}

/** Constructor with user specified data */
template< typename TValueType >
Array< TValueType >
::Array(const ValueType *datain, SizeValueType sz, bool LetArrayManageMemory)
{
  vnl_vector< TValueType >::data = const_cast< TValueType * >( datain );
  // Argh!! Discard constness WRONG.!!
  vnl_vector< TValueType >::num_elmts = sz;
  m_LetArrayManageMemory = LetArrayManageMemory;
}

/** Destructor */
template< typename TValueType >
Array< TValueType >
::~Array()
{
  if ( !m_LetArrayManageMemory )
    {
    vnl_vector< TValueType >::data = 0;
    }
}

/** Set the pointer from which the data is imported.
 * If "LetArrayManageMemory" is false, then the application retains
 * the responsibility of freeing the memory for this data.  If
 * "LetArrayManageMemory" is true, then this class will free the
 * memory when this object is destroyed. */
template< typename TValueType >
void
Array< TValueType >
::SetData(TValueType *datain, bool LetArrayManageMemory)
{
  if ( m_LetArrayManageMemory )
    {
    vnl_vector< TValueType >::destroy();
    }
  vnl_vector< TValueType >::data = datain;
  m_LetArrayManageMemory = LetArrayManageMemory;
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
template< typename TValueType >
void
Array< TValueType >
::SetData(TValueType *datain, SizeValueType sz, bool LetArrayManageMemory)
{
  if ( m_LetArrayManageMemory )
    {
    vnl_vector< TValueType >::destroy();
    }
  vnl_vector< TValueType >::data = datain;
  vnl_vector< TValueType >::num_elmts = sz;
  m_LetArrayManageMemory = LetArrayManageMemory;
}

template< typename TValueType >
void Array< TValueType >
::SetSize(SizeValueType sz)
{
  if ( this->size() != sz )
    {
    // If the array doesn't own the data we do not want to erase it
    // on a resize
    if ( !m_LetArrayManageMemory )
      {
      vnl_vector< TValueType >::data = 0;
      }

    // Call the superclass's set_size
    this->set_size(sz);

    // Size we have allocated new data we need to take
    // responsibility for deleting it
    m_LetArrayManageMemory = true;
    }
}

template< typename TValueType >
const typename Array< TValueType >
::Self &
Array< TValueType >
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

template< typename TValueType >
const typename Array< TValueType >
::Self &
Array< TValueType >
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
