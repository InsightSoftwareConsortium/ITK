/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVariableLengthVector.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkVariableLengthVector_txx
#define _itkVariableLengthVector_txx

#include "itkVariableLengthVector.h"
#include "itkNumericTraitsVariableLengthVectorPixel.h"

namespace itk
{

/** Default constructor  */
template < typename TValueType >
VariableLengthVector<TValueType >
::VariableLengthVector() : 
  m_LetArrayManageMemory( true ),
  m_Data( 0 ),
  m_NumElements( 0 ) {}


/** Constructor with size */
template < typename TValueType >
VariableLengthVector<TValueType >
::VariableLengthVector(unsigned int length) :
  m_LetArrayManageMemory( true ),
  m_Data( 0 )
{
  Reserve( length );
}

/** Constructor with user specified data */
template < typename TValueType >
VariableLengthVector<TValueType >
::VariableLengthVector( ValueType *datain, unsigned int sz, bool LetArrayManageMemory) :
  m_LetArrayManageMemory( LetArrayManageMemory ),
  m_Data( datain ),
  m_NumElements( sz ) {}

/** Constructor with user specified data */
template < typename TValueType >
VariableLengthVector<TValueType >
::VariableLengthVector( const ValueType *datain, unsigned int sz, bool LetArrayManageMemory)
  : m_LetArrayManageMemory( LetArrayManageMemory )
{
  m_Data = const_cast< ValueType * >(datain);
  m_NumElements = sz;
}

/** Copy constructer.. Override the default non-templated copy constructor
 * that the compiler provides */
template < typename TValueType >
VariableLengthVector<TValueType >
::VariableLengthVector(const VariableLengthVector< TValueType > & v)
{
  m_NumElements = v.Size();
  m_Data = this->AllocateElements(m_NumElements);
  m_LetArrayManageMemory = true;
  for( ElementIdentifier i=0; i< v.Size(); i++ )
    {
    this->m_Data[i] = v[i];
    }
}


/** Destructor*/
template < typename TValueType >
VariableLengthVector<TValueType >
::~VariableLengthVector()
{
  // if data exists and we are responsible for its memory, get rid of it.. 
  if(m_LetArrayManageMemory && m_Data)
    {
    delete [] m_Data;
    }
}

/** Reserve memory of certain size for m_Data */
template < typename TValueType >
void VariableLengthVector<TValueType >
::Reserve( ElementIdentifier size )
{
  if( m_Data )
    {
    if( size> m_NumElements )
      { 
      TValueType *temp = this->AllocateElements( size );
      // only copy the portion of the data used in the old buffer
      memcpy(temp, m_Data, m_NumElements*sizeof(TValueType));
      if (m_Data && m_LetArrayManageMemory)
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
template < typename TValueType >
TValueType * VariableLengthVector< TValueType >
::AllocateElements( ElementIdentifier size ) const
{
  TValueType *data;
  try 
    {
    data = new TValueType[ size ];
    }
  catch(...)
    {
    data = 0;
    }
  if( !data )
    {
    itkGenericExceptionMacro( << "Failed to allocate memory of length " << size 
                              << " for VariableLengthVector.");
    }
  return data;
}


/** Set the pointer from which the data is imported.
 * If "LetArrayManageMemory" is false, then the application retains
 * the responsibility of freeing the memory for this data.  If
 * "LetArrayManageMemory" is true, then this class will free the
 * memory when this object is destroyed. Note that you need to explicitly
 * set the number of elements.*/
template < typename TValueType >
void 
VariableLengthVector<TValueType >
::SetData(TValueType* datain,bool LetArrayManageMemory)
{
  // Free any existing data if we manage its memory
  if(m_LetArrayManageMemory && m_Data)   
    {
    delete [] m_Data;
    }  
  
  m_LetArrayManageMemory = LetArrayManageMemory;
  m_Data = datain;
}


/** Similar to the previous method. In the above method, the size must be 
 * seperately set prior to using user-supplied data. This introduces an
 * unnecessary allocation step to be performed. This method avoids it 
 * and should be used to import data whereever possible to avoid this.
 * Set the pointer from which the data is imported.
 * If "LetArrayManageMemory" is false, then the application retains
 * the responsibility of freeing the memory for this data.  If
 * "LetArrayManageMemory" is true, then this class will free the
 * memory when this object is destroyed. */
template < typename TValueType >
void 
VariableLengthVector<TValueType >
::SetData(TValueType* datain, unsigned int sz, bool LetArrayManageMemory)
{
  // Free any existing data if we manage its memory
  if(m_LetArrayManageMemory && m_Data)
    {
    delete [] m_Data;
    }

  m_LetArrayManageMemory = LetArrayManageMemory;
  m_Data = datain;
  m_NumElements = sz;
}


template < typename TValueType >
void VariableLengthVector<TValueType >
::SetSize(unsigned int sz, bool destroyExistingData)
{
  if( destroyExistingData )
    {
    // Free any existing data if we manage its memory and if we need to destroy
    if(!m_LetArrayManageMemory)
      {
      m_Data=0;
      m_NumElements = 0;
      }
    else if( m_Data )
      {
      if( (m_NumElements != sz))
        {
        if(m_NumElements>0)
          {
          delete [] m_Data;
          m_Data = 0;
          }
        }
      else return;
      }
    }

  if ( m_NumElements != sz )
    {
    Reserve( sz );
    }
}

/** Set the all the elements of the array to the specified value */
template < typename TValueType > 
void VariableLengthVector<TValueType >
::Fill (TValueType const& v) 
{
  for( ElementIdentifier i=0; i< m_NumElements; i++ )
    {
    this->m_Data[i] = v;
    }
}

/** Assignment operator  **/
template < typename TValueType > const VariableLengthVector< TValueType >& 
VariableLengthVector<TValueType >
::operator=(const Self & v)
{
  if( this == &v )
    {
    return *this;
    }
  this->SetSize( v.Size());
  for( ElementIdentifier i=0; i< v.Size(); i++ )
    {
    this->m_Data[i] = v[i];
    }
  return *this;
}

template < typename TValueType > VariableLengthVector< TValueType >& 
VariableLengthVector<TValueType >
::operator- ()
{
  for( ElementIdentifier i=0; i< m_NumElements; i++ )
    {
    m_Data[i] = -m_Data[i];
    }
  return *this;
}

template < typename TValueType > bool 
VariableLengthVector<TValueType >
::operator==( const Self & v) const
{
  if( m_NumElements != v.Size() ) 
    {
    return false;
    }
  for( ElementIdentifier i=0; i< m_NumElements; i++ )
    {
    if( m_Data[i] != v[i] ) 
      { 
      return false; 
      }
    }
  return true;
}

template < typename TValueType > bool 
VariableLengthVector<TValueType >
::operator!=( const Self & v) const
{
  if( m_NumElements != v.Size() ) 
    {
    return true;
    }
  for( ElementIdentifier i=0; i< m_NumElements; i++ )
    {
    if( m_Data[i] != v[i] ) 
      { 
      return true; 
      }
    }
  return false;
}

/**
 * Returns vector's Squared Euclidean Norm
 */
template < typename TValueType > 
typename VariableLengthVector< TValueType >::RealValueType
VariableLengthVector<TValueType >
::GetSquaredNorm( void ) const
{
  RealValueType sum = 0.0;
  for( unsigned int i=0; i< this->m_NumElements; i++) 
    {
    const RealValueType value = (*this)[i];
    sum += value * value;
    }
  return sum;
}

} // namespace itk

#endif
