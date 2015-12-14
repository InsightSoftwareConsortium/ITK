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

#ifndef itkFancyString_hxx
#define itkFancyString_hxx

#include "itkFancyString.h"
#include "itkStringTools.h"

namespace itk
{

/////////////////////////////////////////////////////////////////////////////
// manipulators for FancyString
/////////////////////////////////////////////////////////////////////////////

/**
 * Definition of FancyString-based manipulators without parameters.
 */
inline FancyString&
operator<<( FancyString& s, void (*mf)(FancyString&) )
{
  (*mf)( s );
  return s;
}

/**
 * Manipulator to clear all characters in a FancyString.
 */
inline void
ClearContent( FancyString& input )
{
  input.ClearContent();
}

/////////////////////////////////////////////////////////////////////////////
// helper functions for converting a string to/from a std::vector
/////////////////////////////////////////////////////////////////////////////

/**
 * Function to convert a string to a vector of type std::vector<T>.
 * Number of elements to read is given by the parameter 'count':
 *   = 0, get the number of elements to read from data.size();
 *   > 0, read number of 'count' elements, and resize the data if necessary;
 *   < 0, default value (-1), automatically compute the number from the input stream.
 *
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
FancyString&
FancyString::ToData( std::vector<T>& outputData, int count )
{
  StringTools::ToData( this->m_Value, outputData, count );
  return *this;
}

/**
 * Functions to convert a vector of type std::vector<T> to a string.
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
FancyString&
FancyString::FromData( const std::vector<T>& inputData )
{
  StringTools::FromData( this->m_Value, inputData );
  return *this;
}

/////////////////////////////////////////////////////////////////////////////
// helper functions for converting a string to/from a itk::Array
/////////////////////////////////////////////////////////////////////////////

/**
 * Function to convert a string to an array of type itk::Array<T>.
 * Number of elements to read is given by the parameter 'count':
 *   = 0, get the number of elements to read from data.size();
 *   > 0, read number of 'count' elements, and resize the data if necessary;
 *   < 0, default value (-1), automatically compute the number from the input stream.
 *
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
FancyString&
FancyString::ToData( Array<T>& outputData, int count )
{
  StringTools::ToData( this->m_Value, outputData, count );
  return *this;
}

/**
 * Functions to convert an array of type itk::Array<T> to a string.
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
FancyString&
FancyString::FromData( const Array<T>& inputData )
{
  StringTools::FromData( this->m_Value, inputData );
  return *this;
}

/////////////////////////////////////////////////////////////////////////////
// helper functions for converting a string to/from a basic data type
/////////////////////////////////////////////////////////////////////////////

/**
 * Functions to convert a string to a value of basic data type.
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
FancyString&
FancyString::ToData( T& outputData )
{
  StringTools::ToData( this->m_Value, outputData );
  return *this;
}

/**
 * Functions to convert a value of basic data type to a string.
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
FancyString&
FancyString::FromData( const T& inputData )
{
  StringTools::FromData( this->m_Value, inputData );
  return *this;
}

} // namespace itk

namespace itk
{

/////////////////////////////////////////////////////////////////////////////
// helper functions for converting a string to/from a std::vector
/////////////////////////////////////////////////////////////////////////////

/**
 * Function to convert a string to a vector of type std::vector<T>.
 * Number of elements to read is given by data.size().
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
FancyString& operator>>( FancyString& s, std::vector<T>& data )
{
  return s.ToData( data, 0 );
}

/**
 * Functions to convert a vector of type std::vector<T> to a string.
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
FancyString& operator<<( FancyString& s, const std::vector<T>& data )
{
  return s.FromData( data );
}

/////////////////////////////////////////////////////////////////////////////
// helper functions for converting a string to/from a itk::Array
/////////////////////////////////////////////////////////////////////////////

/**
 * Function to convert a string to an array of type itk::Array<T>.
 * Number of elements to read is given by data.GetSize().
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
FancyString& operator>>( FancyString& s, Array<T>& data )
{
  return s.ToData( data, 0 );
}

/**
 * Functions to convert an array of type itk::Array<T> to a string.
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
FancyString& operator<<( FancyString& s, const Array<T>& data )
{
  return s.FromData( data );
}

/////////////////////////////////////////////////////////////////////////////
// helper functions for converting a string to/from a basic data type
/////////////////////////////////////////////////////////////////////////////

/**
 * Functions to convert a string to a value of basic data type.
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
FancyString& operator>>( FancyString& s, T& data )
{
  return s.ToData( data );
}

/**
 * Functions to convert a value of basic data type to a string.
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
FancyString& operator<<( FancyString& s, const T& data )
{
  return s.FromData( data );
}

} // namespace itk

#endif // itkFancyString_hxx
