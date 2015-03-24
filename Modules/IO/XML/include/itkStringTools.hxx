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

#ifndef itkStringTools_hxx
#define itkStringTools_hxx

#include "itkStringTools.h"

#include <sstream>

namespace itk
{

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
std::string&
StringTools::ToData( std::string& s, std::vector<T>& data, int count )
{
  std::istringstream iss( s, std::istringstream::in );
  iss.exceptions( iss.failbit | iss.badbit );

  if ( count < 0 )
    {
    // compute the number of elements to be read from the input stream
    try
      {
      while ( !iss.eof() ) // loop until error occured or reach end of stream
        {
        T value = T();
        iss >> value;
        data.push_back( value );
        }
      }
    catch ( const std::istringstream::failure& e )
      {
      if ( !iss.eof() )
        {
        throw e; // loop terminated abnomally if not because of EOF
        }
      }
    }
  else
    {
    // the number of elements to be read is provided by count or, if count is 0, data.size()
    if ( count == 0 )
      {
      count = (int)data.size();
      }
    if ( static_cast<size_t>(count) > data.size() )
      {
      data.resize( static_cast<size_t>(count) );
      }
    for ( size_t i = 0; i < static_cast<size_t>(count); i++ )
      {
      T value = T();
      iss >> value;
      data[i] = value;
      }
    }

  if ( iss.eof() )
    {
    s.clear();
    }
  else
    {
    s.erase( 0, iss.tellg() );
    }
  return s;
}

/**
 * Functions to convert a vector of type std::vector<T> to a string.
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
std::string&
StringTools::FromData( std::string& s, const std::vector<T>& data )
{
  std::ostringstream oss( std::ostringstream::out );
  oss.exceptions( oss.badbit );
  for ( size_t i = 0; i < data.size(); i++ )
    {
    oss << " " << data[i];
    }

  s.append( oss.str() );
  return s;
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
std::string&
StringTools::ToData( std::string& s, Array<T>& data, int count )
{
  std::istringstream iss( s, std::istringstream::in );
  iss.exceptions( iss.failbit | iss.badbit );

  if ( count < 0 )
    {
    // compute the number of elements to be read from the input stream
    std::vector<T> v;
    try
      {
      while ( !iss.eof() ) // loop until error occured or reach end of stream
        {
        T value = T();
        iss >> value;
        v.push_back( value );
        }
      }
    catch ( const std::istringstream::failure& e )
      {
      if ( !iss.eof() )
        {
        throw e; // loop terminated abnomally if not because of EOF
        }
      }

    data.SetSize( v.size() );
    for ( size_t i = 0; i < v.size(); i++ )
      {
      data[i] = v[i];
      }
    }
  else
    {
    // the number of elements to be read is provided by count or, if count is 0, data.size()
    if ( count == 0 )
      {
      count = (int)data.GetSize();
      }
    if ( static_cast<size_t>(count) > data.GetSize() )
      {
      data.SetSize( static_cast<size_t>(count) );
      }
    for ( size_t i = 0; i < static_cast<size_t>(count); i++ )
      {
      T value = T();
      iss >> value;
      data[i] = value;
      }
    }

  if ( iss.eof() )
    {
    s.clear();
    }
  else
    {
    s.erase( 0, iss.tellg() );
    }
  return s;
}

/**
 * Functions to convert an array of type itk::Array<T> to a string.
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
std::string&
StringTools::FromData( std::string& s, const Array<T>& data )
{
  std::ostringstream oss( std::ostringstream::out );
  oss.exceptions( oss.badbit );
  for ( size_t i = 0; i < static_cast<size_t>(data.GetSize()); i++ )
    {
    oss << " " << data[i];
    }

  s.append( oss.str() );
  return s;
}

/////////////////////////////////////////////////////////////////////////////
// helper functions for converting a string to/from a basic data type
/////////////////////////////////////////////////////////////////////////////

/**
 * Functions to convert a string to a value of basic data type.
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
std::string&
StringTools::ToData( std::string& s, T& data )
{
  std::istringstream iss( s, std::istringstream::in );
  iss.exceptions( iss.failbit | iss.badbit );
  iss >> data;

  if ( iss.eof() )
    {
    s.clear();
    }
  else
    {
    s.erase( 0, iss.tellg() );
    }
  return s;
}

/**
 * Functions to convert a value of basic data type to a string.
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
std::string&
StringTools::FromData( std::string& s, const T& data )
{
  std::ostringstream oss( std::ostringstream::out );
  oss.exceptions( oss.badbit );
  oss << data;

  s.append( oss.str() );
  return s;
}

} // namespace itk

#endif // itkStringTools_hxx
