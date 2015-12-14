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

#ifndef itkStringTools_h
#define itkStringTools_h

#include <vector>
#include <map>
#include "itkArray.h"
#include "ITKIOXMLExport.h"

namespace itk
{

/**
 * \class StringTools
 * \brief A set of tools to manipulate a string.
 *
 * This class defines a collection of methods that aid users to perform string-based
 * I/O operations, e.g., data reading/writing, string parsing, comparison, etc.
 *
 * \ingroup ITKIOXML
 */
class ITKIOXML_EXPORT StringTools
{
public:
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
  static std::string& ToData( std::string& s, std::vector<T>& data, int count = -1 );

  /**
   * Functions to convert a vector of type std::vector<T> to a string.
   * An exception will be thrown if errors were encountered during the conversion.
   */
  template < typename T >
  static std::string& FromData( std::string& s, const std::vector<T>& data );

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
  static std::string& ToData( std::string& s, Array<T>& data, int count = -1 );

  /**
   * Functions to convert an array of type itk::Array<T> to a string.
   * An exception will be thrown if errors were encountered during the conversion.
   */
  template < typename T >
  static std::string& FromData( std::string& s, const Array<T>& data );

  /////////////////////////////////////////////////////////////////////////////
  // helper functions for converting a string to/from a basic data type
  /////////////////////////////////////////////////////////////////////////////

  /**
   * Functions to convert a string to a value of basic data type.
   * An exception will be thrown if errors were encountered during the conversion.
   */
  template < typename T >
  static std::string& ToData( std::string& s, T& data );

  /**
   * Functions to convert a value of basic data type to a string.
   * An exception will be thrown if errors were encountered during the conversion.
   */
  template < typename T >
  static std::string& FromData( std::string& s, const T& data );

  /////////////////////////////////////////////////////////////////////////////
  // helper functions for string manipulations
  /////////////////////////////////////////////////////////////////////////////

  /** Method to trim the spaces or user-specified characters on both ends of a string. */
  static std::string& Trim( std::string& str, const std::string& dislike = " \t\n\r" );

  /** Method to trim the spaces or user-specified characters on left end of a string. */
  static std::string& TrimLeft( std::string& str, const std::string& dislike = " \t\n\r" );

  /** Method to trim the spaces or user-specified characters on right end of a string. */
  static std::string& TrimRight( std::string& str, const std::string& dislike = " \t\n\r" );

  /** Method to covert lower-case characters to upper cases in a string. */
  static std::string& ToUpperCase( std::string& str );

  /** Method to covert upper-case characters to lower cases in a string. */
  static std::string& ToLowerCase( std::string& str );

  /** Method to split a string into two parts with user-defined delimiters. */
  static void Split( const std::string& s, std::string& lpart, std::string& rpart, const std::string& delims = "=:" );

  /** Method to split a string into a sequence of strings with user-defined delimiters. */
  static void Split( const std::string& s, std::vector<std::string>& result, const std::string& delims = ";|" );

  /**
   * Method to split a string into a sequence of sub-strings with user-defined delimiters,
   * then each sub-string is further splitted into a <key,value> pair with separators "=:".
   */
  static void Split( const std::string& s, std::map<std::string,std::string>& result, const std::string& delims = ";|" );

  /** Method to test whether one string matches with another. */
  static bool MatchWith( const std::string& s1, const std::string& s2, bool ignoreCase = true );

  /** Method to test whether a string starts with a user-given sub-string. */
  static bool StartWith( const std::string& s1, const std::string& s2, bool ignoreCase = true );

  /** Method to test whether a string ends with a user-given sub-string. */
  static bool EndWith( const std::string& s1, const std::string& s2, bool ignoreCase = true );

  /** Method to test whether a string contains a user-given sub-string. */
  static bool ContainSub( const std::string& s1, const std::string& s2, bool ignoreCase = true );
}; // class StringTools

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStringTools.hxx"
#endif

#endif // itkStringTools_h
