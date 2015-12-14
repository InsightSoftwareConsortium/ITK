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

#ifndef itkFancyString_h
#define itkFancyString_h

#include <string>
#include <vector>
#include <map>
#include "itkArray.h"
#include "ITKIOXMLExport.h"

namespace itk
{

/**
 * \class FancyString
 * \brief A special string type that is used to aid I/O operations in DOM-based object readers/writers.
 *
 * This class is created for the purpose of overloading the ">>" and "<<" operators for the string type
 * to perform DOM-related operations. As ITK has overloaded this operator
 * for other operations, we need a new string type to use this operator in our implementation.
 * In addition, this class can be used anywhere that expects a "const char *" to avoid explicit type casting.
 *
 * This class also adds new string manipulation functions to std::string, including trims, case conversions,
 * splits, comparisons, sub-string tests, and so on.
 *
 * \ingroup ITKIOXML
 */
class ITKIOXML_EXPORT FancyString
{
public:
  FancyString();
  FancyString( const std::string& str );
  FancyString( const char* s );

  FancyString& operator=( const std::string& str );
  FancyString& operator=( const char* s );

  /** Function to cast this type to "const char *". */
  operator const char * () const;

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
  FancyString& ToData( std::vector<T>& outputData, int count = -1 );

  /**
   * Functions to convert a vector of type std::vector<T> to a string.
   * An exception will be thrown if errors were encountered during the conversion.
   */
  template < typename T >
  FancyString& FromData( const std::vector<T>& inputData );

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
  FancyString& ToData( Array<T>& outputData, int count = -1 );

  /**
   * Functions to convert an array of type itk::Array<T> to a string.
   * An exception will be thrown if errors were encountered during the conversion.
   */
  template < typename T >
  FancyString& FromData( const Array<T>& inputData );

  /////////////////////////////////////////////////////////////////////////////
  // helper functions for converting a string to/from a basic data type
  /////////////////////////////////////////////////////////////////////////////

  /**
   * Function to convert this value to a "std::string".
   */
  operator const std::string& () const;

  /**
   * Function to convert this value to a "std::string".
   */
  const std::string& ToString() const;

  /**
   * Functions to convert a string to a value of basic data type.
   * An exception will be thrown if errors were encountered during the conversion.
   */
  template < typename T >
  FancyString& ToData( T& outputData );

  /**
   * Functions to convert a value of basic data type to a string.
   * An exception will be thrown if errors were encountered during the conversion.
   */
  template < typename T >
  FancyString& FromData( const T& inputData );

  /////////////////////////////////////////////////////////////////////////////
  // helper functions for string manipulations
  /////////////////////////////////////////////////////////////////////////////

  /** Clear all characters. */
  void ClearContent();

  /** Extend the string by appending additional characters. */
  void Append(const FancyString& str);

  /** Method to trim the spaces or user-specified characters on both ends of a string. */
  FancyString& Trim( const std::string& dislike = " \t\n\r" );

  /** Method to trim the spaces or user-specified characters on left end of a string. */
  FancyString& TrimLeft( const std::string& dislike = " \t\n\r" );

  /** Method to trim the spaces or user-specified characters on right end of a string. */
  FancyString& TrimRight( const std::string& dislike = " \t\n\r" );

  /** Method to covert lower-case characters to upper cases in a string. */
  FancyString& ToUpperCase();

  /** Method to covert upper-case characters to lower cases in a string. */
  FancyString& ToLowerCase();

  /** Method to split a string into two parts with user-defined delimiters. */
  void Split( std::string& lpart, std::string& rpart, const std::string& delims = "=:" ) const;

  /** Method to split a string into a sequence of strings with user-defined delimiters. */
  void Split( std::vector<std::string>& result, const std::string& delims = ";|" ) const;

  /**
   * Method to split a string into a sequence of sub-strings with user-defined delimiters,
   * then each sub-string is further splitted into a <key,value> pair with separators "=:".
   */
  void Split( std::map<std::string,std::string>& result, const std::string& delims = ";|" );

  /** Method to test whether one string matches with another. */
  bool MatchWith( const std::string& s2, bool ignoreCase = true );

  /** Method to test whether a string starts with a user-given sub-string. */
  bool StartWith( const std::string& s2, bool ignoreCase = true );

  /** Method to test whether a string ends with a user-given sub-string. */
  bool EndWith( const std::string& s2, bool ignoreCase = true );

  /** Method to test whether a string contains a user-given sub-string. */
  bool ContainSub( const std::string& s2, bool ignoreCase = true );

private:

  std::string m_Value;

}; // class FancyString

} // namespace itk

/////////////////////////////////////////////////////////////////////////////
// helper function to compare FancyString with std::string and char*
/////////////////////////////////////////////////////////////////////////////

bool ITKIOXML_EXPORT operator!=( itk::FancyString& s, const std::string& );
bool ITKIOXML_EXPORT operator!=( itk::FancyString& s, const char* );
bool ITKIOXML_EXPORT operator!=( itk::FancyString& s, const itk::FancyString&);

bool ITKIOXML_EXPORT operator==( itk::FancyString& s, const std::string& );
bool ITKIOXML_EXPORT operator==( itk::FancyString& s, const char* );
bool ITKIOXML_EXPORT operator==( itk::FancyString& s, const itk::FancyString&);

namespace itk
{
/////////////////////////////////////////////////////////////////////////////
// manipulators for FancyString (currently only one is defined)
/////////////////////////////////////////////////////////////////////////////

/**
 * Definition of FancyString-based manipulators without parameters.
 */
FancyString& operator<<( FancyString& s, void (*mf)(FancyString&) );

/**
 * Manipulator to clear all characters in a FancyString.
 */
void ClearContent( FancyString& input );

/////////////////////////////////////////////////////////////////////////////
// helper functions for converting a string to/from a std::vector
/////////////////////////////////////////////////////////////////////////////

/**
 * Function to convert a string to a vector of type std::vector<T>.
 * Number of elements to read is given by data.size().
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
FancyString& operator>>( FancyString& s, std::vector<T>& data );

/**
 * Functions to convert a vector of type std::vector<T> to a string.
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
FancyString& operator<<( FancyString& s, const std::vector<T>& data );

/////////////////////////////////////////////////////////////////////////////
// helper functions for converting a string to/from a itk::Array
/////////////////////////////////////////////////////////////////////////////

/**
 * Function to convert a string to an array of type itk::Array<T>.
 * Number of elements to read is given by data.GetSize().
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
FancyString& operator>>( FancyString& s, Array<T>& data );

/**
 * Functions to convert an array of type itk::Array<T> to a string.
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
FancyString& operator<<( FancyString& s, const Array<T>& data );

/////////////////////////////////////////////////////////////////////////////
// helper functions for converting a string to/from a basic data type
/////////////////////////////////////////////////////////////////////////////

/**
 * Functions to convert a string to a value of basic data type.
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
FancyString& operator>>( FancyString& s, T& data );

/**
 * Functions to convert a value of basic data type to a string.
 * An exception will be thrown if errors were encountered during the conversion.
 */
template < typename T >
FancyString& operator<<( FancyString& s, const T& data );

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFancyString.hxx"
#endif

#endif // itkFancyString_h
