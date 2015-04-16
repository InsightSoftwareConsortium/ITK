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
#ifndef itkInternationalizationIOHelpers_h
#define itkInternationalizationIOHelpers_h
#include "ITKIOImageBaseExport.h"

// This header provides some helper functions to deal with unicode filenames
// It is mainly directed towards being able to use utf-8 encoded filenames
// on windows.
// This should help better dealing with internationalization (a.k.a i18n)

#include "itkMacro.h"
#include "itkIOConfigure.h"

#ifdef ITK_HAVE_UNISTD_H
#include <unistd.h> // for unlink
#else
#include <io.h>
#endif

#include <cstdio>
#include <fcntl.h>
#include <iostream>
#include <string>
#include <sys/stat.h>

// Find out how to handle unicode filenames on windows:
// * VS>=8.0 has _wopen and _wfopen and can open a (i/o)fstream using a wide
// string
// * VS7.x and MinGW have _wopen and _wfopen but cannot open a
//   (i/o)fstream using a wide string. They can however compile fdstream

#if defined( ITK_SUPPORTS_WCHAR_T_FILENAME_CSTYLEIO ) \
  && ( defined( ITK_SUPPORTS_WCHAR_T_FILENAME_IOSTREAMS_CONSTRUCTORS ) || defined( ITK_SUPPORTS_FDSTREAM_HPP ) )
#define LOCAL_USE_WIN32_WOPEN 1
#include <windows.h> // required by winnls.h
#include <winnls.h>  // for MultiByteToWideChar
#else
#define LOCAL_USE_WIN32_WOPEN 0
#endif

#if ( LOCAL_USE_WIN32_WOPEN && defined( ITK_SUPPORTS_WCHAR_T_FILENAME_IOSTREAMS_CONSTRUCTORS ) ) \
  || ( !LOCAL_USE_WIN32_WOPEN )
#define LOCAL_USE_FDSTREAM 0
#include <fstream>
#else
#define LOCAL_USE_FDSTREAM 1
#include "itkfdstream/fdstream.hpp"
#endif

namespace itk
{
namespace i18n
{
// Check if the string is correctly encoded
#if LOCAL_USE_WIN32_WOPEN
inline bool IsStringEncodingValid(const std::string & str)
{
  // Check if the string is really encoded in utf-8 using windows API
  // MultiByteToWideChar returns 0 if there was a problem during conversion
  // when given the MB_ERR_INVALID_CHARS flag
  const int utf16_size = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, str.c_str(),
                                             static_cast< int >( str.length() ), 0, 0);

  return ( utf16_size != 0 );
}

#else
inline bool IsStringEncodingValid( const std::string & itkNotUsed(str) )
{
  return true;
}

#endif

#if LOCAL_USE_WIN32_WOPEN
// Convert a utf8 encoded std::string to a utf16 encoded wstring on windows
inline std::wstring Utf8StringToWString(const std::string & str)
{
  // We do not set the MB_ERR_INVALID_CHARS to do an approximate conversion when
  // non
  // utf8 characters are found. An alternative would be to throw an exception

  // First get the size
  const int utf16_size = MultiByteToWideChar(CP_UTF8, 0, str.c_str(),
                                             static_cast< int >( str.length() ), 0, 0);

  // Now do the conversion
  std::wstring wstr;

  wstr.resize(utf16_size);
  MultiByteToWideChar(CP_UTF8, 0, str.c_str(),
                      static_cast< int >( str.length() ), &wstr[0], utf16_size);

  return wstr;
}

#endif

// Get a file descriptor from a filename (using utf8 to wstring
// on windows if requested) without specifying any specific permissions
inline int I18nOpen(const std::string & str, const int & flags)
{
#if LOCAL_USE_WIN32_WOPEN
  // mingw has _wopen
  // Convert to utf16
  const std::wstring str_utf16 = Utf8StringToWString(str);
  return _wopen(str_utf16.c_str(), flags);
#else
  return open(str.c_str(), flags);
#endif
}

// Get a file descriptor from a filename (using utf8 to wstring
// on windows if requested)
inline int I18nOpen(const std::string & str, const int & flags, const int & mode)
{
#if LOCAL_USE_WIN32_WOPEN
  // mingw has _wopen
  // Convert to utf16
  const std::wstring str_utf16 = Utf8StringToWString(str);
  return _wopen(str_utf16.c_str(), flags, mode);
#else
  return open(str.c_str(), flags, mode);
#endif
}

// Reading wrapper around I18nOpen to avoid explicitely specifying the flags
inline int I18nOpenForReading(const std::string & str)
{
#if LOCAL_USE_WIN32_WOPEN
  return I18nOpen(str, _O_RDONLY | _O_BINARY);
#else
  return I18nOpen(str, O_RDONLY);
#endif
}

// Writing wrapper around I18nOpen to avoid explicitely specifying the flags
inline int I18nOpenForWriting(const std::string & str, const bool append = false)
{
#if LOCAL_USE_WIN32_WOPEN
  if ( !append ) { return I18nOpen(str, _O_WRONLY | _O_CREAT | _O_BINARY, _S_IREAD | _S_IWRITE); }
  else { return I18nOpen(str, _O_WRONLY | _O_CREAT | _O_APPEND | _O_BINARY, _S_IREAD | _S_IWRITE); }
#elif S_IRUSR
  if ( !append ) { return I18nOpen(str, O_WRONLY | O_CREAT, S_IRUSR | S_IWUSR); }
  else { return I18nOpen(str, O_WRONLY | O_CREAT | O_APPEND, S_IRUSR | S_IWUSR); }
#else
  if ( !append ) { return I18nOpen(str, O_WRONLY | O_CREAT, S_IREAD | S_IWRITE); }
  else { return I18nOpen(str, O_WRONLY | O_CREAT | O_APPEND, S_IREAD | S_IWRITE); }
#endif
}

// Get a FILE * pointer from a filename (using utf8 to wstring
// on windows if requested)
inline FILE * I18nFopen(const std::string & str, const std::string & mode)
{
#if LOCAL_USE_WIN32_WOPEN
  // Convert to utf16
  const std::wstring str_utf16 = Utf8StringToWString(str);
  const std::wstring mode_utf16 = Utf8StringToWString(mode);
  return _wfopen( str_utf16.c_str(), mode_utf16.c_str() );
#else
  return fopen( str.c_str(), mode.c_str() );
#endif
}

#if LOCAL_USE_FDSTREAM
class I18nOfstream:public std::ostream
{
public:
  I18nOfstream(const char *str,
               std::ios_base::openmode mode = std::ios_base::out):
    std::ostream(0),
    m_fd( I18nOpenForWriting(str, ( mode & std::ios::app ) ? true:false) ),
    m_buf(m_fd)
  {
    ///\todo better handle mode flag
    this->rdbuf(&m_buf);
  }

  ~I18nOfstream() { this->close(); }

  bool is_open() { return ( m_fd != -1 ); }

  void close()
  {
    if ( m_fd != -1 ) { ::close(m_fd); }
    m_fd = -1;
  }

private:
  int           m_fd;
  itk::fdoutbuf m_buf;
};

class I18nIfstream:public std::istream
{
public:
  I18nIfstream(const char *str,
               std::ios_base::openmode mode = std::ios_base::in):
    std::istream(0),
    m_fd( I18nOpenForReading(str) ),
    m_buf(m_fd)
  {
    ///\todo better handle mode flag
    (void) mode;
    this->rdbuf(&m_buf);
  }

  ~I18nIfstream() { this->close(); }

  bool is_open() { return ( m_fd != -1 ); }

  void close()
  {
    if ( m_fd != -1 ) { ::close(m_fd); }
    m_fd = -1;
  }

private:
  int          m_fd;
  itk::fdinbuf m_buf;
};
#elif LOCAL_USE_WIN32_WOPEN
class I18nOfstream:public std::ofstream
{
public:
  I18nOfstream(const char *str, std::ios_base::openmode mode = std::ios_base::out):
    std::ofstream(Utf8StringToWString(str).c_str(), mode)
  {}
};

class I18nIfstream:public std::ifstream
{
public:
  I18nIfstream(const char *str, std::ios_base::openmode mode = std::ios_base::in):
    std::ifstream(Utf8StringToWString(str).c_str(), mode)
  {}
};
#else
typedef std::ofstream I18nOfstream;
typedef std::ifstream I18nIfstream;
#endif
} // end namespace
} // end namespace

#undef LOCAL_USE_WIN32_WOPEN
#undef LOCAL_USE_FDSTREAM

#endif  /* itkInternationalizationIOHelpers_h */
