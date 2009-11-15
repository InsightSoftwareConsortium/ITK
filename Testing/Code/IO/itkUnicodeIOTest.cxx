/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkUnicodeIOTest.cxx
  Language:  C++
  Date:      $Date$xgoto-l

  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <cstdlib> // for EXIT_FAILURE and EXIT_SUCCESS
#include <cstring> // for strcmp

#if !(defined(WIN32) || defined(_WIN32))
# include <unistd.h> // for unlink
#endif

#include <stdio.h> // Borland needs this (cstdio does not work easy)
#include <fcntl.h>
#include <iostream>
#include <string>
#include <string.h> // needed for Sun CC ??
#include <sys/stat.h>

// Find out how to handle unicode filenames:
// * VS>=8.0 has _wopen and _wfopen and can open a (i/o)fstream using a wide string
// * cygwin has NO _wopen an NO _wfopen. If you really need unicode
//   filenames on cygwin, just use cygwin >= 1.7 for now, it works with utf8
//   natively. Alternatively, we could try and use pure win32 functions such as
//   CreateFileW and convert the win32 file handle using _open_osfhandle and _fdopen
// * VS6.0 has _wopen and _wfopen but cannot open a (i/o)fstream using a wide string
//   nor can it compile fdstream => disable unicode filename support
// * Borland c++, VS7.x and MinGW have _wopen and _wfopen but cannot open a
//   (i/o)fstream using a wide string. They can however compile fdstream

#if (defined(WIN32) || defined(_WIN32)) \
  && (!(defined(__CYGWIN__) || defined(__CYGWIN32__))) \
    && (!(defined(_MSC_VER) && (_MSC_VER <= 1200)))
# define LOCAL_USE_WIN32_WOPEN 1
# include <windows.h>
# include <winnls.h>
#else
# define LOCAL_USE_WIN32_WOPEN 0
# if (defined(_MSC_VER) && (_MSC_VER <= 1200))
#  include <io.h>
# endif
#endif

#if (defined(_MSC_VER) && ((_MSC_VER <= 1200)||(_MSC_VER >= 1400))) \
  || (!LOCAL_USE_WIN32_WOPEN)
# define LOCAL_USE_FDSTREAM 0
# include <fstream>
#else
# define LOCAL_USE_FDSTREAM 1
# include "fdstream.hpp"
#endif


namespace itk
{
namespace Utf8
{

#if LOCAL_USE_WIN32_WOPEN
 
// Check if the string is really encoded in utf-8 using windows API
inline bool IsValidUtf8(const std::string & str)
{
  // MultiByteToWideChar returns 0 if there was a problem during conversion
  // when given the MB_ERR_INVALID_CHARS flag
  const int utf16_size = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, str.c_str(),
                                             static_cast<int>(str.length()), 0, 0);
  return (utf16_size != 0);
}

// Convert a utf8 encoded std::string to a utf16 encoded wstring on windows
inline std::wstring Utf8StringToWString( const std::string & str )
{
  // We do not set the MB_ERR_INVALID_CHARS to do an approximate conversion when non
  // utf8 characters are found. An alternative would be to throw an exception

  // First get the size
  const int utf16_size = MultiByteToWideChar(CP_UTF8, 0, str.c_str(),
                                             static_cast<int>(str.length()), 0, 0);

  // Now do the conversion
  std::wstring wstr;
  wstr.resize(utf16_size);
  MultiByteToWideChar(CP_UTF8, 0, str.c_str(),
                      static_cast<int>(str.length()), &wstr[0], utf16_size);
  
  return wstr;
}

#endif

// Get a file descriptor from a utf8 encoded filename
// without specifying any specific permissions
inline int utf8open( const std::string & str, const int & flags )
{
#if LOCAL_USE_WIN32_WOPEN
  // cygwin has NO _wopen but mingw has
  // If you really need unicode filenames on cygwin, just use cygwin >= 1.7
  // Convert to utf16
  const std::wstring str_utf16 = Utf8StringToWString( str );
  return _wopen(str_utf16.c_str(), flags);
#else
  return open(str.c_str(), flags);
#endif
}

// Get a file descriptor from a utf8 encoded filename
inline int utf8open( const std::string & str, const int & flags, const int & mode )
{
#if LOCAL_USE_WIN32_WOPEN
  // cygwin has NO _wopen but mingw has
  // If you really need unicode filenames on cygwin, just use cygwin >= 1.7
  // Convert to utf16
  const std::wstring str_utf16 = Utf8StringToWString( str );
  return _wopen(str_utf16.c_str(), flags, mode);
#else
  return open(str.c_str(), flags, mode);
#endif
}

// Reading wrapper around utf8open to avoid explicitely specifying the flags
inline int utf8openforreading( const std::string & str )
{
#if LOCAL_USE_WIN32_WOPEN
  return utf8open(str, _O_RDONLY | _O_BINARY );
#else
  ///\todo check if cygwin has and needs the O_BINARY flag
  return utf8open(str, O_RDONLY );
#endif
}

// Writting wrapper around utf8open to avoid explicitely specifying the flags
inline int utf8openforwritting( const std::string & str, const bool append = false )
{
#if LOCAL_USE_WIN32_WOPEN
  if (!append) return utf8open(str, _O_WRONLY | _O_CREAT | _O_BINARY, _S_IREAD | _S_IWRITE );
  else return utf8open(str, _O_WRONLY | _O_CREAT | _O_APPEND | _O_BINARY, _S_IREAD | _S_IWRITE );
#else
  ///\todo check if cygwin has and needs the O_BINARY flag
  if (!append) return utf8open(str, O_WRONLY | O_CREAT, S_IREAD | S_IWRITE );
  else return utf8open(str, O_WRONLY | O_CREAT | O_APPEND, S_IREAD | S_IWRITE );
#endif
}

// Get a FILE * pointer from a utf8 encoded filename
inline FILE * utf8fopen( const std::string & str, const std::string & mode )
{
#if LOCAL_USE_WIN32_WOPEN
  // cygwin has NO _wfopen but mingw has
  // If you really need unicode filenames on cygwin, just use cygwin >= 1.7
  // Convert to utf16
  const std::wstring str_utf16 = Utf8StringToWString( str );
  const std::wstring mode_utf16 = Utf8StringToWString( mode );
  return _wfopen(str_utf16.c_str(), mode_utf16.c_str());
#else
  return fopen(str.c_str(), mode.c_str());
#endif
}

#if LOCAL_USE_FDSTREAM
class utf8ofstream : public std::ostream 
{
public:
  utf8ofstream( const char * str, 
    std::ios_base::openmode mode = std::ios_base::out )
    : std::ostream(0)
    , m_fd( utf8openforwritting( str, (mode & std::ios::app)?true:false ) )
    , m_buf( m_fd )
    {
    ///\todo better handle mode flag
    this->rdbuf(&m_buf);
    }

  ~utf8ofstream() { this->close(); }

  bool is_open() { return (m_fd!=-1); }

  void close()
  {
    if ( m_fd!=-1 ) ::close( m_fd );
    m_fd = -1;
  }

private:
  int m_fd;
  boost::fdoutbuf m_buf;
};

class utf8ifstream : public std::istream 
{
public:
  utf8ifstream( const char * str, 
    std::ios_base::openmode mode = std::ios_base::in )
    : std::istream(0)
    , m_fd( utf8openforreading( str ) )
    , m_buf( m_fd )
    {
    ///\todo better handle mode flag
    this->rdbuf(&m_buf);
    }

  ~utf8ifstream() { this->close(); }

  bool is_open() { return (m_fd!=-1); }

  void close()
  {
    if ( m_fd!=-1 ) ::close( m_fd );
    m_fd = -1;
  }

private:
  int m_fd;
  boost::fdinbuf m_buf;
};
#elif LOCAL_USE_WIN32_WOPEN
class utf8ofstream : public std::ofstream
{
public:
  utf8ofstream( const char * str, std::ios_base::openmode mode = std::ios_base::out )
    : std::ofstream( Utf8StringToWString(str).c_str(), mode )
  {
  }
};

class utf8ifstream : public std::ifstream
{
public:
  utf8ifstream( const char * str, std::ios_base::openmode mode = std::ios_base::in )
    : std::ifstream( Utf8StringToWString(str).c_str(), mode )
  {
  }
};
#else
typedef std::ofstream utf8ofstream;
typedef std::ifstream utf8ifstream;
#endif

} // end namespace
} // end namespace



// Some utility functions for the test

// Check if alpha.txt exists using _wfopen with a wstring on MSVC and mingw
// and fopen with a UTF-8 char * otherwise
bool checkAlphaExists()
{
#if LOCAL_USE_WIN32_WOPEN
  // cygwin has NO _wfopen but mingw has
  // If you really need unicode filenames on cygwin, just use cygwin >= 1.7
  // Borland does not understand L"\u03B1.txt"
  std::wstring wstr;
  wstr.push_back((wchar_t)(0x03B1));
  wstr += L".txt";
  FILE * tmp = _wfopen(wstr.c_str(), L"r");
#else
  std::string utf8_str;
  utf8_str.append(1, (char)(0xCE));
  utf8_str.append(1, (char)(0xB1));
  utf8_str += ".txt";
  FILE * tmp = fopen(utf8_str.c_str(), "r");
#endif
  if (tmp!=0)
    {
    // closing is required at least for mingw to be able to delete the file afterwards
    fclose(tmp);
    return true;
    }
  return false;
}

// Try to delete alpha.txt using wunlink with a wstring on MSVC
// and unlink with UTF-8 char * otherwise
bool removeAlpha()
{
#if LOCAL_USE_WIN32_WOPEN
  // cygwin has NO _wunlink but mingw has
  // If you really need unicode filenames on cygwin, just use cygwin >= 1.7
  // Borland does not understand L"\u03B1.txt"
  std::wstring wstr;
  wstr.push_back((wchar_t)(0x03B1));
  wstr += L".txt";
  return (_wunlink(wstr.c_str())!=-1);
#else
  std::string utf8_str;
  utf8_str.append(1,(char)(0xCE));
  utf8_str.append(1,(char)(0xB1));
  utf8_str += ".txt";
  return (unlink(utf8_str.c_str())!=-1);
#endif
}




// Temporarily use its own main to start by sorting out compilation problems
// without too much annoyance to the rest of the world
// main will then become
// int itkUnicodeIOTest( int , char * [] )
int main( int , char * [] )
{
  std::cout << "Starting unicode IO test." << std::endl;
  
  int nberror = 0;
   
  // Put alpha.txt encoded in utf8 within a std::string
  std::string utf8_str;
  utf8_str.append(1, (char)(0xCE));
  utf8_str.append(1, (char)(0xB1));
  utf8_str += ".txt";

#if LOCAL_USE_WIN32_WOPEN
  // Check if we actually find it is a valid utf-8 string
  if ( !itk::Utf8::IsValidUtf8(utf8_str) )
    {
    std::cout << "Wrongly detected invalid utf8 string." << std::endl;
    ++nberror;
    }

  // Check that the conversion worked
  // Borland does not understand L"\u03B1.txt"
  std::wstring utf16_str;
  utf16_str.push_back((wchar_t)(0x03B1));
  utf16_str += L".txt";
  const std::wstring fromutf8_utf16_str = itk::Utf8::Utf8StringToWString( utf8_str );

  if ( fromutf8_utf16_str != utf16_str )
    {
    std::cout << "Utf-8 to utf-16 conversion failed" << std::endl;
    ++nberror;
    }

  // Create a non utf8 std::string
  std::string bad_utf8_str;
  bad_utf8_str.push_back((char)(0xC0));
  bad_utf8_str.push_back((char)(0xC0));
  bad_utf8_str += ".txt";

  // Check if we actually find it is a non-valid utf-8 string
  if ( itk::Utf8::IsValidUtf8(bad_utf8_str) )
    {
    std::cout << "Did not detect invalid utf8 string using windows API." << std::endl;
    ++nberror;
    }
#endif



  // Start by removing alpha.txt if it exists
  removeAlpha();

  
  // Create alpha.txt using utf8fopen
  FILE * wfile = itk::Utf8::utf8fopen(utf8_str, "wb");
  
  if (!checkAlphaExists())
    {
    std::cout << "alpha.txt does not exist after utf8fopen." << std::endl;
    ++nberror;
    }

  if (wfile!=NULL)
    {
    fputs("test",wfile);
    fclose(wfile);
    }
  else
    {
    std::cout << "wfile is null." << std::endl;
    ++nberror;
    }

  FILE * rfile = itk::Utf8::utf8fopen(utf8_str, "rb");

  if (rfile!=NULL)
    {
    char teststring[10];
    char * retptr = fgets(teststring, 10, rfile);
    if (retptr!=NULL)
      {
      std::cout << "teststring=" << teststring <<std::endl;

      if ( strcmp( teststring, "test" ) != 0 )
        {
        std::cout << "teststring is not equal to test." << std::endl;
        ++nberror;
        }
      }
    else
      {
      std::cout << "Could not read from file after utf8fopen." << std::endl;
      ++nberror;
      }
    fclose(rfile);
    }
  else
    {
    std::cout << "rfile is null." << std::endl;
    ++nberror;
    }

  if (!removeAlpha())
    {
    std::cout << "Could not remove alpha.txt after utf8fopen." << std::endl;
    ++nberror;
    }


  // Create alpha.txt using open and write to it using streams
  itk::Utf8::utf8ofstream wstream(utf8_str.c_str(), std::ios::binary | std::ios::out );
  if (!checkAlphaExists())
    {
    std::cout << "alpha.txt does not exist after utf8ofstream creation." << std::endl;
    ++nberror;
    }

  if (wstream.is_open())
    {
    wstream << "teststream" << std::flush;
    }
  else
    {
    std::cout << "wstream is not open." << std::endl;
    ++nberror;
    }
  wstream.close();
  

  itk::Utf8::utf8ifstream rstream(utf8_str.c_str(), std::ios::binary | std::ios::in );

  if (rstream.is_open())
    {
    std::string teststring;
    std::getline(rstream, teststring);
    std::cout << "teststring=" << teststring <<std::endl;
    
    if ( teststring != std::string("teststream") )
      {
      std::cout << "teststring is not equal to teststream." << std::endl;
      ++nberror;
      }

    rstream.close();
    }
  else
    {
    std::cout << "rstream is not open." << std::endl;
    ++nberror;
    }


  if (!removeAlpha())
    {
    std::cout << "Could not remove alpha.txt after utf8ofstreamcreation." << std::endl;
    ++nberror;
    }


  // Check number of errors
  if ( nberror > 0 ) 
    {  
    std::cout << "Test failed with " << nberror << " errors." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
