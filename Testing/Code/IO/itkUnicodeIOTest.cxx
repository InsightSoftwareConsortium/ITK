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
#include "fdstream.hpp"

#include <cstdlib> // for EXIT_FAILURE and EXIT_SUCCESS

#include <stdio.h> // Borland needs this (cstdio doesn't work)
#include <fcntl.h>
#include <iostream>
#include <string>
#include <sys/stat.h>


#if (defined(WIN32) || defined(_WIN32)) && (!(defined(__CYGWIN__) || defined(__CYGWIN32__)))
// cygwin has NO _wopen an NO _wfopen
// If you really need unicode filenames on cygwin, just use cygwin >= 1.7 for now
// Alternatively, we should try and use pure win32 functions such as
// CreateFileW and convert the win32 file handle using _open_osfhandle and _fdopen
# define LOCAL_USE_WIN32_WOPEN 1
#else
# define LOCAL_USE_WIN32_WOPEN 0
#endif

#if LOCAL_USE_WIN32_WOPEN
#  include <windows.h>
#  include <winnls.h>
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
  const int utf16_size = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, str.c_str(), str.length(), 0, 0);
  return (utf16_size != 0);
}

// Convert a utf8 encoded std::string to a utf16 encoded wstring on windows
inline std::wstring Utf8StringToWString( const std::string & str )
{
  // We do not set the MB_ERR_INVALID_CHARS to do an approximate conversion when non
  // utf8 characters are found. An alternative would be to throw an exception

  // First get the size
  const int utf16_size = MultiByteToWideChar(CP_UTF8, 0, str.c_str(), str.length(), 0, 0);

  // Now do the conversion
  std::wstring wstr;
  wstr.resize(utf16_size);
  MultiByteToWideChar(CP_UTF8, 0, str.c_str(), str.length(), &wstr[0], utf16_size);
  
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
  ///\todo check if cygwin has and needs the O_BINARY flag
  return utf8open(str, _O_RDONLY | _O_BINARY );
#else
  return utf8open(str, O_RDONLY );
#endif
}

// Writting wrapper around utf8open to avoid explicitely specifying the flags
inline int utf8openforwritting( const std::string & str, const bool append = false )
{
#if LOCAL_USE_WIN32_WOPEN
  ///\todo check if cygwin has and needs the O_BINARY flag
  if (!append) return utf8open(str, _O_WRONLY | _O_CREAT | _O_BINARY, _S_IREAD | _S_IWRITE );
  else return utf8open(str, _O_WRONLY | _O_CREAT | _O_APPEND | _O_BINARY, _S_IREAD | _S_IWRITE );
#else
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
  const std::wstring wstr( L"\u03B1.txt" );
  FILE * tmp = _wfopen(wstr.c_str(), L"r");
#else
  std::string utf8_str;
  utf8_str.push_back(0xCE);
  utf8_str.push_back(0xB1);
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
  const std::wstring wstr( L"\u03B1.txt" );
  return (_wunlink(wstr.c_str())!=-1);
#else
  std::string utf8_str;
  utf8_str.push_back(0xCE);
  utf8_str.push_back(0xB1);
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
  utf8_str.push_back(0xCE);
  utf8_str.push_back(0xB1);
  utf8_str += ".txt";

#if LOCAL_USE_WIN32_WOPEN
  if ( !itk::Utf8::IsValidUtf8(utf8_str) )
    {
    std::cout << "Wrongly detected invalid utf8 string." << std::endl;
    ++nberror;
    }

  // Create a non utf8 std::string
  std::string bad_utf8_str;
  bad_utf8_str.push_back(0xC0);
  bad_utf8_str.push_back(0xC0);
  bad_utf8_str += ".txt";

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
  const int wfd = itk::Utf8::utf8openforwritting(utf8_str);
  
  if (!checkAlphaExists())
    {
    std::cout << "alpha.txt does not exist after utf8openforwritting." << std::endl;
    ++nberror;
    }

  if (wfd!=-1)
    {
    boost::fdostream wstream(wfd);
    wstream << "test2" << std::flush;
    close(wfd);
    }
  else
    {
    std::cout << "wfd is equal to -1." << std::endl;
    ++nberror;
    }

  
  const int rfd = itk::Utf8::utf8openforreading(utf8_str);

  if (rfd!=-1)
    {
    boost::fdistream rstream(rfd);
    std::string teststring2;
    std::getline(rstream, teststring2);std::cout << "teststring2=" << teststring2 <<std::endl;
    
    if ( teststring2 != std::string("test2") )
      {
      std::cout << "teststring2 is not equal to test2." << std::endl;
      ++nberror;
      }
    
    close(rfd);
    }
  else
    {
    std::cout << "rfd is equal to -1." << std::endl;
    ++nberror;
    }

  if (!removeAlpha())
    {
    std::cout << "Could not remove alpha.txt after utf8open." << std::endl;
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
