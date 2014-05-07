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
#include "itkInternationalizationIOHelpers.h"

#include <string.h> // for strcmp (cstring cannot be used on both Sun and VS6)

// Some utility functions for the test
#if defined(ITK_SUPPORTS_WCHAR_T_FILENAME_CSTYLEIO) \
   && ( defined(ITK_SUPPORTS_WCHAR_T_FILENAME_IOSTREAMS_CONSTRUCTORS) || defined(ITK_SUPPORTS_FDSTREAM_HPP) )
# define LOCAL_USE_WIN32_WOPEN 1
#else
# define LOCAL_USE_WIN32_WOPEN 0
#endif

// Check if alpha.txt exists using _wfopen with a wstring on MSVC and mingw
// and fopen with a UTF-8 char * otherwise
bool checkAlphaExists()
{
#if LOCAL_USE_WIN32_WOPEN
  // mingw has _wfopen
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
  if (tmp!=ITK_NULLPTR)
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
  // mingw has _wunlink
  std::wstring wstr;
  wstr.push_back((wchar_t)(0x03B1));
  wstr += L".txt";
  return (_wunlink(wstr.c_str()) != -1);
#else
  std::string utf8_str;
  utf8_str.append(1,(char)(0xCE));
  utf8_str.append(1,(char)(0xB1));
  utf8_str += ".txt";
  return (unlink(utf8_str.c_str()) != -1);
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

  // Check if we actually find it is a valid string
  if ( !itk::i18n::IsStringEncodingValid(utf8_str) )
    {
    std::cout << "Wrongly detected invalid utf8 string." << std::endl;
    ++nberror;
    }

#if LOCAL_USE_WIN32_WOPEN
  // Check that the string to wide string conversion works
  std::wstring utf16_str;
  utf16_str.push_back((wchar_t)(0x03B1));
  utf16_str += L".txt";
  const std::wstring fromutf8_utf16_str = itk::i18n::Utf8StringToWString( utf8_str );

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
  if ( itk::i18n::IsStringEncodingValid(bad_utf8_str) )
    {
    std::cout << "Did not detect invalid utf8 string using windows API." << std::endl;
    ++nberror;
    }
#endif

  // Start by removing alpha.txt if it exists
  removeAlpha();

  // Create alpha.txt using utf8fopen
  FILE * wfile = itk::i18n::I18nFopen(utf8_str, "wb");

  if (!checkAlphaExists())
    {
    std::cout << "alpha.txt does not exist after utf8fopen." << std::endl;
    ++nberror;
    }

  if (wfile!=ITK_NULLPTR)
    {
    fputs("test",wfile);
    fclose(wfile);
    }
  else
    {
    std::cout << "wfile is null." << std::endl;
    ++nberror;
    }

  FILE * rfile = itk::i18n::I18nFopen(utf8_str, "rb");

  if (rfile!=ITK_NULLPTR)
    {
    char teststring[10];
    char * retptr = fgets(teststring, 10, rfile);
    if (retptr!=ITK_NULLPTR)
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
      std::cout << "Could not read from file after I18nFopen." << std::endl;
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
    std::cout << "Could not remove alpha.txt after I18nFopen." << std::endl;
    ++nberror;
    }


  // Create alpha.txt using open and write to it using streams
  itk::i18n::I18nOfstream wstream(utf8_str.c_str(), std::ios::binary | std::ios::out );
  if (!checkAlphaExists())
    {
    std::cout << "alpha.txt does not exist after I18nOfstream creation." << std::endl;
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


  itk::i18n::I18nIfstream rstream(utf8_str.c_str(), std::ios::binary | std::ios::in );

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
    std::cout << "Could not remove alpha.txt after I18nOfstream creation." << std::endl;
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
