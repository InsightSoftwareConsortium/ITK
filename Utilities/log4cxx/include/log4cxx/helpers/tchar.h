/*
 * Copyright 2003,2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
 
#ifndef _LOG4CXX_HELPERS_TCHAR_H
#define _LOG4CXX_HELPERS_TCHAR_H

#include <log4cxx/config.h>
#include <string>
#include <iostream>
#include <sstream>
#include <cwchar>
#include <algorithm> // min & max
#include <stdio.h> // sprintf
#include <streambuf> // basic_streambuf
#include <log4cxx/helpers/strictmath.h>

class Convert
{
public:
  static wchar_t * ansiToUnicode(wchar_t * dst, const char * src)
  {
    ::mbstowcs(dst, src, 512);
    return dst;
  }

  static char * unicodeToAnsi(char * dst, const wchar_t * src)
  {
    ::wcstombs(dst, src, 512);
    return dst;
  }
  
  static void int64ToString(wchar_t * dst, size_t maxlen, const int64_t& ll)
  {
#ifdef WIN32
    _snwprintf(dst, maxlen, L"%I64d", ll);
#else
    swprintf(dst, maxlen, L"%lld", ll);
#endif
  }
  
  static void int64ToString(char * dst, size_t maxlen, const int64_t& ll)
  {
#ifdef WIN32
    _snprintf(dst, maxlen, "%I64d", ll);
#else
    snprintf(dst, maxlen, "%lld", ll);
#endif
  }
};

namespace std 
{
  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& os, const int64_t& ll)
  {
    _CharT buff[21];
    Convert::int64ToString(buff, 20, ll);
    os << buff;
    return os;
  }
}

#ifdef MUST_UNDEF_T
#undef _T
#endif

#ifdef WIN32
#ifndef USES_CONVERSION
  #include <malloc.h>
  #define USES_CONVERSION void * _dst = _alloca(1024);
#endif
#else
  #include <alloca.h>
  #define USES_CONVERSION void * _dst = alloca(1024);
#endif

#ifndef W2A
#define W2A(src) Convert::unicodeToAnsi((char *)_dst, src)
#endif

#ifndef A2W
#define A2W(src) Convert::ansiToUnicode((wchar_t *)_dst, src)
#endif

#ifdef UNICODE
  #include <wctype.h>

#ifndef _T
  #define _T(x) L ## x
#endif

#ifndef TCHAR
  typedef wchar_t TCHAR;
#endif
  #define totupper towupper
  #define totlower towlower
  #define tcout std::wcout
  #define tcerr std::wcerr
#ifdef WIN32
  #define tstrncasecmp _wcsnicmp
#else
  #define tstrncasecmp wcsncasecmp
#endif // WIN32

#ifndef T2A
  #define T2A(src) W2A(src)
#endif

#ifndef T2W
  #define T2W(src) src
#endif

#ifndef A2T
  #define A2T(src) A2W(src)
#endif

#ifndef W2T
  #define W2T(src) src
#endif

  #define ttol(s) wcstol(s, 0, 10)
  #define itot _itow
  #define tcscmp wcscmp
#else // Not UNICODE
  #include <ctype.h>

#ifndef _T
  #define _T(x) x
#endif

  typedef char TCHAR;
  #define totupper toupper
  #define totlower tolower
  #define tcout std::cout
  #define tcerr std::cerr
#ifdef WIN32
  #define tstrncasecmp _strnicmp
#else
  #define tstrncasecmp strncasecmp
#endif // WIN32

#ifndef T2A
  #define T2A(src) src
#endif

#ifndef T2W
  #define T2W(src) A2W(src)
#endif

#ifndef A2T
  #define A2T(src) src
#endif

#ifndef W2T
  #define W2T(src) W2A(src)
#endif

  #define ttol atol
  #define itot itoa
  #define tcscmp strcmp
#endif // UNICODE

#define _MinInc  size_t(512)
#define _MaxInc size_t(100 * 1024)

namespace log4cxx
{
  class stringbuf : public std::basic_streambuf<TCHAR>
  {
  public:
    typedef std::basic_streambuf<TCHAR> base;
    typedef base::char_type char_type;
    typedef base::traits_type traits_type;
    typedef std::allocator<char_type> allocator_type;
    typedef traits_type::int_type int_type;

    ~stringbuf()
    {
      char_type * b = pbase();
      if (b)
      {
        al.deallocate(b, epptr() - b);
      }
    }

    virtual int_type overflow(
      int_type c = stringbuf::traits_type::eof())
    {
      using namespace std;

      if (traits_type::eq_int_type(traits_type::eof(), c))
      {
        return traits_type::not_eof(c);
      }

      char_type *b = pbase();
      if (b == 0)
      {
        char_type * p = (char_type *)al.allocate(_MinInc, 0);
        setp(p, p + _MinInc);
      }
      else
      {
        size_t os = epptr() - b; // taille allouée
        size_t is =
          helpers::StrictMath::maximum(
          helpers::StrictMath::minimum(
          (os * 2), _MaxInc),_MinInc)
          + 1; // incrément d'allocation
        char_type *p = (char_type *)al.allocate(os + is, 0);
        traits_type::copy(p, b, os);
        al.deallocate(b, epptr() - b);
        setp(p, p + os + is);
        pbump((int)os);

      }

      *pptr() = c;
      pbump(1);

      return traits_type::not_eof(c);
    }

    std::basic_string<char_type> str() const
    {
      return std::basic_string<char_type>(pbase(), pptr() - pbase());
    }

    void str(const std::basic_string<char_type>& s)
    {
      setp(pbase(), epptr());
    }

    virtual pos_type seekoff(off_type off, std::ios_base::seekdir way,
      std::ios_base::openmode mode = std::ios_base::in | std::ios_base::out)
    {
      pos_type ret = pos_type(off_type(-1));
      if (mode == std::ios_base::out)
      {
        if (way == std::ios_base::beg)
        {
          setp(pbase(), epptr());
          ret = pos_type(off);
          pbump(off);
        }
        else if (way == std::ios_base::end)
        {
          setp(pbase(), epptr());
          ret = pos_type(off + epptr() - pbase());
          pbump(off + epptr() - pbase());
        }
        else if (way == std::ios_base::cur)
        {
          pbump(off);
          ret = pos_type(pptr() - pbase());
        }
      }

      return ret;
    }

    virtual pos_type seekpos(pos_type pos,
      std::ios_base::openmode mode = std::ios_base::in | std::ios_base::out)
    {
      std::streamoff off = (std::streamoff)pos;
      if (pbase() + off > epptr())
      {
        return pos_type(-1);
      }
      else
      {
        setp(pbase(), epptr());
        pbump(off);
        return pos;
      }
    }

  protected:
    allocator_type al;
  };

  class StringBuffer : public std::basic_ostream<TCHAR>
  {
  public:
    StringBuffer() : std::basic_ostream<TCHAR>(0)
      { this->init(&buffer); }
    inline std::basic_string<TCHAR> str() const
      { return buffer.str(); }
    inline void str(const std::basic_string<TCHAR>& s)
      { buffer.str(s); }

  protected:
    stringbuf buffer;
  };

  typedef std::basic_string<TCHAR> String;
  typedef std::basic_ostream<TCHAR> ostream;
  typedef std::basic_istream<TCHAR> istream;
} 

#endif //_LOG4CXX_HELPERS_TCHAR_H
