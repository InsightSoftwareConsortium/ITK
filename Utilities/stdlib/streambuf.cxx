/*
 * Copyright (c) 1998
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */ 

#include <streambuf>

// Implementation of non-inline member functions of class
// basic_streambuf<char, char_traits<char> >

__STL_BEGIN_NAMESPACE

locale basic_streambuf<char, char_traits<char> >::pubimbue(const locale& loc)
{
  this->imbue(loc);
  locale tmp = _M_locale;
  _M_locale = loc;
  return tmp;
}

// The default constructor.
basic_streambuf<char, char_traits<char> >::basic_streambuf()
  : _M_get(_M_default_get), _M_put(_M_default_put), _M_locale()
{
  _M_lock._M_initialize();

  _FILE_I_set(_M_get, 0, 0, 0);
  _FILE_O_set(_M_put, 0, 0, 0);
}

// This constructor is an extension.  It is for streambuf subclasses that
// are synchronized with C stdio files.
basic_streambuf<char, char_traits<char> >
  ::basic_streambuf(FILE* __get, FILE* __put)
    : _M_get(__get ? *__get : _M_default_get),
      _M_put(__put ? *__put : _M_default_put),
      _M_locale()
{
  _M_lock._M_initialize();

  if (&_M_get == &_M_default_get)
    _FILE_I_set(_M_get, 0, 0, 0);
  if (&_M_put == &_M_default_put)
    _FILE_O_set(_M_put, 0, 0, 0);      
}

void basic_streambuf<char, char_traits<char> >::imbue(const locale&)
{}

basic_streambuf<char, char_traits<char> >*
basic_streambuf<char, char_traits<char> >::setbuf(char*, streamsize)
{
  return this;
}

basic_streambuf<char, char_traits<char> >::pos_type
basic_streambuf<char, char_traits<char> >
  ::seekoff(off_type, ios_base::seekdir, ios_base::openmode)
{
  return pos_type(-1);
}

basic_streambuf<char, char_traits<char> >::pos_type
basic_streambuf<char, char_traits<char> >
  ::seekpos(pos_type, ios_base::openmode)
{
  return pos_type(-1);
}

int basic_streambuf<char, char_traits<char> >::sync()
{
  return 0;
}

streamsize basic_streambuf<char, char_traits<char> >::showmanyc()
{
  return 0;
}

streamsize basic_streambuf<char, char_traits<char> >
  ::xsgetn(char* s, streamsize n)
{
  streamsize result = 0;
  const int_type eof = traits_type::eof();

  while (result < n) {
    if (_FILE_I_avail(_M_get) > 0) {
      size_t chunk = min(static_cast<size_t>(_FILE_I_avail(_M_get)),
                         static_cast<size_t>(n - result));
      traits_type::copy(s, _FILE_I_next(_M_get), chunk);
      result += chunk;
      s += chunk;
      _FILE_I_bump(_M_get, chunk);
    }
    else {
      int_type c = sbumpc();
      if (c != eof) {
        s[result] = c;
        ++result;
        ++s;
      }
      else
        break; 
    }
  }
  
  return result;
}

basic_streambuf<char, char_traits<char> >::int_type
basic_streambuf<char, char_traits<char> >::underflow()
{
  return traits_type::eof();
}

basic_streambuf<char, char_traits<char> >::int_type
basic_streambuf<char, char_traits<char> >::uflow()
{
  const int_type eof = traits_type::eof();
  return this->underflow() == eof 
    ? eof
    : traits_type::to_int_type(_FILE_I_postincr(_M_get));
}

basic_streambuf<char, char_traits<char> >::int_type
basic_streambuf<char, char_traits<char> >::pbackfail(int_type __c)
{
  return traits_type::eof();
}


streamsize basic_streambuf<char, char_traits<char> >
  ::xsputn(const char* s, streamsize n)
{
  streamsize result = 0;
  const int_type eof = traits_type::eof();

  while (result < n) {
    if (_FILE_O_avail(_M_put) > 0) {
      size_t chunk = min(static_cast<size_t>(_FILE_O_avail(_M_put)),
                         static_cast<size_t>(n - result));
      traits_type::copy(_FILE_O_next(_M_put), s, chunk);
      result += chunk;
      s += chunk;
      _FILE_O_bump(_M_put, chunk);
    }

    else if (this->overflow(_Traits::to_int_type(*s)) != eof) {
      ++result;
      ++s;
    }
    else
      break;
  }
  return result;
}

streamsize basic_streambuf<char, char_traits<char> >
  ::_M_xsputnc(char c, streamsize n)
{
  streamsize result = 0;
  const int_type eof = traits_type::eof();

  while (result < n) {
    if (_FILE_O_avail(_M_put) > 0) {
      size_t chunk = min(static_cast<size_t>(_FILE_O_avail(_M_put)),
                         static_cast<size_t>(n - result));
      traits_type::assign(_FILE_O_next(_M_put), chunk, c);
      result += chunk;
      _FILE_O_bump(_M_put, chunk);
    }

    else if (this->overflow(traits_type::to_int_type(c)) != eof)
      ++result;
    else
      break;
  }
  return result;
}

basic_streambuf<char, char_traits<char> >::int_type
basic_streambuf<char, char_traits<char> >::overflow(int_type c)
{
  return traits_type::eof();
}

basic_streambuf<char, char_traits<char> >::int_type
basic_streambuf<char, char_traits<char> >::_M_snextc_aux()
{
  int_type eof = traits_type::eof();
  if (_FILE_I_avail(_M_get) == 0)
    return this->uflow() == eof ? eof : this->sgetc();
  else {
    _FILE_I_set(_M_get,
                _FILE_I_begin(_M_get),
                _FILE_I_end(_M_get),
                _FILE_I_end(_M_get));
    return this->underflow();
  }
}

//----------------------------------------------------------------------
// Force instantiation of basic_streambuf

// not basic_streambuf<char>, because it's specialized.

#ifdef INSTANTIATE_WIDE_STREAMS
template class basic_streambuf<wchar_t>;
#endif /* INSTANTIATE_WIDE_STREAMS */

__STL_END_NAMESPACE

// Local Variables:
// mode:C++
// End:
