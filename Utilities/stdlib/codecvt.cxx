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

#include <stl_config.h>
#include <stl_locale.h>
#include <stl_codecvt.h>
#include <algorithm>

__STL_BEGIN_NAMESPACE

_Locale_ctype* __acquire_ctype(const char* name);
void __release_ctype(_Locale_ctype* cat);

//----------------------------------------------------------------------
// codecvt<char, char, mbstate_t>

codecvt<char, char, mbstate_t>::codecvt(size_t refs)
  : locale::facet(refs)
{}

codecvt<char, char, mbstate_t>::~codecvt()
{}

int codecvt<char, char, mbstate_t>::do_length(state_type&,
                                              const  extern_type* from, 
                                              const  extern_type* end,
                                              size_t              max) const
{
  return min((size_t) (end - from), max);
}

//----------------------------------------------------------------------
// codecvt<wchar_t, char, mbstate_t>

codecvt<wchar_t, char, mbstate_t>::codecvt(size_t refs)
  : locale::facet(refs)
{}

codecvt<wchar_t, char, mbstate_t>::~codecvt() 
{}

codecvt<wchar_t, char, mbstate_t>::result
codecvt<wchar_t, char, mbstate_t>::do_out(state_type&         /* state */, 
                                          const intern_type*  from,
                                          const intern_type*  from_end,
                                          const intern_type*& from_next,
                                          extern_type*        to,
                                          extern_type*        to_limit,
                                          extern_type*&       to_next) const
{
  ptrdiff_t len = min(from_end - from, to_limit - to);
  copy(from, from + len, to);
  from_next = from + len;
  to_next   = to   + len;
  return ok;
}

codecvt<wchar_t, char, mbstate_t>::result
codecvt<wchar_t, char, mbstate_t>::do_in (state_type&       /* state */,
                                          const extern_type*  from,
                                          const extern_type*  from_end,
                                          const extern_type*& from_next,
                                          intern_type*        to,
                                          intern_type*        to_limit,
                                          intern_type*&       to_next) const
{
  ptrdiff_t len = min(from_end - from, to_limit - to);
  copy(from, from + len, to);
  from_next = from + len;
  to_next   = to   + len;
  return ok;
}

codecvt<wchar_t, char, mbstate_t>::result
codecvt<wchar_t, char, mbstate_t>::do_unshift(state_type&   /* state */,
                                              extern_type*  to, 
                                              extern_type*  to_limit,
                                              extern_type*& to_next) const
{
  to_next = to;
  return noconv;
}

int codecvt<wchar_t, char, mbstate_t>::do_encoding() const __STL_NOTHROW {
  return 1;
}


bool codecvt<wchar_t, char, mbstate_t>::do_always_noconv() const __STL_NOTHROW
{
  return true;
}

int codecvt<wchar_t, char, mbstate_t>::do_length(state_type&,
                                                 const  extern_type* from, 
                                                 const  extern_type* end,
                                                 size_t max) const 
{
  return min((size_t) (end - from), max);
}

int codecvt<wchar_t, char, mbstate_t>::do_max_length() const __STL_NOTHROW {
  return 1;
}

//----------------------------------------------------------------------
// codecvt_byname<char>

codecvt_byname<char, char, mbstate_t>
  ::codecvt_byname(const char* /* name */, size_t refs)
    : codecvt<char, char, mbstate_t>(refs)
{}


//----------------------------------------------------------------------
// codecvt_byname<wchar_t>

codecvt_byname<wchar_t, char, mbstate_t>
  ::codecvt_byname(const char* name, size_t refs)
    : codecvt<wchar_t, char, mbstate_t>(refs),
      _M_ctype(__acquire_ctype(name))
{
  if (!_M_ctype)
    locale::_M_throw_runtime_error();
}

codecvt_byname<wchar_t, char, mbstate_t>::~codecvt_byname()
{
  __release_ctype(_M_ctype);
}

codecvt<wchar_t, char, mbstate_t>::result
codecvt_byname<wchar_t, char, mbstate_t>
  ::do_out(state_type&     state,
           const wchar_t*  from,
           const wchar_t*  from_end,
           const wchar_t*& from_next,
           char*           to,
           char*           to_limit,
           char*&          to_next) const
{
  while (from != from_end) {
    size_t chars_stored = _Locale_wctomb(_M_ctype,
                                         to, to_limit - to, *from,
                                         &state);
    if (chars_stored == (size_t) -1) {
      from_next = from;
      to_next   = to;
      return error;
    }

    else if (chars_stored == (size_t) -2) {
      from_next = from;
      to_next   = to;
      return partial;
    }

    ++from;
    to += chars_stored;
  }

  from_next = from;
  to_next   = to;
  return ok;
}

codecvt<wchar_t, char, mbstate_t>::result
codecvt_byname<wchar_t, char, mbstate_t>
  ::do_in(state_type&         state,
          const extern_type*  from,
          const extern_type*  from_end,
          const extern_type*& from_next,
          intern_type*        to,
          intern_type*        to_limit,
          intern_type*&       to_next) const
{
  while (from != from_end) {
    size_t chars_read = _Locale_mbtowc(_M_ctype,
                                       to, from, from_end - from,
                                       &state);
    if (chars_read == (size_t) -1) {
      from_next = from;
      to_next   = to;
      return error;
    }

    if (chars_read == (size_t) -1) {
      from_next = from;
      to_next   = to;
      return partial;
    }

    from += chars_read;
    to++;
  }

  from_next = from;
  to_next   = to;
  return ok;
}

codecvt<wchar_t, char, mbstate_t>::result
codecvt_byname<wchar_t, char, mbstate_t>
  ::do_unshift(state_type&   state,
               extern_type*  to,
               extern_type*  to_limit,
               extern_type*& to_next) const
{
  to_next = to;
  size_t result = _Locale_unshift(_M_ctype, &state,
                                  to, to_limit - to, &to_next);
  if (result == (size_t) -1)
    return error;
  else if (result == (size_t) -2)
    return partial;
  else
    return to_next == to ? noconv : ok;
}

int
codecvt_byname<wchar_t, char, mbstate_t>::do_encoding() const __STL_NOTHROW
{
  if (_Locale_is_stateless(_M_ctype)) {
    int max_width = _Locale_mb_cur_max(_M_ctype);
    int min_width = _Locale_mb_cur_min(_M_ctype);
    return min_width == max_width ? min_width : 0;
  }
  else
    return -1;
}


bool codecvt_byname<wchar_t, char, mbstate_t>
  ::do_always_noconv() const __STL_NOTHROW
{
  return false;
}

int codecvt_byname<wchar_t, char, mbstate_t>
  ::do_length(state_type&,
              const  extern_type* from, const  extern_type* end,
              size_t max) const 
{
  return min((size_t) (end - from), max);
}

int
codecvt_byname<wchar_t, char, mbstate_t>::do_max_length() const __STL_NOTHROW
{
  return _Locale_mb_cur_max(_M_ctype);
}

__STL_END_NAMESPACE

// Local Variables:
// mode:C++
// End:

