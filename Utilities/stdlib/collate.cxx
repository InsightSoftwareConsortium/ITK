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
#include <stl_collate.h>
#include <c_locale.h>
#include <algorithm>
#include <vector>
#include <string>

__STL_BEGIN_NAMESPACE

_Locale_collate* __acquire_collate(const char* name);
void __release_collate(_Locale_collate* cat);

// collate<char>

collate<char>::collate(size_t refs)
  : locale::facet(refs)
{}

collate<char>::~collate()
{}

int collate<char>::do_compare(const char* low1, const char* high1,
                              const char* low2, const char* high2) const
{
  return __lexicographical_compare_3way(low1, high1, low2, high2);
}

string collate<char>::do_transform(const char* low, const char* high) const
{
  return string(low, high);
}

long collate<char>::do_hash(const char* low, const char* high) const {
  unsigned long result = 0;
  for ( ; low < high; ++low)
    result = 5 * result + *low;
  return result;
}

// collate<wchar_t>

collate<wchar_t>::collate(size_t refs)
  : locale::facet(refs)
{}

collate<wchar_t>::~collate()
{}

int
collate<wchar_t>::do_compare(const wchar_t* low1, const wchar_t* high1,
                             const wchar_t* low2, const wchar_t* high2) const
{
  return __lexicographical_compare_3way(low1, high1, low2, high2);
}

basic_string<wchar_t>
collate<wchar_t>::do_transform(const wchar_t* low, const wchar_t* high) const
{
  return basic_string<wchar_t>(low, high);
}


long collate<wchar_t>::do_hash(const wchar_t* low, const wchar_t* high) const
{
  unsigned long result = 0;
  for ( ; low < high; ++low)
    result = 5 * result + *low;
  return result;
}

// collate_byname<char>

collate_byname<char>::collate_byname(const char* name, size_t refs)
  : collate<char>(refs),
    _M_collate(__acquire_collate(name))
{
  if (!_M_collate)
    locale::_M_throw_runtime_error();
}

collate_byname<char>::~collate_byname()
{
  __release_collate(_M_collate);
}

int collate_byname<char>::do_compare(const char* __low1,
                                     const char* __high1,
                                     const char* __low2,
                                     const char* __high2) const {
  return _Locale_strcmp(_M_collate,
                        __low1, __high1 - __low1, 
                        __low2, __high2 - __low2);
}

collate_byname<char>::string_type
collate_byname<char>::do_transform(const char* low, const char* high) const {
  vector<char> buf(high - low);
  size_t n = _Locale_strxfrm(_M_collate,
                             &buf.front(), high - low,
                             low, high - low);
  string_type result(buf.begin(), buf.begin() + n);
  return result;
}

// collate_byname<wchar_t>

collate_byname<wchar_t>::collate_byname(const char* name, size_t refs)
  : collate<wchar_t>(refs),
    _M_collate(__acquire_collate(name))
{
  if (!_M_collate)
    locale::_M_throw_runtime_error();
}

collate_byname<wchar_t>::~collate_byname() 
{
  __release_collate(_M_collate);
}

int collate_byname<wchar_t>::do_compare(const wchar_t* low1,
                                        const wchar_t* high1,
                                        const wchar_t* low2,
                                        const wchar_t* high2) const
{
  return _Locale_strwcmp(_M_collate,
                         low1, high1 - low1, 
                         low2, high2 - low2);
}

collate_byname<wchar_t>::string_type
collate_byname<wchar_t>
  ::do_transform(const wchar_t* low, const wchar_t* high) const
{
  vector<wchar_t> buf(high - low);
  size_t n = _Locale_strwxfrm(_M_collate,
                              &buf.front(), high - low,
                              low, high - low);
  string_type result(buf.begin(), buf.begin() + n);
  return result;
}

__STL_END_NAMESPACE


// Local Variables:
// mode:C++
// End:

