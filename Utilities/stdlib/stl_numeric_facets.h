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

// WARNING: This is an internal header file, included by other C++
// standard library headers.  You should not attempt to use this header
// file directly.


#ifndef __SGI_STL_INTERNAL_NUMERIC_FACETS_H
#define __SGI_STL_INTERNAL_NUMERIC_FACETS_H

__STL_BEGIN_NAMESPACE

#include <limits>

//----------------------------------------------------------------------
// numpunct facets

template <class _Ch> class numpunct;
template <class _Ch> class numpunct_byname;

template <>
class numpunct<char> : public locale::facet
{
  friend class _Locale_impl;

public:
  typedef char               char_type;
  typedef basic_string<char> string_type;

  explicit numpunct(size_t __refs = 0);

  char_type decimal_point() const { return do_decimal_point(); }
  char_type thousands_sep() const { return do_thousands_sep(); }
  string grouping() const { return do_grouping(); }
  string_type truename() const { return do_truename(); }
  string_type falsename() const { return do_falsename(); }

  static locale::id id;

protected:
  ~numpunct();

  string_type _M_truename;
  string_type _M_falsename;

  virtual char_type do_decimal_point() const { return '.'; }
  virtual char_type do_thousands_sep() const { return ','; }
  virtual string do_grouping() const { return string(); }
  virtual string_type do_truename() const { return _M_truename; }
  virtual string_type do_falsename()  const { return _M_falsename; }
};

template <>
class numpunct<wchar_t> : public locale::facet
{
  friend class _Locale_impl;
public:
  typedef wchar_t               char_type;
  typedef basic_string<wchar_t> string_type;

  explicit numpunct(size_t __refs = 0);

  char_type decimal_point() const { return do_decimal_point(); }
  char_type thousands_sep() const { return do_thousands_sep(); }
  string grouping() const { return do_grouping(); }
  string_type truename() const { return do_truename(); }
  string_type falsename() const { return do_falsename(); }

  static locale::id id;

protected:
  string_type _M_truename;
  string_type _M_falsename;

  ~numpunct();

  virtual char_type do_decimal_point() const;
  virtual char_type do_thousands_sep() const;
  virtual string do_grouping() const;
  virtual string_type do_truename() const;
  virtual string_type do_falsename()  const;
};

template <>
class numpunct_byname<char> : public numpunct<char> {
public:
  typedef char                char_type;
  typedef basic_string<char> string_type;

  explicit numpunct_byname(const char* __name, size_t __refs = 0);

protected:

  ~numpunct_byname();

  virtual char_type   do_decimal_point() const;
  virtual char_type   do_thousands_sep() const;
  virtual string_type do_grouping()      const;

private:
  _Locale_numeric* _M_numeric;
};

template <>
class numpunct_byname<wchar_t>: public numpunct<wchar_t> {
public:
  typedef wchar_t               char_type;
  typedef basic_string<wchar_t> string_type;

  explicit numpunct_byname(const char* __name, size_t __refs = 0);

protected:

  ~numpunct_byname();

  virtual char_type   do_decimal_point() const;
  virtual char_type   do_thousands_sep() const;
  virtual string do_grouping() const;

private:
  _Locale_numeric* _M_numeric;
};


//----------------------------------------------------------------------
// num_get facets

template <class _CharT, class _InputIter = istreambuf_iterator<_CharT> >  
class num_get: public locale::facet
{
  friend class _Locale_impl;
public:
  typedef _CharT     char_type;
  typedef _InputIter iter_type;

  explicit num_get(size_t __refs = 0) : locale::facet(__refs) {}
    

  iter_type get(iter_type __in, iter_type __end, ios_base& __str,
                ios_base::iostate& __err, bool& __val) const {
    return do_get(__in, __end, __str, __err, __val);
  }

  iter_type get(iter_type __in, iter_type __end, ios_base& __str,
                ios_base::iostate& __err, short& __val) const {
    return do_get(__in, __end, __str, __err, __val);
  }

  iter_type get(iter_type __in, iter_type __end, ios_base& __str,
                ios_base::iostate& __err, int& __val) const {
    return do_get(__in, __end, __str, __err, __val);
  }

  iter_type get(iter_type __in, iter_type __end, ios_base& __str,
                ios_base::iostate& __err, long& __val) const {
    return do_get(__in, __end, __str, __err, __val);
  }

  iter_type get(iter_type __in, iter_type __end, ios_base& __str,
                ios_base::iostate& __err, unsigned short& __val) const {
    return do_get(__in, __end, __str, __err, __val);
  }

  iter_type get(iter_type __in, iter_type __end, ios_base& __str,
                ios_base::iostate& __err, unsigned int& __val) const {
    return do_get(__in, __end, __str, __err, __val);
  }

  iter_type get(iter_type __in, iter_type __end, ios_base& __str,
                ios_base::iostate& __err, unsigned long& __val) const {
    return do_get(__in, __end, __str, __err, __val);
  }

#ifdef __STL_LONG_LONG

  iter_type get(iter_type __in, iter_type __end, ios_base& __str,
                ios_base::iostate& __err, long long& __val) const {
    return do_get(__in, __end, __str, __err, __val);
  }

  iter_type get(iter_type __in, iter_type __end, ios_base& __str,
                ios_base::iostate& __err, unsigned long long& __val) const {
    return do_get(__in, __end, __str, __err, __val);
  }

#endif /* _STL_LONG_LONG */

  iter_type get(iter_type __in, iter_type __end, ios_base& __str,
                 ios_base::iostate& __err, float& __val) const {
    return do_get(__in, __end, __str, __err, __val);
  }

  iter_type get(iter_type __in, iter_type __end, ios_base& __str,
                ios_base::iostate& __err, double& __val) const {
    return do_get(__in, __end, __str, __err, __val);
  }

  iter_type get(iter_type __in, iter_type __end, ios_base& __str,
                ios_base::iostate& __err, long double& __val) const {
    return do_get(__in, __end, __str, __err, __val);
  }

  iter_type get(iter_type __in, iter_type __end, ios_base& __str,
                ios_base::iostate& __err, void*& __val) const {
    return do_get(__in, __end, __str, __err, __val);
  }

  static locale::id id;

protected:
  ~num_get();

  typedef basic_string<_CharT> string_type; 
  typedef ctype<_CharT>        _Ctype;
  typedef numpunct<_CharT>     _Numpunct;

  virtual iter_type do_get(iter_type __in, iter_type __end,
                           ios_base& __str, ios_base::iostate& __err,
                           bool& __val) const;

  virtual iter_type do_get(iter_type __in, iter_type __end, ios_base& __str,
                           ios_base::iostate& __err, short& __val) const {
    return _M_do_get_integer(__in, __end, __str, __err, __val, _CharT());
  }

  virtual iter_type do_get(iter_type __in, iter_type __end, ios_base& __str,
                           ios_base::iostate& __err, int& __val) const {
    return _M_do_get_integer(__in, __end, __str, __err, __val, _CharT());
  }

  virtual iter_type do_get(iter_type __in, iter_type __end, ios_base& __str,
                           ios_base::iostate& __err, long& __val) const {
    return _M_do_get_integer(__in, __end, __str, __err, __val, _CharT());
  }

  virtual iter_type do_get(iter_type __in, iter_type __end, ios_base& __str,
                           ios_base::iostate& __err,
                           unsigned short& __val) const {
    return _M_do_get_integer(__in, __end, __str, __err, __val, _CharT());
  }

  virtual iter_type do_get(iter_type __in, iter_type __end, ios_base& __str,
                           ios_base::iostate& __err, 
                           unsigned int& __val) const {
    return _M_do_get_integer(__in, __end, __str, __err, __val, _CharT());
  }

  virtual iter_type do_get(iter_type __in, iter_type __end, ios_base& __str,
                           ios_base::iostate& __err,
                           unsigned long& __val) const {
    return _M_do_get_integer(__in, __end, __str, __err, __val, _CharT());
  }

#ifdef __STL_LONG_LONG

  virtual iter_type do_get(iter_type __in, iter_type __end, ios_base& __str,
                           ios_base::iostate& __err,
                           long long& __val) const {
    return _M_do_get_integer(__in, __end, __str, __err, __val, _CharT());
  }

  virtual iter_type do_get(iter_type __in, iter_type __end, ios_base& __str,
                           ios_base::iostate& __err,
                           unsigned long long& __val) const {
    return _M_do_get_integer(__in, __end, __str, __err, __val, _CharT());
  }
    
#endif /* __STL_LONG_LONG */

  virtual iter_type do_get(iter_type __in, iter_type __end, ios_base& __str,
                          ios_base::iostate& __err,
                          float& __val) const {
    return _M_do_get_float(__in, __end, __str, __err, __val, _CharT());
  }

  virtual iter_type do_get(iter_type __in, iter_type __end, ios_base& __str,
                           ios_base::iostate& __err, 
                           double& __val) const {
    return _M_do_get_float(__in, __end, __str, __err, __val, _CharT());
  }


  virtual iter_type do_get(iter_type __in, iter_type __end, ios_base& __str,
                           ios_base::iostate& __err,
                           long double& __val) const {
    return _M_do_get_float(__in, __end, __str, __err, __val, _CharT());
  }

  virtual iter_type do_get(iter_type __in, iter_type __end, ios_base& __str,
                           ios_base::iostate& __err,
                           void*& __p) const {
    size_t __val;
    iter_type __tmp
      = _M_do_get_integer(__in, __end, __str, __err, __val, _CharT());
    if (!(__err & ios_base::failbit))
      __p = reinterpret_cast<void*>(__val);
    return __tmp;
  }
};

template <class _CharT, class _InputIter>
num_get<_CharT, _InputIter>::~num_get()
{}

// _M_do_get_integer and its helper functions.  

// Helper functions for _M_do_get_integer
void __initialize_get_digit(wchar_t*, wchar_t*, const ctype<wchar_t>&);
int  __get_digit(wchar_t, const wchar_t*, const wchar_t*, wchar_t);

// __get_integer for wchar_t.
template <class _InputIter, class _Integer>
pair<_InputIter, bool>
__get_integer(_InputIter __first, _InputIter __last,
              int __base, wchar_t __separator, bool __do_sep,
              const locale& __loc,
              _Integer& __val) 
{
  int __got = 0;
  bool __overflow = false;
  bool __negative = false;
  _Integer __result = 0;

  const ctype<wchar_t>& __ctype = use_facet<ctype<wchar_t> >(__loc);

  wchar_t __digits[10];
  wchar_t __xdigits[12];
  __initialize_get_digit(__digits, __xdigits, __ctype);

  if (__first == __last)
    return make_pair(__first, false);
  else {
    wchar_t __c = *__first;
    if (__c == __ctype.widen('-')) {
      __negative = true;
      ++__first;
    }
    else if (__c == __ctype.widen('+')) 
      ++__first;
  }

  for ( ; __first != __last ; ++__first) {
    int __n = __get_digit(*__first, __digits, __xdigits, __separator);
    if (__n < 0) {               // Not a digit
      if (__do_sep && __n == -1) // Separator.  Just ignore it.
        continue;
      else                       // Something other than digit or separator.
        break;
    }
    else if (__n >= __base)
      break;

    _Integer __next = __base * __result + __n;
    ++__got;
    if (__result != 0)
      __overflow = __overflow || __next <= __result;
    __result = __next;
  }

  __val = __overflow
    ? numeric_limits<_Integer>::min()
    : (__negative ? -__result : __result);
  return make_pair(__first, __got > 0 && !__overflow);
}

// __get_integer for char

template <class _InputIter, class _Integer>
bool
__get_integer_nogroup(_InputIter& __first, _InputIter& __last,
                      int __base, _Integer& __val,
                      int __got, bool __is_negative) 
{
  bool __overflow = false;
  _Integer __result = 0;

  if (__first == __last) {      // We may have already read a 0.  If so,
    if (__got > 0) {            // the result is 0 even if we're at eof.
      __val = 0;
      return true;
    }
    else
      return false;
  }

  if (numeric_limits<_Integer>::is_signed) {
      
    _Integer __min_over_base = numeric_limits<_Integer>::min() / __base;

    for ( ; __first != __last ; ++__first) {
      char __c = *__first;
      int __n = -1;

      if (__c >= '0' && __c <= '9')
        __n = __c - '0';
      else if (__c >= 'a' && __c <= 'f')
        __n = __c  + 10 - 'a';
      else if (__c >= 'A' && __c <= 'F')
        __n = __c + 10 - 'A';

      if (__n < 0 || __n >= __base)
        break;
      ++__got;
      if (__result < __min_over_base)
        __overflow = true;  // don't need to keep accumulating
      else {
        _Integer __next = __base * __result - __n;
        if (__result != 0)
        __overflow = __overflow || __next >= __result;
      __result = __next;
      }
    }

    __val = __overflow
      ? __is_negative ? numeric_limits<_Integer>::min()
                      : numeric_limits<_Integer>::max()
      : (__is_negative ? __result : -__result);
  }

  else {

    _Integer __max_over_base = numeric_limits<_Integer>::max() / __base;

    for ( ; __first != __last ; ++__first) {
      char __c = *__first;
      int __n = -1;

      if (__c >= '0' && __c <= '9')
        __n = __c - '0';
      else if (__c >= 'a' && __c <= 'f')
        __n = __c  + 10 - 'a';
      else if (__c >= 'A' && __c <= 'F')
        __n = __c + 10 - 'A';

      if (__n < 0 || __n >= __base)
        break;
      ++__got;
      if (__result > __max_over_base)
        __overflow = true;  //don't need to keep accumulating
      else {
        _Integer __next = __base * __result + __n;
        if (__result != 0)
          __overflow = __overflow || __next <= __result;
        __result = __next;
      }
    }

    __val = __overflow
      ? numeric_limits<_Integer>::max()
      : (__is_negative ? -__result : __result);
  }

  return ((__got > 0) && !__overflow);  
}

extern bool __valid_grouping(const string &, const string &);

template <class _InputIter, class _Integer>
bool
__get_integer_group(_InputIter& __first, _InputIter& __last,
                    int __base, _Integer& __val,
                    char __separator, const string& __grouping,
                    int __got, bool __is_negative)
{
  bool __overflow = false;
  _Integer __result = 0;
  string __group_sizes;
  char __current_group_size = 0;

  if (__first == __last) {      // We may have already read a 0.  If so,
    if (__got > 0) {            // the result is 0 even if we're at eof.
      __val = 0;
      return true;
    }
    else
      return false;
  }

  if (numeric_limits<_Integer>::is_signed) {

    _Integer __min_over_base = numeric_limits<_Integer>::min() / __base;

    for ( ; __first != __last ; ++__first) {
      char __c = *__first;
      int __n = -1;

      if (__c == __separator) {
        __group_sizes.push_back(__current_group_size);
        __current_group_size = 0;
        continue;
      }

      if (__c >= '0' && __c <= '9')
        __n = __c - '0';
      else if (__c >= 'a' && __c <= 'f')
        __n = __c  + 10 - 'a';
      else if (__c >= 'A' && __c <= 'F')
        __n = __c + 10 - 'A';

      if (__n < 0 || __n >= __base)
        break;
      ++__got;
      ++__current_group_size;

      if (__result < __min_over_base)
        __overflow = true;  // don't need to keep accumulating
      else {
        _Integer __next = __base * __result - __n;
        if (__result != 0)
        __overflow = __overflow || __next >= __result;
      __result = __next;
      }
    }

  if (__group_sizes.size() != 0)
    __group_sizes.push_back(__current_group_size);
  
  __val = __overflow
    ? __is_negative ? numeric_limits<_Integer>::min()
                    : numeric_limits<_Integer>::max()
    : (__is_negative ? __result : -__result);
  }

  else {

    _Integer __max_over_base = numeric_limits<_Integer>::max() / __base;

    for ( ; __first != __last ; ++__first) {
      char __c = *__first;
      int __n = -1;

      if (__c == __separator) {
        __group_sizes.push_back(__current_group_size);
        __current_group_size = 0;
        continue;
      }

      if (__c >= '0' && __c <= '9')
        __n = __c - '0';
      else if (__c >= 'a' && __c <= 'f')
        __n = __c  + 10 - 'a';
      else if (__c >= 'A' && __c <= 'F')
        __n = __c + 10 - 'A';

      if (__n < 0 || __n >= __base)
        break;

      ++__got;
      ++__current_group_size;

      if (__result > __max_over_base)
        __overflow = true;  //don't need to keep accumulating
      else {
        _Integer __next = __base * __result + __n;
        if (__result != 0)
          __overflow = __overflow || __next <= __result;
        __result = __next;
      }
    }

    if (__group_sizes.size() != 0)
      __group_sizes.push_back(__current_group_size);
  
    __val = __overflow
      ? numeric_limits<_Integer>::max()
      : (__is_negative ? -__result : __result);
  }
  
  reverse(__group_sizes.begin(), __group_sizes.end());
  return __got > 0   && !__overflow &&
    __valid_grouping(__group_sizes, __grouping);
}

template <class _InputIter, class _CharT>
int __get_base_or_zero(_InputIter& __in, _InputIter& __end,
                       ios_base::fmtflags __basefield,
                       _CharT __zero_char, _CharT __x_char, _CharT __X_char)
{
  int __base;
  int __valid_zero = 0;
  
  switch (__basefield) {
  case ios_base::oct:
    __base = 8;
    break;
  case ios_base::dec:
    __base = 10;
    break;
  case ios_base::hex:
    __base = 16;
    if (__in != __end && *__in == __zero_char) {
      ++__in;
      if (__in != __end &&
          (*__in == __x_char || *__in == __X_char))
        ++__in;
      else
        __valid_zero = 1; // That zero is valid by itself.
    }
    break;
  default:
    if (__in != __end && *__in == __zero_char) {
      ++__in;
      if (__in != __end &&
          (*__in == __x_char || *__in == __X_char)) {
        ++__in;
        __base = 16;
      }
      else {
          __base = 8;
          __valid_zero = 1; // That zero is still valid by itself.
        }
    }
    else
      __base = 10;
    break;
  }
  return (__base << 1) | __valid_zero;
}

template <class _Integer, class _IterType, class _CharT>
_IterType _M_do_get_integer(_IterType __in, _IterType __end, ios_base& __str,
                            ios_base::iostate& __err, _Integer& __val, _CharT)
{
  locale __loc = __str.getloc();

  const ctype<_CharT>& __ct = use_facet<ctype<_CharT> >(__loc);
  const char __narrow_atoms[5] = {'+', '-', '0', 'x', 'X'};
  _CharT __atoms[5];
  __ct.widen(__narrow_atoms, __narrow_atoms + 5, __atoms);
  _CharT __plus_char  = __atoms[0];
  _CharT __minus_char = __atoms[1];
  _CharT __zero_char  = __atoms[2];
  _CharT __x_char     = __atoms[3];
  _CharT __X_char     = __atoms[4];
  
  bool __negative = false;
  _CharT __c = *__in;
  if (__c == __minus_char) {
      __negative = true;
      ++__in;
  }
  else if (__c == __plus_char) 
      ++__in;

  const ios_base::fmtflags __basefield = __str.flags() & ios_base::basefield;
  const int __base_or_zero
    = __get_base_or_zero(__in, __end, __basefield,
                         __zero_char, __x_char, __X_char);
  const int __got = __base_or_zero & 1;
  const int __base = __base_or_zero >> 1;

  typedef numpunct<_CharT> _Numpunct;

  const _Numpunct& __numpunct = use_facet<_Numpunct>(__loc);
  _CharT __separator  = __numpunct.thousands_sep();
  string __grouping = __numpunct.grouping();

  bool __result =
    __grouping.empty() ?
      __get_integer_nogroup(__in, __end, __base,  __val, __got, __negative)
                       :
      __get_integer_group  (__in, __end, __base, __val,
                            __separator, __grouping, __got, __negative);

  __err = __result ? ios_base::goodbit : ios_base::failbit;
  if (__in == __end)
    __err |= ios_base::eofbit;
  return __in;
}


// _M_do_get_float and its helper functions.

template <class _InputIter, class _CharT>
_InputIter __copy_sign(_InputIter __first, _InputIter __last,
                       string& __v,
                       _CharT __plus, _CharT __minus) {
  if (__first != __last) {
    _CharT __c = *__first;
    if (__c == __plus)
      ++__first;
    else if (__c == __minus) {
      __v.push_back('-');
      ++__first;
    }
  }
  return __first;
}



inline pair<char, bool> __get_fdigit(char __c, const char*)
  { return pair<char, bool>(__c, __c >= '0' && __c <= '9'); }

pair<char, bool> __get_fdigit(wchar_t, const wchar_t*);

inline pair<char, bool> __get_fdigit_or_sep(char __c, char __sep,
                                            const char *) {
  return __c == __sep ? pair<char, bool>(',', true)
                      : pair<char, bool>(__c, __c >= '0' && __c <= '9');
}

pair<char, bool> __get_fdigit_or_sep(wchar_t, wchar_t, const wchar_t*);

template <class _InputIter, class _CharT>
pair<_InputIter, bool>
__copy_digits(_InputIter __first, _InputIter __last,
              string& __v, const _CharT* __digits)
{
  bool __ok = false;

  for ( ; __first != __last; ++__first) {
    pair<char, bool> __tmp = __get_fdigit(*__first, __digits);
    if (__tmp.second) {
      __v.push_back(__tmp.first);
      __ok = true;
    }
    else
      break;
  }
  return make_pair(__first, __ok);
}

template <class _InputIter, class _CharT>
pair<_InputIter, bool>
__copy_grouped_digits(_InputIter __first, _InputIter __last,
                      string& __v, const _CharT * __digits,
                      _CharT __sep, const string& __grouping,
                      bool& __grouping_ok)
{
  bool __ok = false;
  string __group_sizes;
  char __current_group_size = 0;

  for ( ; __first != __last; ++__first) {
    pair<char, bool> __tmp = __get_fdigit_or_sep(*__first, __sep,
                                                 __digits);
    if (__tmp.second) {
      if (__tmp.first == ',') {
        __group_sizes.push_back(__current_group_size);
        __current_group_size = 0;
      }
      else {
        __ok = true;
        __v.push_back(__tmp.first);
        ++__current_group_size;
      }
    }
    else
      break;
  }
  
  if (__group_sizes.size() != 0)
    __group_sizes.push_back(__current_group_size);
  
  reverse(__group_sizes.begin(), __group_sizes.end());
  __grouping_ok = __valid_grouping(__group_sizes, __grouping);
  return make_pair(__first, __ok);      
}
    

void __initialize_get_float(const locale&,
                            char&, char&, char&, char&, char&,
                            char&, string&,
                            char*);

void __initialize_get_float(const locale&,
                            wchar_t&, wchar_t&, wchar_t&, wchar_t&, wchar_t&,
                            wchar_t&, string&, wchar_t*);

void __string_to_float(const string&, float&);
void __string_to_float(const string&, double&);
void __string_to_float(const string&, long double&);

template <class _Float, class _IterType, class _CharT>
_IterType _M_do_get_float(_IterType __in, _IterType __end, ios_base& __s,
                          ios_base::iostate& __err, _Float& __val, _CharT) {

  // Create a string, copying characters of the form 
  // [+-]? [0-9]* .? [0-9]* ([eE] [+-]? [0-9]+)?

  bool __digits_before_dot = false;
  bool __digits_after_dot = false;
  bool __ok;
  pair<_IterType, bool> __tmp;
  string __buf;

  typedef numpunct<_CharT> _Numpunct;
  locale __loc = __s.getloc();

  _CharT __digits[10];
  _CharT __plus;
  _CharT __minus;
  _CharT __dot;
  _CharT __pow_e;
  _CharT __pow_E;
  _CharT __sep;
  string __grouping;
  bool   __grouping_ok = true;

  __initialize_get_float(__loc,
                         __plus, __minus, __dot, __pow_e, __pow_E,
                         __sep, __grouping, __digits);

  // Get an optional sign
  __in = __copy_sign(__in, __end, __buf, __plus, __minus);

  // Get an optional string of digits.
  if (__grouping.size() != 0)
    __tmp = __copy_grouped_digits(__in, __end, __buf, __digits,
                                  __sep, __grouping, __grouping_ok);
  else
    __tmp = __copy_digits(__in, __end, __buf, __digits);
  __in = __tmp.first;
  __digits_before_dot = __tmp.second;
    
  // Get an optional decimal point, and an optional string of digits.
  if (__in != __end && *__in == __dot) {
    __buf.push_back('.');
    ++__in;

    __tmp = __copy_digits(__in, __end, __buf, __digits);
    __in = __tmp.first;
    __digits_after_dot = __tmp.second;
  }

  // There have to be some digits, somewhere.
  __ok = __digits_before_dot || __digits_after_dot;
  
  // Get an optional exponent.
  if (__ok && __in != __end && (*__in == __pow_e || *__in == __pow_E)) {
    __buf.push_back('e');
    ++__in;
    __in = __copy_sign(__in, __end, __buf, __plus, __minus);
    __tmp = __copy_digits(__in, __end, __buf, __digits);
    __in = __tmp.first;
    __ok = __tmp.second;        // If we have an exponent then the sign 
                                // is optional but the digits aren't.
  }

  __string_to_float(__buf, __val);
  
  __err = __ok ? ios_base::goodbit : ios_base::failbit;
  if (__tmp.first == __end)
    __err |= ios_base::eofbit;
  return __tmp.first;
}

template <class _CharT, class _InputIter>
_InputIter
num_get<_CharT, _InputIter>::do_get(_InputIter __in, _InputIter __end,
                                    ios_base& __s,
                                    ios_base::iostate& __err, bool& __x) const
{
  if (__s.flags() & ios_base::boolalpha) {
    locale __loc = __s.getloc();
    const numpunct<_CharT>& __np = use_facet<numpunct<_CharT> >(__loc);

    const basic_string<_CharT> __truename  = __np.truename();
    const basic_string<_CharT> __falsename = __np.falsename();
    bool __true_ok  = true;
    bool __false_ok = true;

    typename basic_string<_CharT>::size_type __n = 0;
    for ( ; __in != __end; ++__in) {
      char __c = *__in;
      __true_ok  = __true_ok  && (__c == __truename[__n]);
      __false_ok = __false_ok && (__c == __falsename[__n]);
      ++__n;

      if ((!__true_ok && !__false_ok) ||
          (__true_ok  && __n >= __truename.size()) ||
          (__false_ok && __n >= __falsename.size())) {
        ++__in;
        break;
      }
    }
    if (__true_ok  && __n < __truename.size())  __true_ok  = false;
    if (__false_ok && __n < __falsename.size()) __false_ok = false;
    
    if (__true_ok || __false_ok) {
      __err = ios_base::goodbit;
      __x = __true_ok;
    }
    else
      __err = ios_base::failbit;

    if (__in == __end)
      __err |= ios_base::eofbit;

    return __in;
  }

  else {
    long __lx;
    _InputIter __tmp = this->do_get(__in, __end, __s, __err, __lx);
    if (!(__err & ios_base::failbit)) {
      if (__lx == 0)
        __x = false;
      else if (__lx == 1)
        __x = true;
      else
        __err |= ios_base::failbit;
    }
    return __tmp;
  }
}


template <class _Integer, class _IterType, class _CharType>
_IterType _M_do_put_integer(_IterType __s, ios_base& __f, _CharType __fill,
                            _Integer  __val);
template <class _Float, class _IterType, class _CharType>
_IterType _M_do_put_float(_IterType __s, ios_base& __f, _CharType __fill,
                          _Float    __x);



//----------------------------------------------------------------------
// num_put facet

template <class _CharT, class _OutputIter = ostreambuf_iterator<_CharT> >  
class num_put: public locale::facet
{
  friend class _Locale_impl;
public:
  typedef _CharT      char_type;
  typedef _OutputIter iter_type;

  explicit num_put(size_t __refs = 0) : locale::facet(__refs) {}

  iter_type put(iter_type __s, ios_base& __f, char_type __fill,
                bool __val) const {
    return do_put(__s, __f, __fill, __val);
  }

  iter_type put(iter_type __s, ios_base& __f, char_type __fill,
               long __val) const {
    return do_put(__s, __f, __fill, __val);
  }

  iter_type put(iter_type __s, ios_base& __f, char_type __fill,
                unsigned long __val) const {
    return do_put(__s, __f, __fill, __val);
  }

#ifdef __STL_LONG_LONG
  iter_type put(iter_type __s, ios_base& __f, char_type __fill,
                long long __val) const {
    return do_put(__s, __f, __fill, __val);
  }

  iter_type put(iter_type __s, ios_base& __f, char_type __fill,
                unsigned long long __val) const {
    return do_put(__s, __f, __fill, __val);
  }
#endif

  iter_type put(iter_type __s, ios_base& __f, char_type __fill,
                double __val) const {
    return do_put(__s, __f, __fill, __val);
  }

  iter_type put(iter_type __s, ios_base& __f, char_type __fill,
                long double __val) const {
    return do_put(__s, __f, __fill, __val);
  }

  iter_type put(iter_type __s, ios_base& __f, char_type __fill,
                const void * __val) const {
    return do_put(__s, __f, __fill, __val);
  }

  static locale::id id;

protected:
  ~num_put();   

  iter_type _M_do_put_bool(iter_type __s, ios_base& __f, char_type __fill,
                           bool    __x) const;

  virtual iter_type do_put(iter_type __s, ios_base& __f, char_type __fill,
                           bool __val) const {
    return __f.flags() & ios_base::boolalpha
      ? _M_do_put_bool(__s, __f, __fill, __val)
      : _M_do_put_integer(__s, __f, __fill, static_cast<long>(__val));
  }

  virtual iter_type do_put(iter_type __s, ios_base& __f, char_type __fill,
                           long __val) const {
    return _M_do_put_integer(__s, __f, __fill, __val);
  }


  virtual iter_type do_put(iter_type __s, ios_base& __f, char_type __fill,
                           unsigned long __val) const {
    return _M_do_put_integer(__s, __f, __fill, __val);
  }

#ifdef __STL_LONG_LONG
  virtual iter_type do_put(iter_type __s, ios_base& __f, char_type __fill,
                           long long __val) const {
    return _M_do_put_integer(__s, __f, __fill, __val);
  }
    
  virtual iter_type do_put(iter_type __s, ios_base& __f, char_type __fill,
                          unsigned long long __val) const {
    return _M_do_put_integer(__s, __f, __fill, __val);
  }
    
#endif /* __STL_LONG_LONG */

  virtual iter_type do_put(iter_type __s, ios_base& __f, char_type __fill,
                           double __val) const {
    return _M_do_put_float(__s, __f, __fill, __val);
  }

  virtual iter_type do_put(iter_type __s, ios_base& __f, char_type __fill,
                           long double __val) const {
    return _M_do_put_float(__s, __f, __fill, __val);
  }

  virtual iter_type do_put(iter_type __s, ios_base& __f, char_type __fill,
                           const void* __val) const {
    return _M_do_put_integer(__s, __f, __fill,
                             reinterpret_cast<unsigned long>(__val));
  }
};

template <class _CharT, class _OutputIter>
num_put<_CharT, _OutputIter>::~num_put()
{}

// _M_do_put_float and its helper functions.  Strategy: write the output
// to a buffer of char, transform the buffer to _CharT, and then copy
// it to the output.

char* __write_float(char*, ios_base::fmtflags, int, double);
char* __write_float(char*, ios_base::fmtflags, int, long double);

ptrdiff_t __insert_grouping(char*, char*, const string&, char, int);
ptrdiff_t __insert_grouping(wchar_t*, wchar_t*, const string&,
                            wchar_t, wchar_t, wchar_t, int);
ptrdiff_t __insert_grouping(char* first, char* last, const string&,
                            char, char, char, int);
ptrdiff_t __insert_grouping(wchar_t* first, wchar_t* last, const string&,
                            wchar_t, wchar_t, wchar_t, int );

wchar_t* __convert_float_buffer(const char*, const char*, wchar_t*,
                                const ctype<wchar_t>&, wchar_t);
void __adjust_float_buffer(char*, char*, char);

template <class _CharT, class _OutputIter>
_OutputIter __copy_float_and_fill(const _CharT* __first, const _CharT* __last,
                                  _OutputIter __out,
                                  ios_base::fmtflags __flags,
                                  int __width, _CharT __fill,
                                  _CharT __plus, _CharT __minus) {
  if (__width <= __last - __first)
    return copy(__first, __last, __out);
  else {
    int __pad = __width - (__last - __first);
    ios_base::fmtflags __dir = __flags & ios_base::adjustfield;

    if (__dir == ios_base::left) {
      __out = copy(__first, __last, __out);
      return fill_n(__out, __pad, __fill);
    }
    else if (__dir == ios_base::internal && __first != __last &&
             (*__first == __plus || *__first == __minus)) {
      *__out++ = *__first++;
      __out = fill_n(__out, __pad, __fill);
      return copy(__first, __last, __out);
    }
    else {
      __out = fill_n(__out, __pad, __fill);
      return copy(__first, __last, __out);
    }
  }
}

// Helper routine for wchar_t
template <class _OutputIter>
_OutputIter __put_float(char* __ibuf, char* __iend, _OutputIter __out,
                        ios_base& __f, wchar_t __fill,
                        const locale& __loc, wchar_t __decimal_point,
                         wchar_t __sep, const string& __grouping)
{
  const ctype<wchar_t>& __ct = use_facet<ctype<wchar_t> >(__loc);

  wchar_t __wbuf[128];
  wchar_t* __eend = __convert_float_buffer(__ibuf, __iend, __wbuf,
                                           __ct, __decimal_point);
  if (__grouping.size() != 0) {
    // In order to do separator-insertion only to the left of the
    // decimal point, we adjust the size of the first (right-most)
    // group.  We need to be careful if there is only one entry in
    // grouping:  in this case we need to duplicate the first entry.

    string __new_grouping = __grouping;
    wchar_t* __decimal_pos = find(__wbuf, __eend, __decimal_point);
    if (__grouping.size() == 1)
      __new_grouping.push_back(__grouping[0]);
    __new_grouping[0] += __eend - __decimal_pos;
    ptrdiff_t __len = __insert_grouping(__wbuf, __eend, __new_grouping,
                                        __sep,
                                        __ct.widen('+'), __ct.widen('-'),
                                        0);
    __eend = __wbuf + __len;
  }

  return __copy_float_and_fill(__wbuf, __eend, __out,
                               __f.flags(), __f.width(0), __fill,
                               __ct.widen('+'), __ct.widen('-')); 
}

// Helper routine for char
template <class _OutputIter>
inline _OutputIter __put_float(char* __ibuf, char* __iend, _OutputIter __out,
                               ios_base& __f, char __fill,
                               const locale&, char __decimal_point,
                               char __sep, const string& __grouping)
{
  __adjust_float_buffer(__ibuf, __iend, __decimal_point);
  if (__grouping.size() != 0) {
    string __new_grouping = __grouping;
    const char * __decimal_pos = find(__ibuf, __iend, __decimal_point);
    if (__grouping.size() == 1)
      __new_grouping.push_back(__grouping[0]);
    __new_grouping[0] += __iend - __decimal_pos;
    ptrdiff_t __len = __insert_grouping(__ibuf, __iend, __new_grouping,
                                        __sep, 0);
    __iend = __ibuf + __len;
  }

  return __copy_float_and_fill(__ibuf, __iend, __out,
                               __f.flags(), __f.width(0), __fill, '+', '-');
}


// _M_do_put_integer and its helper functions.

char* __write_integer(char*, ios_base::fmtflags, long);
char* __write_integer(char*, ios_base::fmtflags, unsigned long);
#ifdef __STL_LONG_LONG
char* __write_integer(char*, ios_base::fmtflags, long long);
char* __write_integer(char*, ios_base::fmtflags, unsigned long long);
#endif /* __STL_LONG_LONG */

template <class _CharT, class _OutputIter>
_OutputIter
__copy_integer_and_fill(const _CharT* __buf, ptrdiff_t __len,
                        _OutputIter __out,
                        ios_base::fmtflags __flg, int __wid, _CharT __fill,
                        _CharT __plus, _CharT __minus)
{
  if (__len >= __wid)
    return copy(__buf, __buf + __len, __out);
  else {
    ptrdiff_t __pad = __wid - __len;
    ios_base::fmtflags __dir = __flg & ios_base::adjustfield;

    if (__dir == ios_base::left) {
      __out = copy(__buf, __buf + __len, __out);
      return fill_n(__out, __pad, __fill);
    }
    else if (__dir == ios_base::internal && __len != 0 &&
             (__buf[0] == __plus || __buf[0] == __minus)) {
      *__out++ = __buf[0];
      __out = fill_n(__out, __pad, __fill);
      return copy(__buf + 1, __buf + __len, __out);
    }
    else if (__dir == ios_base::internal && __len >= 2 &&
             (__flg & ios_base::showbase) &&
             (__flg & ios_base::basefield) == ios_base::hex) {
      *__out++ = __buf[0];
      *__out++ = __buf[1];
      __out = fill_n(__out, __pad, __fill);
      return copy(__buf + 2, __buf + __len, __out);
    }
    else {
      __out = fill_n(__out, __pad, __fill);
      return copy(__buf, __buf + __len, __out);
    }
  }
}

// Helper function for wchar_t
template <class _OutputIter>
_OutputIter
__put_integer(char* __buf, char* __iend, _OutputIter __s,
              const locale& __loc, const string& __grouping, wchar_t __sep,
              ios_base::fmtflags __flags, int __wid, wchar_t __fill)
{
  const ctype<wchar_t>& __ct = use_facet<ctype<wchar_t> >(__loc);
  wchar_t __plus  = __ct.widen('+');
  wchar_t __minus = __ct.widen('-');

  wchar_t __wbuf[64];
  __ct.widen(__buf, __iend, __wbuf);
  ptrdiff_t __len = __iend - __buf;
  wchar_t* __eend = __wbuf + __len;
  if (!__grouping.empty()) {
    int __basechars;
    if (__flags & ios_base::showbase)
      switch (__flags & ios_base::basefield) {
        case ios_base::hex: __basechars = 2; break;
        case ios_base::oct: __basechars = 1; break;
        default: __basechars = 0;
      }
    else
      __basechars = 0;

    __len = __insert_grouping(__wbuf, __eend, __grouping, __sep,
                              __plus, __minus, __basechars);
  }

  return __copy_integer_and_fill(__wbuf, __len, __s,
                                 __flags, __wid, __fill,
                                 __plus, __minus);
}

// Helper function for char
template <class _OutputIter>
_OutputIter
__put_integer(char* __buf, char* __iend, _OutputIter __s,
              const locale&, const string& __grouping, char __sep,
              ios_base::fmtflags __flags, int __wid, char __fill)
{
  ptrdiff_t __len = __iend - __buf;

  if (!__grouping.empty()) {
    int __basechars;
    if (__flags & ios_base::showbase)
      switch (__flags & ios_base::basefield) {
        case ios_base::hex: __basechars = 2; break;
        case ios_base::oct: __basechars = 1; break;
        default: __basechars = 0;
      }
    else
      __basechars = 0;
    __len = __insert_grouping(__buf, __iend, __grouping, __sep, __basechars);
  }
  
  return __copy_integer_and_fill(__buf, __len, __s,
                                 __flags, __wid, __fill,
                                 '+', '-');
}


template <class _Integer, class _IterType, class _CharType>
_IterType _M_do_put_integer(_IterType __s, ios_base& __f, _CharType __fill,
                            _Integer  __val)
{
  char __buf[64];               // Large enough for a base 8 64-bit integer,
                                // plus any necessary grouping.

  ios_base::fmtflags __flags = __f.flags();
  char* __iend = __write_integer(__buf, __f.flags(), __val);  
  locale __loc = __f.getloc();
  const numpunct<_CharType>& __np = use_facet<numpunct<_CharType> >(__loc);
  string __grouping  = __np.grouping();
  _CharType __sep = __np.thousands_sep();
  
  return __put_integer(__buf, __iend, __s,
                       __loc, __grouping, __sep,
                       __flags, __f.width(0), __fill);
}
  

template <class _Float, class _IterType, class _CharType>
_IterType _M_do_put_float(_IterType __s, ios_base& __f, _CharType __fill,
                          _Float    __x) {
  char   __buf[128];
  char* __iend = __write_float(__buf, __f.flags(), __f.precision(), __x);
  
  locale __loc = __f.getloc();
  const numpunct<_CharType>& __np = use_facet<numpunct<_CharType> >(__loc);
  return __put_float(__buf, __iend, __s, __f, __fill,
                     __loc, __np.decimal_point(),
                     __np.thousands_sep(), __np.grouping());
}



// _M_do_put_bool, used only for alpha output of bool.

template <class _CharT, class _OutputIter>
_OutputIter 
num_put<_CharT, _OutputIter>
  ::_M_do_put_bool(iter_type __s, ios_base& __f, char_type __fill,
                   bool    __x) const 
{
  locale __loc = __f.getloc();
  typedef numpunct<_CharT> _Punct;
  const _Punct& __np = use_facet<_Punct>(__loc);
  basic_string<_CharT> __str = __x ? __np.truename() : __np.falsename();

  // Reuse __copy_integer_and_fill.  Since internal padding makes no
  // sense for bool, though, make sure we use something else instead.
  // The last two argument to __copy_integer_and_fill are dummies.
  ios_base::fmtflags __flags = __f.flags();
  if ((__flags & ios_base::adjustfield) == ios_base::internal)
    __flags = (__flags & ~ios_base::adjustfield) | ios_base::right;

  return __copy_integer_and_fill(__str.c_str(), __str.size(), __s,
                                 __flags, __f.width(0), __fill,
                                 (_CharT) 0, (_CharT) 0);
}

__STL_END_NAMESPACE

#endif /* __SGI_STL_INTERNAL_NUMERIC_FACETS_H */

// Local Variables:
// mode:C++
// End:

