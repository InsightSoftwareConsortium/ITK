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


#ifndef __SGI_STL_INTERNAL_MONETARY_H
#define __SGI_STL_INTERNAL_MONETARY_H

__STL_BEGIN_NAMESPACE

class money_base {
public:
  enum part {none, space, symbol, sign, value};
  struct pattern {
    char field[4];
  };
};

// moneypunct facets: forward declaration
template <class _charT, bool _International = false> class moneypunct;

// money_get facets

// helper functions for do_get

template <class _InIt1, class _InIt2>
pair<_InIt1, bool> __get_string(_InIt1 __first,     _InIt1 __last,
                               _InIt2 __str_first, _InIt2 __str_last) {
  pair<_InIt1, _InIt2> __pr = mismatch(__first, __last, __str_first);
  return make_pair(__pr.first, __pr.second == __str_last);
}

template <class _InIt, class _OuIt, class _CharT>
pair<_InIt, bool> __get_monetary_value(_InIt __first, _InIt __last,
                                       _OuIt    __out,
                                       const ctype<_CharT>& __ctype,
                                       _CharT   __point,
                                       int      __frac_digits,
				       bool&    __syntax_ok)
{
  if (__first == __last || !__ctype.is(ctype_base::digit, *__first))
    return make_pair(__first, false);

  while (__first != __last && __ctype.is(ctype_base::digit, *__first))
    *__out++ = *__first++;

  if (__first == __last || *__first != __point) {
    for (int __digits = 0; __digits != __frac_digits; ++__digits)
      *__out++ = _CharT('0');
    return make_pair(__first, true);
  }

  ++__first;

  int __digits = 0;

  while (__first != __last && __ctype.is(ctype_base::digit, *__first)) {
    *__out++ = *__first++;
    ++__digits;
  }

  __syntax_ok = (__digits == __frac_digits);  

  return make_pair(__first, true);
}

template <class _InIt, class _OuIt, class _CharT>
pair<_InIt, bool>
__get_monetary_value(_InIt __first, _InIt __last, _OuIt __out,
                     const ctype<_CharT>& __ctype,
                     _CharT   __point,
                     int      __frac_digits,
                     _CharT __sep,
                     const string& __grouping,
		     bool&         __syntax_ok)
{
  if (__first == __last || !__ctype.is(ctype_base::digit, *__first))
    return make_pair(__first, false);

  string __group_sizes;
  char   __current_group_size = 0;

  while (__first != __last) {
    if (__ctype.is(ctype_base::digit, *__first)) {
      ++__current_group_size;
      *__out++ = *__first++;
    }
    else
    if (*__first == __sep) {
      __group_sizes.push_back(__current_group_size);
      __current_group_size = 0;
      ++__first;
    }
    else
      break;
  }

  if (__group_sizes.size() != 0)
    __group_sizes.push_back(__current_group_size);
  reverse(__group_sizes.begin(), __group_sizes.end());
  __syntax_ok = __valid_grouping(__group_sizes, __grouping);  

  if (__first == __last || *__first != __point) {
    for (int __digits = 0; __digits != __frac_digits; ++__digits)
      *__out++ = _CharT('0');
    return make_pair(__first, true); // OK not to have decimal point
  }

  ++__first;

  int __digits = 0;

  while (__first != __last && __ctype.is(ctype_base::digit, *__first)) {
      *__out++ = *__first++;
     ++__digits;
  }

  __syntax_ok = __syntax_ok && (__digits == __frac_digits);

  return make_pair(__first, true);
}

template <class _CharT, class _InputIter = istreambuf_iterator<_CharT> >
class money_get : public locale::facet 
{
  friend class _Locale_impl;
public:
  typedef _CharT               char_type;
  typedef _InputIter           iter_type;
  typedef basic_string<_CharT> string_type;

  explicit money_get(size_t __refs = 0): locale::facet(__refs) {}

  inline iter_type get(iter_type __s, iter_type  __end, bool __intl,
                       ios_base&  __str, ios_base::iostate&  __err,
                       long double& __units) const
    { return do_get(__s,  __end, __intl,  __str,  __err, __units); }

  inline iter_type get(iter_type __s, iter_type  __end, bool __intl,
                       ios_base&  __str, ios_base::iostate& __err,
                       string_type& __digits) const
    { return do_get(__s,  __end, __intl,  __str,  __err, __digits); }

  static locale::id id;

protected:
  ~money_get() {}
  virtual iter_type do_get(iter_type __s, iter_type  __end, bool  __intl,
                           ios_base&  __str, ios_base::iostate& __err,
                           long double& __units) const {
    string_type __buf;
    __s = do_get(__s, __end, __intl, __str, __err, __buf);
    if (__err == ios_base::goodbit || __err == ios_base::eofbit) {
      __buf.push_back(0);
      //Can't use atold, since it might be wchar_t.
      __get_integer(__buf.begin(), __buf.end(), 10,
                    char_type(), false, locale::classic(),
                    __units);
    }
    if (__s == __end)
      __err |= ios_base::eofbit;
    return __s;
  }

  virtual iter_type do_get(iter_type __s, iter_type __end, bool __intl,
                           ios_base&  __str, ios_base::iostate& __err,
                           string_type& __digits) const;

};

template <class _CharT, class _InputIter>
_InputIter money_get<_CharT, _InputIter>
  ::do_get(iter_type __s, iter_type  __end, bool  __intl,
           ios_base&  __str, ios_base::iostate&  __err,
           string_type& __digits) const {
  if (__s == __end) {
    __err |= ios_base::eofbit;
    return __s;
  }

  typedef moneypunct<_CharT, false> _Punct;
  typedef moneypunct<_CharT, true>  _Punct_intl;
  typedef ctype<_CharT>             _Ctype;

  locale __loc = __str.getloc();
  const _Punct&      __punct      = use_facet<_Punct>(__loc);
  const _Punct_intl& __punct_intl = use_facet<_Punct_intl>(__loc);
  const _Ctype&      __ctype      = use_facet<_Ctype>(__loc);
                   
  money_base::pattern __format = __intl ? __punct_intl.neg_format()
                                        : __punct.neg_format();
  string_type __ns = __intl ? __punct_intl.negative_sign()
                            : __punct.negative_sign();
  string_type __ps = __intl ? __punct_intl.positive_sign()
                            : __punct.positive_sign();
  int __i;
  bool __is_positive = true;
  bool __symbol_required = (__str.flags() & ios_base::showbase) != 0;
  string_type __buf;
  back_insert_iterator<string_type> __out(__buf);
//  pair<iter_type, bool> __result;

  for (__i = 0; __i < 4; ++__i) {
    switch (__format.field[__i]) {
    case (char) money_base::none:
      if (__i == 3) {
        if (__ctype.is(ctype_base::space, *__s)) {
          __err = ios_base::failbit;
          return __s;
        }
        break;
      }
      while (__s != __end && __ctype.is(ctype_base::space, *__s))
        ++__s;
      break;
    case (char) money_base::space:
      if (!__ctype.is(ctype_base::space, *__s)) {
        __err = ios_base::failbit;
        return __s;
      }
      ++__s;
      while (__s != __end && __ctype.is(ctype_base::space, *__s))
        ++__s;
      break;
    case money_base::symbol: {
      string_type __cs = __intl ? __punct_intl.curr_symbol()
                                : __punct.curr_symbol();
      pair<iter_type, bool>
      __result  = __get_string(__s, __end, __cs.begin(), __cs.end());
      if (!__result.second && __symbol_required)
        __err = ios_base::failbit;
      __s = __result.first;
      break;
    }
    case money_base::sign: {
      if (__s == __end) {
        if (__ps.size() == 0)
          break;
        if (__ns.size() == 0) {
          __is_positive = false;
          break;
        }
        __err = ios_base::failbit;
        return __s;
      }
      else {
        if (__ps.size() == 0) {
          if (__ns.size() == 0)
            break;
          if (*__s == ++__ns[0]) {
            ++__s;
            __is_positive = false;
            break;
          }
          __err = ios_base::failbit;
          return __s;
        } 
        else {
          if (*__s == __ps[0]) {
            ++__s;
            break;
          }
          if (__ns.size() == 0)
            break;
          if (*__s == __ns[0]) {
            ++__s;
            __is_positive = false;
            break;
          }
          __err = ios_base::failbit;
          return __s;
        }
      }
      break;
    }
    case money_base::value: {
      _CharT __point = __intl ? __punct_intl.decimal_point()
                              : __punct.decimal_point();
      int __frac_digits = __intl ? __punct_intl.frac_digits()
                                 : __punct.frac_digits();
      string __grouping = __intl ? __punct_intl.grouping()
                                 : __punct.grouping();
      bool __syntax_ok = true;
      pair<_InputIter, bool> __result;
      if (__grouping.size() == 0) {
        __result = __get_monetary_value(__s, __end, __out, __ctype,
                                        __point, __frac_digits,
					__syntax_ok);
        if (!__syntax_ok)
	  __err |= ios_base::failbit;
	if (!__result.second) {
	  __err |= ios_base::failbit;
	  return __result.first;
	}
      }
      else {
        _CharT __sep = __intl ? __punct_intl.thousands_sep()
                              : __punct.thousands_sep();
        __result = __get_monetary_value(__s, __end, __out, __ctype,
                                        __point, __frac_digits,
                                        __sep,
                                        __grouping, __syntax_ok);
      
	if (!__syntax_ok)
	  __err |= ios_base::failbit;
        if (!__result.second) {
          __err = ios_base::failbit;
	  return __result.first;
        }
      }
      __s = __result.first;
      break;
    }                           // Close money_base::value case
    }                           // Close switch statement
  }                             // Close for loop

  if (__is_positive) {
    if (__ps.size() > 1) {
      pair<_InputIter, bool>
        __result = __get_string(__s, __end, __ps.begin() + 1, __ps.end());
      __s = __result.first;
      if (!__result.second)
	__err |= ios::failbit;
    }
    if (!(__err & ios_base::failbit))
      __digits = __buf;
  }
  else {
    if (__ns.size() > 1) {
      pair<_InputIter, bool>
        __result = __get_string(__s, __end, __ns.begin() + 1, __ns.end());
      __s = __result.first;
      if (!__result.second)
	__err |= ios::failbit;
    }
    if (!(__err & ios::failbit)) {
      __buf.insert(__buf.begin(),
		      use_facet<ctype<_CharT> >(__loc).widen('-'));
      __digits = __buf;
    }
  }
  if (__s == __end)
    __err |= ios::eofbit;
  return __s;
}

// money_put facets

char*    __write_monetary_value(long double, char* __buf,    const locale&);
wchar_t* __write_monetary_value(long double, wchar_t* __buf, const locale&);

template <class _CharT, class _OutputIter = ostreambuf_iterator<_CharT> >
class money_put : public locale::facet {
  friend class _Locale_impl;
public:
  typedef _CharT               char_type;
  typedef _OutputIter          iter_type;
  typedef basic_string<_CharT> string_type;

  explicit money_put(size_t __refs = 0);

  inline iter_type put(iter_type __s, bool __intl, ios_base& __str,
                       char_type  __fill, long double __units) const
    { return do_put(__s, __intl, __str, __fill, __units); }
  inline iter_type put(iter_type __s, bool __intl, ios_base& __str,
                       char_type  __fill, 
                       const string_type& __digits) const
    { return do_put(__s, __intl, __str, __fill, __digits); }

  static locale::id id;

protected:
  ~money_put();
  virtual iter_type do_put(iter_type __s, bool  __intl, ios_base&  __str,
                           char_type __fill, long double __units) const {

    locale __loc = __str.getloc();
    _CharT  __buf[64];
    return do_put(__s, __intl, __str, __fill, __buf + 0);
  }
    
  virtual iter_type do_put(iter_type __s, bool  __intl, ios_base&  __str,
                           char_type __fill,
                           const string_type& __digits) const;
};

template <class _CharT, class _OutputIter>
money_put<_CharT, _OutputIter>::money_put(size_t __refs)
  : locale::facet(__refs)
{}

template <class _CharT, class _OutputIter>
money_put<_CharT, _OutputIter>::~money_put()
{}

ptrdiff_t __insert_grouping(char* first, char* last, const string&,
                            char, char, char, int);
ptrdiff_t __insert_grouping(wchar_t* first, wchar_t* last, const string&,
                            wchar_t, wchar_t, wchar_t, int );

template <class _CharT, class _OutputIter>
_OutputIter
money_put<_CharT, _OutputIter>
 ::do_put(iter_type __s, bool __intl, ios_base& __str,
          char_type __fill,
          const string_type& __digits) const { 
  typedef ctype<_CharT>             _Ctype;
  typedef moneypunct<_CharT, false> _Punct;
  typedef moneypunct<_CharT, true>  _Punct_intl;

  locale __loc = __str.getloc();
  const _Ctype&      __ctype      = use_facet<_Ctype>(__loc);
  const _Punct&      __punct      = use_facet<_Punct>(__loc);
  const _Punct_intl& __punct_intl = use_facet<_Punct_intl>(__loc);

  // some special characters

  char_type __minus = __ctype.widen('-');
  char_type __plus  = __ctype.widen('+');
  char_type __space = __ctype.widen(' ');
  char_type __zero  = __ctype.widen('0');
  char_type __point = __intl ? __ctype.widen(__punct_intl.decimal_point())
			     : __ctype.widen(__punct.decimal_point());

  char_type __sep = __intl ? __punct_intl.thousands_sep()
			   : __punct     .thousands_sep();

  string __grouping = __intl ? __punct_intl.grouping()
		             : __punct     .grouping();
				
  size_t __frac_digits      = __intl ? __punct_intl.frac_digits() 
                                  : __punct.frac_digits();

  string_type __curr_sym = __intl ? __punct_intl.curr_symbol() 
                                  : __punct.curr_symbol();

    // if there are no digits we are going to return __s.  If there
    // are digits, but not enough to fill the frac_digits, we are
    // going to add zeros.  I don't know whether this is right or
    // not.

  if (__digits.size() == 0) 
    return __s;

  typename string_type::const_iterator __digits_first = __digits.begin();
  typename string_type::const_iterator __digits_last  = __digits.end();

  bool __is_negative = *__digits_first == __minus;
  if (__is_negative)
    ++__digits_first;

  string_type __sign = __intl ?
			 __is_negative ? __punct_intl.negative_sign()
				       : __punct_intl.positive_sign()
			      :
			 __is_negative ? __punct.negative_sign()
				       : __punct.positive_sign();
  typename string_type::const_iterator __cp = __digits_first;
  while (__cp != __digits_last && __ctype.is(ctype_base::digit, *__cp))
    ++__cp;
  if (__cp == __digits_first)
    return __s;
  __digits_last = __cp;

  // If grouping is required, we make a copy of __digits and
  // insert the grouping.

  // To handle the fractional digits, we augment the first group
  // by frac_digits.  If there is only one group, we need first
  // to duplicate it.

  string_type __new_digits(__digits_first, __digits_last);

  if (__grouping.size() != 0) {
    if (__grouping.size() == 1)
      __grouping.push_back(__grouping[0]);
    __grouping[0] += __frac_digits;
    ptrdiff_t __value_length = __insert_grouping(__new_digits.begin(),
	  				         __new_digits.end(),
					         __grouping,
					         __sep,
					         __plus, __minus, 0);
    __digits_first = __new_digits.begin();
    __digits_last  = __digits_first + __value_length;
  }

  // Determine the amount of padding required, if any.  
    
  size_t __width        = __str.width();
  size_t __value_length = __digits_last - __digits_first;
  size_t __length       = __value_length;
      
  __length += __sign.size();
  if (__frac_digits != 0)
    ++__length;

  bool __generate_curr = (__str.flags() & ios_base::showbase) != 0;
  if (__generate_curr)
    __length += __curr_sym.size();
  money_base::pattern __format =
    __intl ? (__is_negative ? __punct_intl.neg_format() 
                            : __punct_intl.pos_format())
           : (__is_negative ? __punct.neg_format() 
                            : __punct.pos_format());
  {
    for (int __i = 0; __i < 4; ++__i)
      if (__format.field[__i] == (char) money_base::space)
        ++__length;
  }

  int __fill_amt = __length < __width ? __width - __length : 0;

  ios_base::fmtflags __fill_pos = __str.flags() & ios_base::adjustfield;

  if (__fill_amt != 0 &&
      !(__fill_pos & (ios_base::left | ios_base::internal)))
    __s = fill_n(__s, __fill_amt, __fill);
    
  for (int __i = 0; __i < 4; ++__i) {
    char __ffield = __format.field[__i];
    if (__ffield == money_base::none) {
      if (__fill_amt != 0 && __fill_pos == ios_base::internal)
        __s = fill_n(__s, __fill_amt, __fill);
    }
    else if (__ffield == money_base::space) {
      *__s++ = __space;
      if (__fill_amt != 0 && __fill_pos == ios_base::internal)
        __s = fill_n(__s, __fill_amt, __fill);
    }
    else if (__ffield == money_base::symbol) {
      if (__generate_curr)
        __s = copy(__curr_sym.begin(), __curr_sym.end(), __s);
    }
    else if (__ffield == money_base::sign) {
      if (__sign.size() != 0)
        *__s++ = __sign[0];
    }
    else if (__ffield == money_base::value) {
      if (__frac_digits == 0)
        __s = copy(__digits_first, __digits_last, __s);
      else {
        if (__value_length <= __frac_digits) {
          *__s++ = __point;
          __s = copy(__digits_first, __digits_last, __s);
          __s =  fill_n(__s, __frac_digits - __value_length, __zero);
        }
        else {
          __s = copy(__digits_first, __digits_last - __frac_digits, __s);
          if (__frac_digits != 0) {
            *__s++ = __point;
            __s = copy(__digits_last - __frac_digits, __digits_last, __s);
          }
        }
      }
    }
  } // Close for loop

  // Ouput rest of sign if necessary.

  if (__sign.size() > 1)
    __s = copy(__sign.begin() + 1, __sign.end(), __s);
  if (!(__fill_pos & (ios_base::right | ios_base::internal)))
    __s = fill_n(__s, __fill_amt, __fill);
  

  return __s;
}

// moneypunct facets: definition of specializations

template <> 
class moneypunct<char, true> : public locale::facet, public money_base 
{
  friend class _Locale_impl;
public:
  typedef char                 char_type;
  typedef basic_string<char>   string_type;

  explicit moneypunct(size_t __refs = 0);

  inline char        decimal_point() const { return do_decimal_point(); }
  inline char        thousands_sep() const { return do_thousands_sep(); }
  inline string      grouping()      const { return do_grouping(); }
  inline string_type curr_symbol()   const { return do_curr_symbol(); }
  inline string_type positive_sign() const { return do_positive_sign(); }
  inline string_type negative_sign() const { return do_negative_sign(); }
  inline int         frac_digits()   const { return do_frac_digits(); }
  inline pattern     pos_format()    const { return do_pos_format(); }
  inline pattern     neg_format()    const { return do_neg_format(); }

  static locale::id id;

protected:
  pattern _M_pos_format;
  pattern _M_neg_format;

  ~moneypunct();

  virtual char        do_decimal_point() const;
  virtual char        do_thousands_sep() const;
  virtual string      do_grouping()      const;

  virtual string_type do_curr_symbol()   const;

  virtual string_type do_positive_sign() const;
  virtual string_type do_negative_sign() const;
  virtual int         do_frac_digits()   const;
  virtual pattern     do_pos_format()    const;
  virtual pattern     do_neg_format()    const;
};

template <> 
class moneypunct<char, false> : public locale::facet, public money_base 
{
  friend class _Locale_impl;
public:
  typedef char                 char_type;
  typedef basic_string<char>   string_type;

  explicit moneypunct(size_t __refs = 0);

  inline char        decimal_point() const { return do_decimal_point(); }
  inline char        thousands_sep() const { return do_thousands_sep(); }
  inline string      grouping()      const { return do_grouping(); }
  inline string_type curr_symbol()   const { return do_curr_symbol(); }
  inline string_type positive_sign() const { return do_positive_sign(); }
  inline string_type negative_sign() const { return do_negative_sign(); }
  inline int         frac_digits()   const { return do_frac_digits(); }
  inline pattern     pos_format()    const { return do_pos_format(); }
  inline pattern     neg_format()    const { return do_neg_format(); }

  static locale::id id;

protected:
  pattern _M_pos_format;
  pattern _M_neg_format;

  ~moneypunct();

  virtual char        do_decimal_point() const;
  virtual char        do_thousands_sep() const;
  virtual string      do_grouping()      const;

  virtual string_type do_curr_symbol()   const;

  virtual string_type do_positive_sign() const;
  virtual string_type do_negative_sign() const;
  virtual int         do_frac_digits()   const;
  virtual pattern     do_pos_format()    const;
  virtual pattern     do_neg_format()    const;
};


template <> 
class moneypunct<wchar_t, true> : public locale::facet, public money_base 
{
  friend class _Locale_impl;
public:
  typedef wchar_t                 char_type;
  typedef basic_string<wchar_t>   string_type;

  explicit moneypunct(size_t __refs = 0);

  inline wchar_t     decimal_point() const { return do_decimal_point(); }
  inline wchar_t     thousands_sep() const { return do_thousands_sep(); }
  inline string      grouping()      const { return do_grouping(); }
  inline string_type curr_symbol()   const { return do_curr_symbol(); }
  inline string_type positive_sign() const { return do_positive_sign(); }
  inline string_type negative_sign() const { return do_negative_sign(); }
  inline int         frac_digits()   const { return do_frac_digits(); }
  inline pattern     pos_format()    const { return do_pos_format(); }
  inline pattern     neg_format()    const { return do_neg_format(); }

  static locale::id id;

protected:
  pattern _M_pos_format;
  pattern _M_neg_format;

  ~moneypunct();

  virtual wchar_t     do_decimal_point() const;
  virtual wchar_t     do_thousands_sep() const;
  virtual string      do_grouping()      const;

  virtual string_type do_curr_symbol()   const;

  virtual string_type do_positive_sign() const;
  virtual string_type do_negative_sign() const;
  virtual int         do_frac_digits()   const;
  virtual pattern     do_pos_format()    const;
  virtual pattern     do_neg_format()    const;
};

template <> 
class moneypunct<wchar_t, false> : public locale::facet, public money_base 
{
  friend class _Locale_impl;
public:
  typedef wchar_t                 char_type;
  typedef basic_string<wchar_t>   string_type;

  explicit moneypunct(size_t __refs = 0);

  inline wchar_t     decimal_point() const { return do_decimal_point(); }
  inline wchar_t     thousands_sep() const { return do_thousands_sep(); }
  inline string      grouping()      const { return do_grouping(); }
  inline string_type curr_symbol()   const { return do_curr_symbol(); }
  inline string_type positive_sign() const { return do_positive_sign(); }
  inline string_type negative_sign() const { return do_negative_sign(); }
  inline int         frac_digits()   const { return do_frac_digits(); }
  inline pattern     pos_format()    const { return do_pos_format(); }
  inline pattern     neg_format()    const { return do_neg_format(); }

  static locale::id id;

protected:
  pattern _M_pos_format;
  pattern _M_neg_format;

  ~moneypunct();

  virtual wchar_t     do_decimal_point() const;
  virtual wchar_t     do_thousands_sep() const;
  virtual string      do_grouping()      const;

  virtual string_type do_curr_symbol()   const;

  virtual string_type do_positive_sign() const;
  virtual string_type do_negative_sign() const;
  virtual int         do_frac_digits()   const;
  virtual pattern     do_pos_format()    const;
  virtual pattern     do_neg_format()    const;
};

template <class _charT, bool _International = false> class moneypunct_byname;

template <>
class moneypunct_byname<char, true> : public moneypunct<char, true> 
{
public:
  typedef money_base::pattern   pattern;
  typedef char                  char_type;
  typedef basic_string<char>    string_type;

  explicit moneypunct_byname(const char * __name, size_t __refs = 0);

protected:
  _Locale_monetary* _M_monetary;
  ~moneypunct_byname();
  virtual char        do_decimal_point() const;
  virtual char        do_thousands_sep() const;
  virtual string      do_grouping()      const;

  virtual string_type do_curr_symbol()   const;

  virtual string_type do_positive_sign() const;
  virtual string_type do_negative_sign() const;
  virtual int         do_frac_digits()   const;
};

template <>
class moneypunct_byname<char, false> : public moneypunct<char, false> 
{
public:
  typedef money_base::pattern   pattern;
  typedef char                  char_type;
  typedef basic_string<char>    string_type;

  explicit moneypunct_byname(const char * __name, size_t __refs = 0);

protected:
  _Locale_monetary* _M_monetary;
  ~moneypunct_byname();
  virtual char        do_decimal_point() const;
  virtual char        do_thousands_sep() const;
  virtual string      do_grouping()      const;

  virtual string_type do_curr_symbol()   const;

  virtual string_type do_positive_sign() const;
  virtual string_type do_negative_sign() const;
  virtual int         do_frac_digits()   const;
};

template <>
class moneypunct_byname<wchar_t, true> : public moneypunct<wchar_t, true> 
{
public:
  typedef money_base::pattern   pattern;
  typedef wchar_t               char_type;
  typedef basic_string<wchar_t> string_type;

  explicit moneypunct_byname(const char * __name, size_t __refs = 0);

protected:
  _Locale_monetary* _M_monetary;
  ~moneypunct_byname();
  virtual wchar_t     do_decimal_point() const;
  virtual wchar_t     do_thousands_sep() const;
  virtual string      do_grouping()      const;

  virtual string_type do_curr_symbol()   const;

  virtual string_type do_positive_sign() const;
  virtual string_type do_negative_sign() const;
  virtual int         do_frac_digits()   const;
};

template <>
class moneypunct_byname<wchar_t, false> : public moneypunct<wchar_t, false> 
{
public:
  typedef money_base::pattern   pattern;
  typedef wchar_t               char_type;
  typedef basic_string<wchar_t> string_type;

  explicit moneypunct_byname(const char * __name, size_t __refs = 0);

protected:
  _Locale_monetary* _M_monetary;
  ~moneypunct_byname();
  virtual wchar_t     do_decimal_point() const;
  virtual wchar_t     do_thousands_sep() const;
  virtual string      do_grouping()      const;

  virtual string_type do_curr_symbol()   const;

  virtual string_type do_positive_sign() const;
  virtual string_type do_negative_sign() const;
  virtual int         do_frac_digits()   const;
};

__STL_END_NAMESPACE

#endif /* __SGI_STL_INTERNAL_MONETARY_H */

// Local Variables:
// mode:C++
// End:


