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


#ifndef __SGI_STL_INTERNAL_TIME_FACETS_H
#define __SGI_STL_INTERNAL_TIME_FACETS_H

__STL_BEGIN_NAMESPACE

_Locale_time* __acquire_time(const char* __name);
void          __release_time(_Locale_time* __time);

// Template functions used by time_get

// Matching input against a list of names

// Alphabetic input of the names of months and the names
// of weekdays requires matching input against a list of names.
// We use a simple generic algorithm to accomplish this.  This
// algorithm is not very efficient, especially for longer lists
// of names, but it probably does not matter for the initial
// implementation and it may never matter, since we do not expect
// this kind of input to be used very often.  The algorithm
// could be improved fairly simply by creating a new list of
// names still in the running at each iteration.  A more sophisticated
// approach would be to build a trie to do the matching.
//
// We compare each character of the input to the corresponding
// character of each name on the list that has not been eliminated,
// either because every character in the name has already been
// matched, or because some character has not been matched.  We
// continue only as long as there are some names that have not been
// eliminated.

// We do not really need a random access iterator (a forward iterator
// would do), but the extra generality makes the notation clumsier,
// and we don't really need it.

// We can recognize a failed match by the fact that the second
// component of the return value will be __name_end.

#define _MAXNAMES        64
#define _MAX_NAME_LENGTH 64

/*
  This second __match() function was added so as to
  avoid the use of iterator_traits<>::difference_type, which
  is not as portable as using the __DISTANCE_TYPE and
  external polymorphism.
 */
template <class _InIt, class _RAIt, class _DiffType>
pair<_InIt, _RAIt>
__match(_InIt __first, _InIt __last, _RAIt __name, 
        _RAIt __name_end, _DiffType*) 
{
  typedef _DiffType difference_type;
  difference_type __n = __name_end - __name;
  size_t __max_pos = 0;
  difference_type __i;
  for (__i = 0; __i < __n; ++__i)
    __max_pos = max(__max_pos,  __name[__i].size());
  difference_type __pos = 0;
  difference_type __check_count = __n;
  bool __do_check[_MAXNAMES];
  for (__i = 0; __i < _MAXNAMES; ++__i)
    __do_check[__i] = true;
  _RAIt __matching_name[_MAX_NAME_LENGTH];
  for (__i = 0; __i < _MAX_NAME_LENGTH; ++__i)
    __matching_name[__i] = __name_end;

  while (__first != __last) {
    for (__i = 0; __i < __n; ++__i)
      if (__do_check[__i])
        if (*__first == __name[__i][__pos]) {
          if (__pos == _DiffType(__name[__i].size()) - 1) {
            __do_check[__i] = 0;
            __matching_name[__pos+1] = __name + __i;
            --__check_count;
            if (__check_count == 0)
              return make_pair(++__first, __name + __i);
          }
        }
        else {
          __do_check[__i] = 0;
          --__check_count;
          if (__check_count == 0) 
            return make_pair(__first, __matching_name[__pos]);
        }

    ++__first; ++__pos;
  }

  return make_pair(__first, __matching_name[__pos]);
}

template <class _InIt, class _RAIt>
pair<_InIt, _RAIt>
__match(_InIt __first, _InIt __last, _RAIt __name, _RAIt __name_end) {
  return __match(__first, __last, __name, __name_end,
                 __DISTANCE_TYPE(__name));
}

// Both time_get and time_put need a structure of type _Time_Info
// to provide names and abbreviated names for months and days,
// as well as the am/pm designator.  The month and weekday tables
// have the all the abbreviated names before all the full names.
// The _Time_Info tables are initialized using the non-template
// function __init_timeinfo, which has two overloadings:  one
// with a single reference parameter for the table to be initialized,
// and one with a second _Locale_time * parameter.  The first form
// is called by time_get/put's constructor, and the second form is
// called by time_get/put_byname's constructor.

struct _Time_Info {
  string _M_dayname[14];
  string _M_monthname[24];
  string _M_am_pm[2];
  string _M_time_format;
  string _M_date_format;
  string _M_date_time_format;
};

void __init_timeinfo(_Time_Info&);
void __init_timeinfo(_Time_Info&, _Locale_time*);

class time_base {
public:
  enum dateorder {no_order, dmy, mdy, ymd, ydm};
};

// __get_formatted_time reads input that is assumed to be formatted
// according to the rules for the C strftime function (C standard,
// 7.12.3.5).  This function is used to implement the do_get_time
// and do_get_date virtual functions, which depend on the locale
// specifications for the time and day formats respectively.
// Note the catchall default case, intended mainly for the '%Z'
// format designator, which does not make sense here since the
// representation of timezones is not part of the locale.
//
// The case branches are implemented either by doing a match using
// the appopriate name table or by doing a __get_integer_nogroup.
//
// 'y' format is assumed to mean that the input represents years
// since 1900.  That is, 2002 should be represented as 102.  There
// is no century-guessing.
//
// The match is successful if and only if the second component of the
// return value is format_end.

// Note that the antepenultimate parameter is being used only to determine
// the correct overloading for the calls to __get_integer_nogroup.

template <class _InIt1, class _InIt2, class _Ch>
pair<_InIt1, _InIt2>
__get_formatted_time(_InIt1 __first,  _InIt1 __last,
                     _InIt2 __format, _InIt2 __format_end,
                     _Ch, const _Time_Info& __table,
                     ios_base::iostate& __err,
                     tm*         __t) {
  while(__first != __last && __format != __format_end) {
    if (*__format == '%') {
      ++__format;
      char __c = *__format;
      switch (__c) {
        case 'a': {
          pair<_InIt1, const string*> __pr =
            __match(__first, __last,
                    __table._M_dayname + 0, __table._M_dayname + 7);
            __first = __pr.first;
            if (__pr.second == __table._M_dayname + 7)
              return make_pair(__first, __format);
            __t->tm_wday = __pr.second - __table._M_dayname;
            break;
        }

        case 'A': {
          pair<_InIt1, const string*> __pr =
            __match(__first, __last,
                    __table._M_dayname + 7, __table._M_dayname + 14);
            __first = __pr.first;
            if (__pr.second == __table._M_dayname + 14)
              return make_pair(__first, __format);
            __t->tm_wday = __pr.second - __table._M_dayname - 7;
            break;
        }

        case 'b': {
          pair<_InIt1, const string*> __pr =
            __match(__first, __last,
                    __table._M_monthname + 0, __table._M_monthname + 12);
            __first = __pr.first;
            if (__pr.second == __table._M_monthname + 12)
              return make_pair(__pr.first, __format);
            __t->tm_mon = __pr.second - __table._M_monthname;
            break;
        }

        case 'B': {
          pair<_InIt1, const string*> __pr =
            __match(__first, __last,
                    __table._M_monthname + 12, __table._M_monthname + 24);
            __first = __pr.first;
            if (__pr.second == __table._M_monthname + 24)
              return make_pair(__first, __format);
            __t->tm_mon = __pr.second - __table._M_monthname - 12;
            break;
        }

        case 'd': {
          bool __result = __get_integer_nogroup(__first, __last, 10,
                                                __t->tm_mday, 0, false);
          if (!__result || __t->tm_mday < 1 || __t->tm_mday > 31) {
            __err |= ios_base::failbit;
            return make_pair(__first, __format);
          }
          break;
        }
        
        case 'H': case 'I': {
          bool __result = __get_integer_nogroup(__first, __last, 10,
                                                __t->tm_hour, 0, false);
          if (!__result)
            return make_pair(__first, __format);
          break;
        }

        case 'j': {
          bool __result = __get_integer_nogroup(__first, __last, 10,
                                                __t->tm_yday, 0, false);
          if (!__result)
            return make_pair(__first, __format);
          break;
        }

        case 'm': {
          bool __result = __get_integer_nogroup(__first, __last, 10,
                                                __t->tm_mon, 0, false);
          --__t->tm_mon;
          if (!__result || __t->tm_mon < 0 || __t->tm_mon > 11) {
            __err |= ios_base::failbit;
            return make_pair(__first, __format);
          }
          break;
        }

        case 'M': {
          bool __result = __get_integer_nogroup(__first, __last, 10,
                                                __t->tm_min, 0, false);
          if (!__result)
            return make_pair(__first, __format);
          break;
        }

        case 'p': {
          pair<_InIt1, const string*> __pr =
            __match(__first, __last,
                    __table._M_am_pm + 0, __table._M_am_pm + 2);
          __first = __pr.first;
          if (__pr.second == __table._M_am_pm + 2)
            return make_pair(__first, __format);
          if (__pr.second == __table._M_am_pm + 1)
            __t->tm_hour += 12;
          break;
        }

        case 'S': {
          bool __result = __get_integer_nogroup(__first, __last, 10,
                                                __t->tm_sec, 0, false);
          if (!__result)
            return make_pair(__first, __format);
          break;
        }

        case 'y': {
          bool __result = __get_integer_nogroup(__first, __last, 10,
                                                __t->tm_year, 0, false);
          if (!__result)
            return make_pair(__first, __format);
          break;
        }

        case 'Y': {
          bool __result = __get_integer_nogroup(__first, __last, 10,
                                                __t->tm_year, 0, false);
          __t->tm_year -= 1900;
          if (!__result)
            return make_pair(__first, __format);
          break;
        }

        default:
          break;
      }

    }
    else {
      if (*__first++ != *__format) break;
    }
    
    ++__format;
  }

  return make_pair(__first, __format);
}


// __get_short_or_long_weekday and __get_short_or_long_monthname are used to
// implement the virtual functions do___get_weekday and do___get_monthname
// respectively.  They use the double lists containing both
// abbreviations and full names.
  

template <class _InIt>
pair<_InIt, bool>
__get_short_or_long_dayname(_InIt __first, _InIt __last,
                            const _Time_Info& __table, tm* __t) {
  pair<_InIt, const string*> __pr =
    __match(__first, __last,
            __table._M_dayname + 0, __table._M_dayname + 14);
  __first = __pr.first;
  __t->tm_wday = (__pr.second - __table._M_dayname) % 7;
  return make_pair(__first, __pr.second != __table._M_dayname + 14);
}

template <class _InIt>
pair<_InIt, bool>
__get_short_or_long_monthname(_InIt __first, _InIt __last,
                              const _Time_Info& __table, tm* __t) {
  pair<_InIt, const string*> __pr =
    __match(__first, __last,
            __table._M_monthname + 0, __table._M_monthname + 24);
  __first = __pr.first;
  __t->tm_mon = (__pr.second - __table._M_monthname) % 12;
  return make_pair(__first, __pr.second != __table._M_monthname + 24);
}
 

template <class _Ch, class _InIt = istreambuf_iterator<_Ch> >
class time_get : public locale::facet, public time_base 
{
  friend class _Locale_impl;
public:
  typedef _Ch   char_type;
  typedef _InIt iter_type;

  explicit time_get(size_t __refs = 0) : locale::facet(__refs) {
    __init_timeinfo(_M_timeinfo);
  }

  dateorder date_order() const { return do_date_order(); }
  iter_type get_time(iter_type __s, iter_type  __end, ios_base&  __str,
                     ios_base::iostate&  __err, tm* __t) const
    { return do_get_time(__s,  __end,  __str,  __err, __t); }
  iter_type get_date(iter_type __s, iter_type  __end, ios_base&  __str,
                     ios_base::iostate&  __err, tm* __t) const
    { return do_get_date(__s,  __end,  __str,  __err, __t); }
  iter_type get_weekday(iter_type __s, iter_type  __end, ios_base&  __str,
                        ios_base::iostate&  __err, tm* __t) const
    { return do_get_weekday(__s,  __end,  __str,  __err, __t); }
  iter_type get_monthname(iter_type __s, iter_type  __end, ios_base&  __str,
                          ios_base::iostate&  __err, tm* __t) const
    { return do_get_monthname(__s,  __end,  __str,  __err, __t); }
  iter_type get_year(iter_type __s, iter_type  __end, ios_base&  __str,
                     ios_base::iostate&  __err, tm* __t) const
    { return do_get_year(__s,  __end,  __str,  __err, __t); }

  static locale::id id;

protected:
  _Time_Info _M_timeinfo;

  time_get(_Locale_time*, size_t __refs) : locale::facet(__refs) {}
  ~time_get() {}

  virtual dateorder do_date_order() const {return no_order;}
    
  virtual iter_type do_get_time(iter_type __s, iter_type  __end,
                                ios_base& __str, ios_base::iostate&  __err,
                                tm* __t) const {
    string::const_iterator __format
      = _M_timeinfo._M_time_format.begin();
    string::const_iterator __format_end
      = _M_timeinfo._M_time_format.end();

    pair<iter_type, string::const_iterator> __result
      = __get_formatted_time(__s, __end, __format, __format_end,
                             _Ch(), _M_timeinfo, __err, __t);
    __err = __result.second == __format_end ? ios_base::goodbit 
                                            : ios_base::failbit;
    if (__result.first == __end)
      __err |= ios_base::eofbit;
    return __result.first;
  }
    
  virtual iter_type do_get_date(iter_type __s, iter_type  __end,
                                ios_base& __str, ios_base::iostate& __err,
                                tm* __t) const {
    string::const_iterator __format
      = _M_timeinfo._M_date_format.begin();
    string::const_iterator __format_end
      = _M_timeinfo._M_date_format.end();

    pair<iter_type, string::const_iterator> __result
      = __get_formatted_time(__s, __end, __format, __format_end,
                           _Ch(), _M_timeinfo, __err, __t);
    if (__result.second == __format_end)
      __err = ios_base::goodbit;
    else {
      __err = ios_base::failbit;
      if (__result.first == __end)
        __err |= ios_base::eofbit;
    }
    return __result.first;
}

  virtual iter_type do_get_weekday(iter_type __s, iter_type  __end,
                                   ios_base& __str,
                                   ios_base::iostate& __err,
                                   tm* __t) const {
    pair<iter_type, bool> __result =
      __get_short_or_long_dayname(__s, __end, _M_timeinfo, __t);
    if (__result.second)
      __err = ios_base::goodbit;
    else {
      __err = ios_base::failbit;
      if (__result.first == __end)
        __err |= ios_base::eofbit;
    }
    return __result.first;
}   


  virtual iter_type do_get_monthname(iter_type __s, iter_type  __end,
                                     ios_base& __str,
                                     ios_base::iostate& __err,
                                     tm* __t) const {
    pair<iter_type, bool> __result =
      __get_short_or_long_monthname(__s, __end, _M_timeinfo, __t);
    if (__result.second)
      __err = ios_base::goodbit;
    else {
      __err = ios_base::failbit;
      if (__result.first == __end)
        __err |= ios_base::eofbit;
    }
  return __result.first;
}   

  virtual iter_type do_get_year(iter_type __s, iter_type  __end,
                                ios_base& __str, ios_base::iostate& __err,
                                tm* __t) const {
    if (__s == __end) {
      __err = ios_base::failbit | ios_base::eofbit;
      return __s;
    }
    bool __result = 
      __get_integer_nogroup(__s, __end, 10, __t->tm_year, 0, false);
    __t->tm_year -= 1900;
    __err = __result ? ios_base::goodbit : ios_base::failbit;
    if (__s == __end)
      __err |= ios_base::eofbit;
    return __s;
  }
};

time_base::dateorder __get_date_order(_Locale_time*);

template <class _Ch, class _InIt = istreambuf_iterator<_Ch> >
class time_get_byname : public time_get<_Ch, _InIt> 
{
public:
  typedef  time_base::dateorder dateorder;
  typedef _InIt                 iter_type;

  explicit time_get_byname(const char* __name, size_t __refs = 0)
    : time_get<_Ch, _InIt>((_Locale_time*) 0, __refs),
      _M_time(__acquire_time(__name))
    { __init_timeinfo(this->_M_timeinfo, this->_M_time); }

protected:
  ~time_get_byname() { __release_time(_M_time); }
  dateorder do_date_order() const { return __get_date_order(_M_time); }
private:
  _Locale_time* _M_time;
};

// time_put facet

// For the formats 'x, 'X', and 'c', do_put calls the first form of
// put with the pattern obtained from _M_timeinfo._M_date_format or
// _M_timeinfo._M_time_format.

// Helper function:  __  takes a single-character
// format.  As indicated by the foregoing remark, this will never be
// 'x', 'X', or 'c'.

char * __write_formatted_time(char * __buf, char __format,
                              const _Time_Info& __table, const tm* __t);

template <class _OuIt>
_OuIt __put_time(char * __first, char * __last, _OuIt __out,
                 const locale& __loc, char) {
  return copy(__first, __last, __out);
}

template <class _OuIt>
_OuIt __put_time(char * __first, char * __last, _OuIt __out,
                 const locale& __loc, wchar_t) {
  const ctype<wchar_t>& __ct = use_facet<ctype<wchar_t> >(__loc);
  wchar_t __wbuf[64];
  __ct.widen(__first, __last, __wbuf);
  ptrdiff_t __len = __last - __first;
  wchar_t * __eend = __wbuf + __len;

  return copy(__wbuf, __eend, __out);
}

template<class _Ch, class _OutputIter = ostreambuf_iterator<_Ch> >
class time_put : public locale::facet 
{
  friend class _Locale_impl;
public:
  typedef _Ch      char_type;
  typedef _OutputIter iter_type;

  explicit time_put(size_t __refs = 0) : locale::facet(__refs) {
    __init_timeinfo(_M_timeinfo);
  }

  iter_type put(iter_type __s, ios_base& __f, char_type __fill,
                const tm* __tmb,
                const _Ch* __pat, const _Ch* __pat_end) const {
    locale __loc = __f.getloc();
    while (__pat != __pat_end) {
      char __c = use_facet<ctype<_Ch> >(__loc).narrow(*__pat, 0);
      if (__c == '%') {
        ++__pat;
      __c = use_facet<ctype<_Ch> >(__loc).narrow(*__pat++, 0);
      __s = do_put(__s, __f, __fill, __tmb, __c, 0);
      }
      else
        *__s++ = *__pat++;
    }
    return __s;
  }
    
  iter_type put(iter_type __s, ios_base& __f, char_type  __fill,
                const tm* __tmb, char __format, char __modifier = 0) const
    { return do_put(__s, __f,  __fill, __tmb, __format, __modifier); }

  static locale::id id;

protected:
  _Time_Info _M_timeinfo;

  time_put(_Locale_time*, size_t __refs) : locale::facet(__refs) {}
  ~time_put() {}
  virtual iter_type do_put(iter_type __s, ios_base& __f,
                           char_type  __fill, const tm* __tmb,
                           char __format, char __modifier) const {
    char __buf[64];
    char * __iend = __write_formatted_time(__buf, __format,
                                           _M_timeinfo, __tmb);
    locale __loc = __f.getloc();
    return __put_time(__buf, __iend, __s, __loc, _Ch());
  }
};

template <class _Ch, class _InIt = ostreambuf_iterator<_Ch> >
class time_put_byname : public time_put<_Ch, _InIt> 
{
  friend class _Locale_impl;
public:
  typedef time_base::dateorder dateorder;
  typedef _InIt iter_type;
  typedef _Ch   char_type;

  explicit time_put_byname(const char * __name, size_t __refs = 0)
    : time_put<_Ch, _InIt>((_Locale_time*) 0, __refs),
      _M_time(__acquire_time(__name))
    { __init_timeinfo(this->_M_timeinfo, this->_M_time); }

protected:
  ~time_put_byname() { __release_time(_M_time); }

private:
  _Locale_time* _M_time;
};

__STL_END_NAMESPACE

#endif /* __SGI_STL_INTERNAL_TIME_FACETS_H */

// Local Variables:
// mode:C++
// End:

