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


#ifndef __SGI_STL_INTERNAL_CODECVT_H
#define __SGI_STL_INTERNAL_CODECVT_H

#include <c_locale.h>
#include <stl_locale.h>

__STL_BEGIN_NAMESPACE

class codecvt_base {
public:
  enum result {ok, partial, error, noconv};
};

template <class _InternT, class _ExternT, class _StateT>
class codecvt;
 
template <class _InternT, class _ExternT, class _StateT>
class codecvt_byname;

template <>
class codecvt<char, char, mbstate_t>
  : public locale::facet, public codecvt_base 
{
  friend class _Locale_impl;
public:
  typedef char       intern_type;
  typedef char       extern_type;
  typedef mbstate_t  state_type;

  explicit codecvt(size_t __refs = 0);

  result out(mbstate_t    __state,
             const char*  __from,
             const char*  __from_end,
             const char*& __from_next,
             char*        __to,
             char*        __to_limit, 
             char*&       __to_next) const {
    return do_out(__state, 
                  __from, __from_end, __from_next,
                  __to,   __to_limit, __to_next);
  }

  result unshift(mbstate_t& __state,
                 char* __to, char* __to_limit, char*& __to_next) const
    { return do_unshift(__state, __to, __to_limit, __to_next); }
    
  result in(mbstate_t&   __state,
            const char*  __from,
            const char*  __from_end,  
            const char*& __from_next,
            char*        __to, 
            char*        __to_limit, 
            char*&       __to_next) const {
    return do_in(__state,
                 __from, __from_end, __from_next,
                 __to,   __to_limit, __to_next);
  }

  int encoding() const __STL_NOTHROW { return do_encoding(); }

  bool always_noconv() const __STL_NOTHROW { return do_always_noconv(); }

  int length(mbstate_t& __state,
             const char* __from, const char* __end,
             size_t __max) const
    { return do_length(__state, __from, __end, __max); }
  
  int max_length() const __STL_NOTHROW { return do_max_length(); }

  static locale::id id;

protected:
  ~codecvt();

  virtual result do_out(mbstate_t&   __state,
                        const char*  __from,
                        const char*  __from_end,
                        const char*& __from_next,
                        char*        __to,
                        char*        __to_limit,
                        char*&       __to_next) const
    { __from_next = __from; __to_next   = __to; return noconv; }

  virtual result do_in (mbstate_t&   __state,
                        const char*  __from,
                        const char*  __from_end,
                        const char*& __from_next,
                        char*        __to,
                        char*        __to_end,
                        char*&       __to_next) const
    { __from_next = __from; __to_next   = __to; return noconv; }

  virtual result do_unshift(mbstate_t& __state,
                            char*      __to,
                            char*      __to_limit,
                            char*&     __to_next) const
    { __to_next = __to; return noconv; }

  virtual int do_encoding() const __STL_NOTHROW { return 1; }

  virtual bool do_always_noconv() const __STL_NOTHROW { return true; }
  
  virtual int do_length(mbstate_t&         __state,
                        const  char* __from, 
                        const  char* __end,
                        size_t              __max) const;

  virtual int do_max_length() const __STL_NOTHROW { return 1; }
};
 
template <>
class codecvt<wchar_t, char, mbstate_t>
  : public locale::facet, public codecvt_base
{
  friend class _Locale_impl;
public:
  typedef wchar_t    intern_type;
  typedef char       extern_type;
  typedef mbstate_t  state_type;

  explicit codecvt(size_t __refs = 0);

  result out(mbstate_t       __state,
             const wchar_t*  __from,
             const wchar_t*  __from_end,
             const wchar_t*& __from_next,
             char*           __to,
             char*           __to_limit,
             char*&          __to_next) const {
    return do_out(__state,
                  __from, __from_end, __from_next, 
                  __to,   __to_limit, __to_next);
  }

  result unshift(mbstate_t& __state,
                 char*  __to, char*  __to_limit, char*& __to_next) const {
    return do_unshift(__state, __to, __to_limit, __to_next);
  }
    
  result in(mbstate_t    __state,
            const char*  __from,
            const char*  __from_end,  
            const char*& __from_next,
            wchar_t*     __to, 
            wchar_t*     __to_limit, 
            wchar_t*&    __to_next) const {
    return do_in(__state, 
                 __from, __from_end, __from_next,
                 __to,  __to_limit, __to_next);
  }

  int encoding() const __STL_NOTHROW { return do_encoding(); }

  bool always_noconv() const __STL_NOTHROW { return do_always_noconv(); }

  int length(mbstate_t&        __state,
             const char* __from,
             const char* __end,
             size_t             __max) const
    { return do_length(__state, __from, __end, __max); }
  
  int max_length() const __STL_NOTHROW { return do_max_length(); }

  static locale::id id;

protected:
  ~codecvt();

  virtual result do_out(mbstate_t&         __state,
                        const wchar_t*  __from,
                        const wchar_t*  __from_end,
                        const wchar_t*& __from_next,
                        char*        __to,
                        char*        __to_limit,
                        char*&       __to_next) const;

  virtual result do_in (mbstate_t&         __state,
                        const char*  __from,
                        const char*  __from_end,
                        const char*& __from_next,
                        wchar_t*        __to,
                        wchar_t*        __to_limit,
                        wchar_t*&       __to_next) const;

  virtual result do_unshift(mbstate_t&   __state,
                            char*  __to, 
                            char*  __to_limit,
                            char*& __to_next) const;

  virtual int do_encoding() const __STL_NOTHROW;

  virtual bool do_always_noconv() const __STL_NOTHROW;
  
  virtual int do_length(mbstate_t&         __state,
                        const  char* __from, 
                        const  char* __end,
                        size_t __max) const;

  virtual int do_max_length() const __STL_NOTHROW;
};

template<>
class codecvt_byname<char, char, mbstate_t>
  : public codecvt<char, char, mbstate_t> {
public:
  explicit codecvt_byname(const char* __name, size_t __refs = 0);
};

template<>
class codecvt_byname<wchar_t, char, mbstate_t>
  : public codecvt<wchar_t, char, mbstate_t> 
{
public:
  explicit codecvt_byname(const char * __name, size_t __refs = 0);    

protected:
  ~codecvt_byname();

  virtual result do_out(mbstate_t&         __state,
                        const wchar_t*  __from,
                        const wchar_t*  __from_end,
                        const wchar_t*& __from_next,
                        char*        __to,
                        char*        __to_limit,
                        char*&       __to_next) const;

  virtual result do_in (mbstate_t&         __state,
                        const char*  __from,
                        const char*  __from_end,
                        const char*& __from_next,
                        wchar_t*        __to,
                        wchar_t*        __to_limit,
                        wchar_t*&       __to_next) const;

  virtual result do_unshift(mbstate_t&   __state,
                            char*  __to, 
                            char*  __to_limit,
                            char*& __to_next) const;

  virtual int do_encoding() const __STL_NOTHROW;

  virtual bool do_always_noconv() const __STL_NOTHROW;
  
  virtual int do_length(mbstate_t&         __state,
                        const  char* __from, 
                        const  char* __end,
                        size_t __max) const;

  virtual int do_max_length() const __STL_NOTHROW;

private:
  _Locale_ctype* _M_ctype;
};


__STL_END_NAMESPACE

#endif /* __SGI_STL_INTERNAL_CODECVT_H */

// Local Variables:
// mode:C++
// End:

