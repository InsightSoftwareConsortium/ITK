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

#ifndef __SGI_STL_INTERNAL_COLLATE_H
#define __SGI_STL_INTERNAL_COLLATE_H

#include <c_locale.h>
#include <stl_string_fwd.h>
#include <stl_locale.h>

__STL_BEGIN_NAMESPACE


template <class _CharT> class collate;
template <class _CharT> class collate_byname;

template <>
class collate<char> : public locale::facet 
{
  friend class _Locale_impl;
public:
  typedef char               char_type;
  typedef basic_string<char> string_type;

  explicit collate(size_t __refs = 0);

  int compare(const char_type* __low1, const char_type* __high1,
              const char_type* __low2, const char_type* __high2) const {
    return do_compare( __low1, __high1, __low2, __high2);
  }

  string_type transform(const char_type* __low, const char_type* __high) const
    { return do_transform(__low, __high); }

  long hash(const char_type* __low, const char_type* __high) const
    { return do_hash(__low, __high); }

  static locale::id id;

protected:
  ~collate();

  virtual int do_compare(const char_type*, const char_type*,
                         const char_type*, const char_type*) const;
  virtual string_type do_transform(const char*, const char*) const;
  virtual long do_hash(const char_type*, const char_type*) const;
};

template <>
class collate<wchar_t> : public locale::facet 
{
  friend class _Locale_impl;
public:
  typedef wchar_t               char_type;
  typedef basic_string<wchar_t> string_type;

  explicit collate(size_t __refs = 0);

  int compare(const char_type* __low1, const char_type* __high1,
              const char_type* __low2, const char_type* __high2) const {
    return do_compare( __low1, __high1, __low2, __high2);
  }

  string_type transform(const char_type* __low, const char_type* __high) const
    { return do_transform(__low, __high); }

  long hash(const char_type* __low, const char_type* __high) const
    { return do_hash(__low, __high); }

  static locale::id id;

protected:
  ~collate();

  virtual int do_compare(const char_type*, const char_type*,
                         const char_type*, const char_type*) const;
  virtual string_type do_transform(const char_type*, const char_type*) const;
  virtual long do_hash(const char_type* __low, const char_type* __high) const;
};

template<>
class collate_byname<char>: public collate<char> 
{
public:
  explicit collate_byname(const char* __name, size_t __refs = 0);

protected:
  ~collate_byname();

  virtual int do_compare(const char_type*, const char_type*,
                         const char_type*, const char_type*) const;
  virtual string_type do_transform(const char*, const char*) const;

private:
  _Locale_collate* _M_collate;
};

template<>
class collate_byname<wchar_t>: public collate<wchar_t> 
{
public:
  explicit collate_byname(const char * __name, size_t __refs = 0);

protected:
  ~collate_byname();

  virtual int do_compare(const char_type*, const char_type*,
                         const char_type*, const char_type*) const;
  virtual string_type do_transform(const char_type*, const char_type*) const;

private:
  _Locale_collate* _M_collate;
};


__STL_END_NAMESPACE

#endif /* __SGI_STL_INTERNAL_COLLATE_H */

// Local Variables:
// mode:C++
// End:
