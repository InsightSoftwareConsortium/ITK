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


#ifndef __SGI_STL_INTERNAL_LOCALE_H
#define __SGI_STL_INTERNAL_LOCALE_H

#include <typeinfo>
#include <stdlib.h>
#include <stl_threads.h>
#include <string>

__STL_BEGIN_NAMESPACE

class _Locale_impl;             // Forward declaration of opaque type.

class locale {
  friend class _Locale_impl;
  friend class ios_base;
public:
  // types:
  class facet;
  class id;
  typedef int category;
#ifdef __STL_STATIC_CONST_INIT_BUG
  enum { none      = 0x000,
    collate   = 0x010,
    ctype     = 0x020,
    monetary  = 0x040,
    numeric   = 0x100,
    time      = 0x200,
    messages  = 0x400,
    all       = collate | ctype | monetary | numeric | time | messages };
#else
  static const category
    none      = 0x000,
    collate   = 0x010,
    ctype     = 0x020,
    monetary  = 0x040,
    numeric   = 0x100,
    time      = 0x200,
    messages  = 0x400,
    all       = collate | ctype | monetary | numeric | time | messages;
#endif

  // construct/copy/destroy:
  locale();
  locale(const locale&) __STL_NOTHROW;
  explicit locale(const char *);
  locale(const locale&, const char*, category);
  template <class _Facet> locale(const locale&, _Facet*);
  locale(const locale&, const locale&, category);
  ~locale() __STL_NOTHROW;
  const locale& operator=(const locale&) __STL_NOTHROW;
  template <class _Facet> locale combine(const locale&);

  // locale operations:
  basic_string<char> name() const;

  bool operator==(const locale&) const;
  bool operator!=(const locale&) const;

  template <class _CharT, class _Traits, class _Alloc>
  bool operator()(const basic_string<_CharT, _Traits, _Alloc>& __x,
                  const basic_string<_CharT, _Traits, _Alloc>& __y) const;

  // global locale objects:
  static locale global(const locale&);
  static const locale& classic();

public:                         // Helper functions for locale globals.
  facet* _M_get_facet(const id&) const;
  static void _M_throw_runtime_error(const char* = 0);

private:                        // More helper functions.
  static void _S_initialize();
  static void _S_uninitialize();
  static _Locale_impl* _S_copy_impl(_Locale_impl*, bool);
  void _M_insert(facet* __f, id& __id);
  locale(_Locale_impl*, bool);

private:                        // Data members
  _Locale_impl* _M_impl;        
};

//----------------------------------------------------------------------
// locale nested classes

class locale::facet : private _Refcount_Base {
  friend class locale;
  friend class _Locale_impl;
protected:
  explicit facet(size_t = 0);
  virtual ~facet();

private:                        // Invalidate assignment and copying.
  facet(const facet&);          
  void operator=(const facet&); 

private:                        // Data members.
  const bool _M_delete;
};

class locale::id {
  friend class locale;
  friend class _Locale_impl;
public:
  size_t _M_index;
  static size_t _S_max;
};

//----------------------------------------------------------------------
// Template members

// Lets the locale act as a comparison function object for strings.
// It may or may not be a strict weak ordering, depending on how the
// collate facet is defined.
template <class _CharT, class _Tr, class _Al>
bool locale::operator()(const basic_string<_CharT, _Tr, _Al>& __x,
                        const basic_string<_CharT, _Tr, _Al>& __y) const {
  const std::collate<_CharT>& __col = use_facet<std::collate<_CharT> >(*this);
  return __col.compare(__x.data(), __x.data() + __x.size(),
                       __y.data(), __y.data() + __y.size());
}

// Make a copy of locale __loc, except use __f for _Facet instead of the 
// corresponding facet from __loc.
template <class _Facet>
locale::locale(const locale& __loc, _Facet* __f)
  : _M_impl(0)
{
  _M_impl = this->_S_copy_impl(__loc._M_impl, __f != 0);
  if (__f != 0)
    this->_M_insert(__f, _Facet::id);
}

// A sort of generalized constructor.  Creates a copy of the 
// current locale, except taking _Facet from the other locale __loc.
template <class _Facet>
locale locale::combine(const locale& __loc) 
{
  locale __result(__loc._M_impl, true);
  if (facet* __f = __loc._M_get_facet(_Facet::id)) {
    __result._M_insert(__f, _Facet::id);
    __f->_M_incr();
  }
  else
    _M_throw_runtime_error();

  return __result;
}

//----------------------------------------------------------------------
// locale globals

template <class _Facet> inline const _Facet& use_facet(const locale& __loc)
{
  const locale::facet* __f = __loc._M_get_facet(_Facet::id);
  if (!__f) {
    __STL_THROW(bad_cast());
    abort();
  }
  return static_cast<const _Facet&>(*__f);
}

template <class _Facet> 
inline bool has_facet(const locale& __loc) __STL_NOTHROW 
{
  return __loc._M_get_facet(_Facet::id) != 0;
}

__STL_END_NAMESPACE

#endif /* __SGI_STL_INTERNAL_LOCALE_H */

// Local Variables:
// mode:C++
// End:

