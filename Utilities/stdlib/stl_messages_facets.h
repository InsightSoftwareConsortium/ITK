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


#ifndef __SGI_STL_INTERNAL_MESSAGES_H
#define __SGI_STL_INTERNAL_MESSAGES_H

__STL_BEGIN_NAMESPACE

// Forward declaration of an opaque type.
struct _Catalog_locale_map;

// messages facets

class messages_base {
public:
  typedef int catalog;
};

template <class _CharT> class messages;

template <>
class messages<char> : public locale::facet, public messages_base 
{
  friend class _Locale_impl;
public:
  typedef messages_base::catalog catalog;
  typedef char                   char_type;
  typedef basic_string<char>     string_type;


  explicit messages(size_t __refs = 0);

  inline catalog open(const string& __fn, const locale& __loc) const //JGS
    { return do_open(__fn, __loc); }
  inline string_type get(catalog __c, int __set, int __msgid,
                         const string_type& __dfault) const
    { return do_get(__c, __set, __msgid, __dfault); }
  inline void close(catalog __c) const
    { do_close(__c); }

  static locale::id id;

protected:
  messages(size_t, _Locale_messages*);
  ~messages();

  virtual catalog     do_open(const string& __fn, const locale& __loc) const;
  virtual string_type do_get(catalog __c, int __set, int __msgid,
                             const string_type& __dfault) const;
  virtual void        do_close(catalog __c) const;

  void _M_initialize(const char* __name);

private:
  _Locale_messages* _M_message_obj;
};

template <>
class messages<wchar_t> : public locale::facet, public messages_base 
{
  friend class _Locale_impl;
public:
  typedef messages_base::catalog catalog;
  typedef wchar_t                char_type;
  typedef basic_string<wchar_t>  string_type;

  explicit messages(size_t __refs = 0);

  inline catalog open(const string& __fn, const locale& __loc) const
    { return do_open(__fn, __loc); }
  inline string_type get(catalog __c, int __set, int __msgid,
                         const string_type& __dfault) const
    { return do_get(__c, __set, __msgid, __dfault); }
  inline void close(catalog __c) const
    { do_close(__c); }

  static locale::id id;

protected:
  messages(size_t, _Locale_messages*);
  ~messages();

  virtual catalog     do_open(const string& __fn, const locale& __loc) const;
  virtual string_type do_get(catalog __c, int __set, int __msgid,
                             const string_type& __dfault) const;
  virtual void        do_close(catalog __c) const;

  void _M_initialize(const char* __name);

private:
  _Locale_messages* _M_message_obj;
  _Catalog_locale_map* _M_map;
};

template <class _CharT> class messages_byname;

template <>
class messages_byname<char> : public messages<char> {
public:
  typedef messages_base::catalog catalog;
  typedef basic_string<char>     string_type;

  explicit messages_byname(const char* __name, size_t __refs = 0);

protected:
  ~messages_byname();
};

template <>
class messages_byname<wchar_t> : public messages<wchar_t> {
public:
  typedef messages_base::catalog catalog;
  typedef basic_string<wchar_t>  string_type;

  explicit messages_byname(const char* __name, size_t __refs = 0);

protected:
  ~messages_byname();
};

__STL_END_NAMESPACE

#endif /* __SGI_STL_INTERNAL_MESSAGES_H */

// Local Variables:
// mode:C++
// End:

