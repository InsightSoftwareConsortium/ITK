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

#include <string>
#include <locale>
#include <typeinfo>
#include <hash_map>

__STL_BEGIN_NAMESPACE

_Locale_messages* __acquire_messages(const char* name); 
void __release_messages(_Locale_messages* cat);

// Class _Catalog_locale_map.  The reason for this is that, internally,
// a message string is always a char*.  We need a ctype facet to convert
// a string to and from wchar_t, and the user is permitted to provide such
// a facet when calling open().

struct _Catalog_locale_map
{
  _Catalog_locale_map() : M(0) {}
  ~_Catalog_locale_map() { if (M) delete M; }

  void insert(int key, const locale& L);
  locale lookup(int key) const;
  void erase(int key);

  hash_map<int, locale>* M;

private:                        // Invalidate copy constructor and assignment
  _Catalog_locale_map(const _Catalog_locale_map&);
  void operator=(const _Catalog_locale_map&);
};

void _Catalog_locale_map::insert(int key, const locale& L)
{
  // Don't bother to do anything unless we're using a non-default ctype facet
  try {
    typedef ctype<wchar_t> wctype;
    if (typeid(use_facet<wctype>(L)) != typeid(const wctype&)) {
      if (!M)
        M = new hash_map<int, locale>;
      if (M->find(key) == M->end())
        M->insert(pair<const int, locale>(key, L));
    }
  }
  catch(...) {}
}

void _Catalog_locale_map::erase(int key)
{
  if (M)
    M->erase(key);
}

locale _Catalog_locale_map::lookup(int key) const
{
  if (M) {
    hash_map<int, locale>::iterator i = M->find(key);
    return i != M->end() ? (*i).second : locale::classic();
  }
  else
    return locale::classic();
}

//----------------------------------------------------------------------
// messages<char>

messages<char>::messages(size_t refs)
  : locale::facet(refs), _M_message_obj(0)
{
  _M_message_obj = __acquire_messages("C");
}

messages<char>::messages(size_t refs, _Locale_messages* msg_obj)
  : locale::facet(refs), _M_message_obj(msg_obj)
{}

messages<char>::~messages()
{
  __release_messages(_M_message_obj);
}

int messages<char>::do_open(const string& filename, const locale&) const
{
  return _M_message_obj
    ? _Locale_catopen(_M_message_obj, filename.c_str())
    : -1;
}

string messages<char>::do_get(catalog cat,
                              int set, int id, const string& dfault) const
{
  return _M_message_obj != 0 && cat >= 0
    ? string(_Locale_catgets(_M_message_obj, cat, set, id, dfault.c_str()))
    : dfault;
}

void messages<char>::do_close(catalog cat) const
{
  if (_M_message_obj)
    _Locale_catclose(_M_message_obj, cat);
}

//----------------------------------------------------------------------
// messages<wchar_t>

messages<wchar_t>::messages(size_t refs)
  : locale::facet(refs),
    _M_message_obj(0), _M_map(new _Catalog_locale_map)
{
  _M_message_obj = __acquire_messages("C");
}

messages<wchar_t>::messages(size_t refs, _Locale_messages* msg_obj)
  : locale::facet(refs),
    _M_message_obj(msg_obj), _M_map(new _Catalog_locale_map)
{}

messages<wchar_t>::~messages()
{
  __release_messages(_M_message_obj);
  delete _M_map;
}

int messages<wchar_t>::do_open(const string& filename, const locale& L) const
{
  int result = _M_message_obj
    ? _Locale_catopen(_M_message_obj, filename.c_str())
    : -1;

  if (result >= 0)
    _M_map->insert(result, L);

  return result;
}

wstring
messages<wchar_t>::do_get(catalog cat,
                          int set, int id, const wstring& dfault) const
{
  typedef ctype<wchar_t> wctype;
  const wctype& ct = use_facet<wctype>(_M_map->lookup(cat));

  const char* str = _Locale_catgets(_M_message_obj, cat, set, id, "");

  // Verify that the lookup failed; an empty string might represent success.
  if (!str)
    return dfault;
  else if (str[0] == '\0') {
    const char* str2 = _Locale_catgets(_M_message_obj, cat, set, id, "*");
    if (!str2 || strcmp(str2, "*") == 0)
      return dfault;
  }

  // str is correct.  Now we must widen it to get a wstring.
  size_t n = strlen(str);

  // NOT PORTABLE.  What we're doing relies on internal details of the 
  // string implementation.  (Contiguity of string elements.)
  wstring result(n, wchar_t(0));
  ct.widen(str, str + n, &*result.begin());
  return result;
}

void messages<wchar_t>::do_close(catalog cat) const
{
  if (_M_message_obj)
    _Locale_catclose(_M_message_obj, cat);
  _M_map->erase(cat);
}

//----------------------------------------------------------------------
// messages_byname<char>

messages_byname<char>::messages_byname(const char* name, size_t refs)
  : messages<char>(refs, name ? __acquire_messages(name) : 0)
{}

messages_byname<char>::~messages_byname()
{}

//----------------------------------------------------------------------
// messages_byname<wchar_t>

messages_byname<wchar_t>::messages_byname(const char* name, size_t refs)
  : messages<wchar_t>(refs, name ? __acquire_messages(name) : 0)
{}

messages_byname<wchar_t>::~messages_byname()
{}


__STL_END_NAMESPACE

// Local Variables:
// mode:C++
// End:
