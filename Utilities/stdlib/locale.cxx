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

#include <vector>
#include <hash_map>
#include <locale>
#include <locale.h>             // C locale header file.
#include <stl_threads.h>

__STL_BEGIN_NAMESPACE

//----------------------------------------------------------------------
// Base class locale::facet

locale::facet::facet(size_t no_del)
  : _Refcount_Base(1), 
    _M_delete(no_del == 0)
{}

locale::facet::~facet()
{}
  
//----------------------------------------------------------------------
// struct locale::id

size_t locale::id::_S_max;
 
//----------------------------------------------------------------------
// Class _Locale_impl

class _Locale_impl : public _Refcount_Base
{
public:
  _Locale_impl(size_t n, const char* s);
  _Locale_impl(const _Locale_impl&);
  ~_Locale_impl();

  void remove(size_t index);
  locale::facet* insert(locale::facet*, size_t index, bool do_incr);
  void insert(_Locale_impl* from, const locale::id& n);

  template <class Facet>
  inline locale::facet* insert(Facet* f)
  {
    return this->insert(f, Facet::id._M_index, false);
  }

  // Helper functions for byname construction of locales.
  void insert_ctype_facets(const char* name);
  void insert_numeric_facets(const char* name);
  void insert_time_facets(const char* name);
  void insert_collate_facets(const char* name);
  void insert_monetary_facets(const char* name);
  void insert_messages_facets(const char* name);

  vector<locale::facet*> facets;
  string name;

private:
  void operator=(const _Locale_impl&);
};

// _Locale_impl non-inline member functions.

_Locale_impl::_Locale_impl(size_t n, const char* s)
  : _Refcount_Base(1),
    facets(n, (locale::facet*) 0),
    name(s)
{}

_Locale_impl::_Locale_impl(const _Locale_impl& L)
  : _Refcount_Base(1),
    facets(L.facets),
    name(L.name)
{
  for (size_t i = 1; i < L.facets.size(); ++i) {
    if (L.facets[i] && L.facets[i]->_M_delete)
      L.facets[i]->_M_incr();
  }
}

_Locale_impl::~_Locale_impl() {
  for (size_t i = 1; i < facets.size(); ++i)
    remove(i);
}

void _Locale_impl::remove(size_t index) {
  if (index > 0 && index < facets.size()) {
    locale::facet* old = facets[index];
    if (old && old->_M_delete)
      if (old->_M_decr() == 0)
        delete old;
    facets[index] = 0;
  }
}

locale::facet*
_Locale_impl::insert(locale::facet* f, size_t index, bool do_incr) {
  if (f != 0 && index != 0) {
    if (index >= facets.size())
      facets.insert(facets.end(),
                    index - facets.size() + 1, (locale::facet*) 0);
    if (do_incr)
      f->_M_incr();

    remove(index);
    facets[index] = f;
    return f;
  }
  else
    return 0;
}

void _Locale_impl::insert(_Locale_impl* from, const locale::id& n) {
  size_t index = n._M_index;
  this->remove(index);
  if (index > 0 && index < from->facets.size())
    this->insert(from->facets[index], index, true);
}

namespace {

// Global variables used in various parts of the locale subsystem
_Locale_impl*   global_impl    = 0;
locale*         classic_locale = 0;
_STL_mutex_lock global_locale_lock __STL_MUTEX_INITIALIZER;

} // Close unnamed namespace

// Six functions, one for each category.  Each of them takes a 
// _Locale_impl* and a name, constructs that appropriate category
// facets by name, and inserts them into the locale.  

void _Locale_impl::insert_ctype_facets(const char* name)
{
  ctype<char>*    ct                      = 0;
  ctype<wchar_t>* wct                     = 0;
  codecvt<char, char, mbstate_t>*    cvt  = 0;
  codecvt<wchar_t, char, mbstate_t>* wcvt = 0;

  char buf[_Locale_MAX_SIMPLE_NAME];
  if (name == 0 || name[0] == 0)
    name = _Locale_ctype_default(buf);

  if (name == 0 || name[0] == 0 || strcmp(name, "C") == 0) {
    this->insert(classic_locale->_M_impl, ctype<char>::id);
    this->insert(classic_locale->_M_impl, ctype<wchar_t>::id);
    this->insert(classic_locale->_M_impl,
                 codecvt<char, char, mbstate_t>::id);
    this->insert(classic_locale->_M_impl,
                 codecvt<wchar_t, char, mbstate_t>::id);
  }
  else {
    __STL_TRY {
      ct   = new ctype_byname<char>(name);
      wct  = new ctype_byname<wchar_t>(name); 
      cvt  = new codecvt_byname<char, char, mbstate_t>(name);
      wcvt = new codecvt_byname<wchar_t, char, mbstate_t>(name);
    }
    __STL_UNWIND(delete ct; delete wct; delete cvt; delete wcvt);

    this->insert(ct);
    this->insert(wct);
    this->insert(cvt);
    this->insert(wcvt);
  }
}

void _Locale_impl::insert_numeric_facets(const char* name)
{
  numpunct<char>*    punct  = 0;
  numpunct<wchar_t>* wpunct = 0;
  num_get<char>*     get    = 0;
  num_get<wchar_t>*  wget   = 0;
  num_put<char>*     put    = 0;
  num_put<wchar_t>*  wput   = 0;

  char buf[_Locale_MAX_SIMPLE_NAME];
  if (name == 0 || name[0] == 0)
    name = _Locale_numeric_default(buf);

  if (name == 0 || name[0] == 0 || strcmp(name, "C") == 0) {
    this->insert(classic_locale->_M_impl, numpunct<char>::id);
    this->insert(classic_locale->_M_impl, numpunct<wchar_t>::id);
    this->insert(classic_locale->_M_impl, num_get<char>::id);
    this->insert(classic_locale->_M_impl, num_get<wchar_t>::id);
    this->insert(classic_locale->_M_impl, num_put<char>::id);
    this->insert(classic_locale->_M_impl, num_put<wchar_t>::id);
  }
  else {
    __STL_TRY {
      punct  = new numpunct_byname<char>(name);
      wpunct = new numpunct_byname<wchar_t>(name);
      get    = new num_get<char>;
      wget   = new num_get<wchar_t>;
      put    = new num_put<char>;
      wput   = new num_put<wchar_t>;
    }
    __STL_UNWIND(delete punct; delete wpunct; delete get; delete wget;
                 delete put; delete wput);

    this->insert(punct);
    this->insert(wpunct);
    this->insert(get);
    this->insert(wget);
    this->insert(put);
    this->insert(wput);
  }
}

void _Locale_impl::insert_time_facets(const char* name)
{
  time_get<char>*    get  = 0;
  time_get<wchar_t>* wget = 0;
  time_put<char>*    put  = 0;
  time_put<wchar_t>* wput = 0;

  char buf[_Locale_MAX_SIMPLE_NAME];
  if (name == 0 || name[0] == 0)
    name = _Locale_time_default(buf);

  if (name == 0 || name[0] == 0 || strcmp(name, "C") == 0) {
    this->insert(classic_locale->_M_impl, time_get<char>::id);
    this->insert(classic_locale->_M_impl, time_get<wchar_t>::id);
    this->insert(classic_locale->_M_impl, time_put<char>::id);
    this->insert(classic_locale->_M_impl, time_put<wchar_t>::id);
  }
  else {
    __STL_TRY {
      get  = new time_get_byname<char>(name);
      wget = new time_get_byname<wchar_t>(name);
      put  = new time_put_byname<char>(name);
      wput = new time_put_byname<wchar_t>(name);
    }
    __STL_UNWIND(delete get; delete wget; delete put; delete wput);
    this->insert(get);
    this->insert(wget);
    this->insert(put);
    this->insert(wput);
  }
}

void _Locale_impl::insert_collate_facets(const char* name)
{
  collate<char>*    col  = 0;
  collate<wchar_t>* wcol = 0;

  char buf[_Locale_MAX_SIMPLE_NAME];
  if (name == 0 || name[0] == 0)
    name = _Locale_collate_default(buf);

  if (name == 0 || name[0] == 0 || strcmp(name, "C") == 0) {
    this->insert(classic_locale->_M_impl, collate<char>::id);
    this->insert(classic_locale->_M_impl, collate<wchar_t>::id);
  }
  else {
    __STL_TRY {
      col   = new collate_byname<char>(name);
      wcol  = new collate_byname<wchar_t>(name); 
    }
    __STL_UNWIND(delete col; delete wcol);

    this->insert(col);
    this->insert(wcol);
  }
}

void _Locale_impl::insert_monetary_facets(const char* name)
{
  moneypunct<char,    false>* punct   = 0;
  moneypunct<char,    true>*  ipunct  = 0;
  moneypunct<wchar_t, false>* wpunct  = 0;
  moneypunct<wchar_t, true>*  wipunct = 0;
  money_get<char>*            get     = 0;
  money_get<wchar_t>*         wget    = 0;
  money_put<char>*            put     = 0;
  money_put<wchar_t>*         wput    = 0;

  char buf[_Locale_MAX_SIMPLE_NAME];
  if (name == 0 || name[0] == 0)
    name = _Locale_monetary_default(buf);

  if (name == 0 || name[0] == 0 || strcmp(name, "C") == 0) {
    this->insert(classic_locale->_M_impl, moneypunct<char, false>::id);
    this->insert(classic_locale->_M_impl, moneypunct<char, true>::id);
    this->insert(classic_locale->_M_impl, moneypunct<wchar_t, false>::id);
    this->insert(classic_locale->_M_impl, moneypunct<wchar_t, true>::id);
    this->insert(classic_locale->_M_impl, money_get<char>::id);
    this->insert(classic_locale->_M_impl, money_get<wchar_t>::id);
    this->insert(classic_locale->_M_impl, money_put<char>::id);
    this->insert(classic_locale->_M_impl, money_put<wchar_t>::id);
  }
  else {
    __STL_TRY {
      punct   = new moneypunct_byname<char, false>(name);
      ipunct  = new moneypunct_byname<char, true>(name);
      wpunct  = new moneypunct_byname<wchar_t, false>(name);
      wipunct = new moneypunct_byname<wchar_t, true>(name);
      get     = new money_get<char>;
      wget    = new money_get<wchar_t>;
      put     = new money_put<char>;
      wput    = new money_put<wchar_t>;
    }
    __STL_UNWIND(delete punct; delete ipunct; delete wpunct; delete wipunct;
                 delete get; delete wget; delete put; delete wput);
    this->insert(punct);
    this->insert(ipunct);
    this->insert(wpunct);
    this->insert(wipunct);
    this->insert(get);
    this->insert(wget);
    this->insert(put);
    this->insert(wput);
  }
}

void _Locale_impl::insert_messages_facets(const char* name)
{
  messages<char>*    msg  = 0;
  messages<wchar_t>* wmsg = 0;

  char buf[_Locale_MAX_SIMPLE_NAME];
  if (name == 0 || name[0] == 0)
    name = _Locale_messages_default(buf);

  if (name == 0 || name[0] == 0 || strcmp(name, "C") == 0) {
    this->insert(classic_locale->_M_impl, messages<char>::id);
    this->insert(classic_locale->_M_impl, messages<wchar_t>::id);
  }
  else {
    __STL_TRY {
      msg  = new messages_byname<char>(name);
      wmsg = new messages_byname<wchar_t>(name);
    }
    __STL_UNWIND(delete msg; delete wmsg);
    this->insert(msg);
    this->insert(wmsg);
  }
}

namespace {

// Takes a reference to a locale::id, and returns its numeric index.
// If no numeric index has yet been assigned, assigns one.  The return
// value is always positive.
size_t get_index(locale::id& id)
{
  if (id._M_index == 0) {
    static _STL_mutex_lock L __STL_MUTEX_INITIALIZER;
    _STL_auto_lock sentry(L);
    size_t new_index = locale::id::_S_max++;
    id._M_index = new_index;
  }
  return id._M_index;
}

void assign_ids() {
  // This assigns ids to every facet that is a member of a category,
  // and also to money_get/put, num_get/put, and time_get/put
  // instantiated using ordinary pointers as the input/output
  // iterators.  (The default is [io]streambuf_iterator.)

  collate<char>::id._M_index                       = 1;
  collate<wchar_t>::id._M_index                    = 2;
  ctype<char>::id._M_index                         = 3;
  ctype<wchar_t>::id._M_index                      = 4;
  codecvt<char, char, mbstate_t>::id._M_index      = 5;
  codecvt<wchar_t, char, mbstate_t>::id._M_index   = 6;
  moneypunct<char, true>::id._M_index              = 7;
  moneypunct<char, false>::id._M_index             = 8;
  moneypunct<wchar_t, true>::id._M_index           = 9;
  moneypunct<wchar_t, false>::id._M_index          = 10;
  money_get<char>::id._M_index                     = 11;
  money_get<wchar_t>::id._M_index                  = 12;
  money_get<char, const char*>::id._M_index        = 13;
  money_get<wchar_t, const wchar_t*>::id._M_index  = 14;
  money_put<char>::id._M_index                     = 15;
  money_put<wchar_t>::id._M_index                  = 16;
  money_put<char, char*>::id._M_index              = 17;
  money_put<wchar_t, wchar_t*>::id._M_index        = 18;
  numpunct<char>::id._M_index                      = 19;
  numpunct<wchar_t>::id._M_index                   = 20;
  num_get<char>::id._M_index                       = 21;
  num_get<wchar_t>::id._M_index                    = 22;
  num_get<char, const char*>::id._M_index          = 23;
  num_get<wchar_t, const wchar_t*>::id._M_index    = 24;
  num_put<char>::id._M_index                       = 25;
  num_put<wchar_t>::id._M_index                    = 26;
  num_put<char, char*>::id._M_index                = 27;
  num_put<wchar_t, wchar_t*>::id._M_index          = 28;
  time_get<char>::id._M_index                      = 29;
  time_get<wchar_t>::id._M_index                   = 30;
  time_get<char, const char*>::id._M_index         = 31;
  time_get<wchar_t, const wchar_t*>::id._M_index   = 32;
  time_put<char>::id._M_index                      = 33;
  time_put<wchar_t>::id._M_index                   = 34;
  time_put<char, char*>::id._M_index               = 35;
  time_put<wchar_t, wchar_t*>::id._M_index         = 36;
  messages<char>::id._M_index                      = 37;
  messages<wchar_t>::id._M_index                   = 38;

  locale::id::_S_max                               = 39;
}

_Locale_impl* make_classic_locale() {
  // The classic locale contains every facet that belongs to a category.
  _Locale_impl* classic = new _Locale_impl(locale::id::_S_max, "C");

  // collate category
  classic->insert(new collate<char>(0));
  classic->insert(new collate<wchar_t>(0));

  // ctype category
  classic->insert(new ctype<char>(0, false, 0));
  classic->insert(new ctype<wchar_t>(0));
  classic->insert(new codecvt<char, char, mbstate_t>(0));
  classic->insert(new codecvt<wchar_t, char, mbstate_t>(0));

  // monetary category
  classic->insert(new moneypunct<char, true>(0));
  classic->insert(new moneypunct<char, false>(0));
  classic->insert(new moneypunct<wchar_t, true>(0));
  classic->insert(new moneypunct<wchar_t, false>(0));
  classic->insert(new money_get<char>(0));
  classic->insert(new money_get<wchar_t>(0));
  classic->insert(new money_put<char>(0));
  classic->insert(new money_put<wchar_t>(0));

  // numeric category
  classic->insert(new numpunct<char>(0));
  classic->insert(new numpunct<wchar_t>(0));
  classic->insert(new num_get<char>(0));
  classic->insert(new num_get<wchar_t>(0));
  classic->insert(new num_put<char>(0));
  classic->insert(new num_put<wchar_t>(0));

  // time category
  classic->insert(new time_get<char>(0));
  classic->insert(new time_get<wchar_t>(0));
  classic->insert(new time_put<char>(0));
  classic->insert(new time_put<wchar_t>(0));
  
  // messages category
  classic->insert(new messages<char>(0));
  classic->insert(new messages<wchar_t>(0));

  return classic;
};

// Give L a name where all facets except those in category c
// are taken from name1, and those in category c are taken from name2.
void combine_names(_Locale_impl* L,
                   const char* name1, const char* name2,
                   locale::category c)
{
  if ((c & locale::all) == 0 || strcmp(name1, name2) == 0)
    L->name = name1;
  else if ((c & locale::all) == locale::all)
    L->name = name2;
  else {
    // Decompose the names.
    char ctype_buf[_Locale_MAX_SIMPLE_NAME];
    char numeric_buf[_Locale_MAX_SIMPLE_NAME];
    char time_buf[_Locale_MAX_SIMPLE_NAME];
    char collate_buf[_Locale_MAX_SIMPLE_NAME];
    char monetary_buf[_Locale_MAX_SIMPLE_NAME];
    char messages_buf[_Locale_MAX_SIMPLE_NAME];

    _Locale_extract_ctype_name((c & locale::ctype) ? name1 : name2,
                               ctype_buf); 
    _Locale_extract_numeric_name((c & locale::numeric) ? name1 : name2,
                                 numeric_buf); 
    _Locale_extract_time_name((c & locale::time) ? name1 : name2,
                              time_buf); 
    _Locale_extract_collate_name((c & locale::collate) ? name1 : name2,
                                 collate_buf); 
    _Locale_extract_monetary_name((c & locale::monetary) ? name1 : name2,
                                  monetary_buf); 
    _Locale_extract_messages_name((c & locale::messages) ? name1 : name2,
                                  messages_buf); 

    // Construct a new composite name.
    char composite_buf[_Locale_MAX_COMPOSITE_NAME];
    _Locale_compose_name(composite_buf,
                         ctype_buf, numeric_buf, time_buf,
                         collate_buf, monetary_buf, messages_buf);
    L->name = composite_buf;
  }
}
  
} // Close unnamed namespace.



//----------------------------------------------------------------------
// class locale

// Helper function: return a copy of I.  If do_clone is true
// we create a new _Locale_impl object, otherwise we just adjust
// the reference counts.
_Locale_impl* locale::_S_copy_impl(_Locale_impl* I, bool do_clone)
{
  if (do_clone) {
    _Locale_impl* result = new _Locale_impl(*I);
    result->name = "*";
    return result;
  }
  else {
    I->_M_incr();
    return I;
  }
}

void locale::_M_insert(facet* f, locale::id& n)
{
  if (f)
    _M_impl->insert(f, get_index(n), false);
}

void locale::_M_throw_runtime_error(const char* name)
{
  const int N = 256;          // size of buffer in __Named_exception.
  char buf[N];

  if (name) {
    const char* prefix = "bad locale name: ";
    strcpy(buf, prefix);
    strncat(buf, name, N - strlen(prefix));
    buf[N-1] = '\0';
  }
  else {
    strcpy(buf, "locale error");
  }
  __STL_THROW(runtime_error(buf));
}

// Initialization of the locale system.  This must be called before
// any locales are constructed.  (Meaning that it must be called when
// the I/O library itself is initialized.)
void locale::_S_initialize()
{
  assign_ids();
  _Locale_impl* classic_impl = make_classic_locale();
  global_impl = classic_impl;
  classic_locale = new locale(classic_impl, false);

  // Postcondition: number of references to classic_impl is 2.
}

void locale::_S_uninitialize()
{
  if (global_impl->_M_decr() == 0)
    delete global_impl;
  delete classic_locale;

  global_impl = 0;
  classic_locale = 0;
}

// Default constructor: create a copy of the global locale.
locale::locale()
  : _M_impl(0)
{
  _M_impl = _S_copy_impl(global_impl, false);
}

// Copy constructor
locale::locale(const locale& L) __STL_NOTHROW
  : _M_impl(0)
{
  _M_impl = _S_copy_impl(L._M_impl, false);
}

// Make a locale directly from a _Locale_impl object.  If the second argument
// is true, we clone the _Locale_impl.  If false, we just twiddle pointers.
locale::locale(_Locale_impl* impl, bool do_copy)
  : _M_impl(0)
{
  _M_impl = _S_copy_impl(impl, do_copy);
}

// Create a locale from a name.
locale::locale(const char* name)
  : _M_impl(0)
{
  if (!name)
    _M_throw_runtime_error(0);

  __STL_TRY {
    _M_impl = new _Locale_impl(locale::id::_S_max, name);

    // Insert categories one at a time.
    _M_impl->insert_ctype_facets(name);
    _M_impl->insert_numeric_facets(name);
    _M_impl->insert_time_facets(name);
    _M_impl->insert_collate_facets(name);
    _M_impl->insert_monetary_facets(name);
    _M_impl->insert_messages_facets(name);
  }
  __STL_UNWIND(delete _M_impl);
}

// Contruct a new locale where all facets that aren't in category c
// come from L1, and all those that are in category c come from L2.
locale::locale(const locale& L1, const locale& L2, category c)
  : _M_impl(0)
{
  _M_impl = new _Locale_impl(*L1._M_impl);
  _Locale_impl* i2 = L2._M_impl;

  static string nameless("*");
  if (L1.name() != nameless && L2.name() != nameless)
    combine_names(_M_impl,
                  L1._M_impl->name.c_str(), L2._M_impl->name.c_str(),
                  c);
  else {
    _M_impl->name = "*";
  }

  if (c & collate) {
    _M_impl->insert(i2, ::std::collate<char>::id);
    _M_impl->insert(i2, ::std::collate<wchar_t>::id);
  }
  if (c & ctype) {
    _M_impl->insert(i2, ::std::ctype<char>::id);
    _M_impl->insert(i2, ::std::ctype<wchar_t>::id);
    _M_impl->insert(i2, ::std::codecvt<char, char, mbstate_t>::id);
    _M_impl->insert(i2, ::std::codecvt<wchar_t, char, mbstate_t>::id);
  }
  if (c & monetary) {
    _M_impl->insert(i2, ::std::moneypunct<char, true>::id);
    _M_impl->insert(i2, ::std::moneypunct<char, false>::id);
    _M_impl->insert(i2, ::std::moneypunct<wchar_t, true>::id);
    _M_impl->insert(i2, ::std::moneypunct<wchar_t, false>::id);
    _M_impl->insert(i2, ::std::money_get<char>::id);
    _M_impl->insert(i2, ::std::money_get<wchar_t>::id);
    _M_impl->insert(i2, ::std::money_put<char>::id);
    _M_impl->insert(i2, ::std::money_put<wchar_t>::id);
  }
  if (c & numeric) {
    _M_impl->insert(i2, ::std::numpunct<wchar_t>::id);
    _M_impl->insert(i2, ::std::numpunct<char>::id);
    _M_impl->insert(i2, ::std::num_get<char>::id);
    _M_impl->insert(i2, ::std::num_get<wchar_t>::id);
    _M_impl->insert(i2, ::std::num_put<char>::id);
    _M_impl->insert(i2, ::std::num_put<wchar_t>::id);
  }
  if (c & time) {
    _M_impl->insert(i2, ::std::time_get<char>::id);
    _M_impl->insert(i2, ::std::time_get<wchar_t>::id);
    _M_impl->insert(i2, ::std::time_put<char>::id);
    _M_impl->insert(i2, ::std::time_put<wchar_t>::id);
  }
  if (c & messages) {
    _M_impl->insert(i2, ::std::messages<char>::id);
    _M_impl->insert(i2, ::std::messages<wchar_t>::id);
  }
}

// Create a locale that's a copy of L, except that all of the facets
// in category c are instead constructed by name.
locale::locale(const locale& L, const char* name, locale::category c)
  : _M_impl(0)
{
  if (name == 0 || strcmp(name, "*") == 0)
    _M_throw_runtime_error(name);

  __STL_TRY {
    _M_impl = new _Locale_impl(*L._M_impl);
    combine_names(_M_impl, L._M_impl->name.c_str(), name, c);

    if (c & locale::ctype)
      _M_impl->insert_ctype_facets(name);
    if (c & locale::numeric)
      _M_impl->insert_numeric_facets(name);
    if (c & locale::time)
      _M_impl->insert_time_facets(name);
    if (c & locale::collate)
      _M_impl->insert_collate_facets(name);
    if (c & locale::monetary)
      _M_impl->insert_monetary_facets(name);
    if (c & locale::messages)
      _M_impl->insert_messages_facets(name);
  }
  __STL_UNWIND(delete _M_impl)
}

// Destructor.
locale::~locale() __STL_NOTHROW
{
  if (_M_impl->_M_decr() == 0)
    delete _M_impl;
}

// Assignment operator.  Much like the copy constructor: just a bit of
// pointer twiddling.
const locale& locale::operator=(const locale& L) __STL_NOTHROW
{
  if (this->_M_impl != L._M_impl) {
    if (this->_M_impl->_M_decr() == 0)
      delete this->_M_impl;
    this->_M_impl = _S_copy_impl(L._M_impl, false);
  }
  return *this;
}

locale::facet* locale::_M_get_facet(const locale::id& n) const
{
  return n._M_index < _M_impl->facets.size()
    ? _M_impl->facets[n._M_index]
    : (locale::facet*) 0;
}

string locale::name() const {
  return _M_impl->name;
}


// Compare two locales for equality.
bool locale::operator==(const locale& L) const {
  static string nameless("*");

  return this->_M_impl == L._M_impl ||
         (this->name() == L.name() && this->name() != nameless);
}

bool locale::operator!=(const locale& L) const {
  return !(*this == L);
}

// Static member functions.
const locale& locale::classic() {
  return *classic_locale;
}

locale locale::global(const locale& L) 
{
  locale old;                   // A copy of the old global locale.

  L._M_impl->_M_incr();
  {
    _STL_auto_lock sentry(global_locale_lock);
    global_impl->_M_decr();     // We made a copy, so it can't be zero.
    global_impl = L._M_impl;
  }

                                // Set the global C locale, if appropriate.
  static string nameless("*");
  if (L.name() != nameless)
    setlocale(LC_ALL, L.name().c_str());

  return old;
}

// static data members.
#ifndef __STL_STATIC_CONST_INIT_BUG
const locale::category locale::none;
const locale::category locale::collate;
const locale::category locale::ctype;
const locale::category locale::monetary;
const locale::category locale::numeric;
const locale::category locale::time; 
const locale::category locale::messages;
const locale::category locale::all;
#endif

//----------------------------------------------------------------------
// Acquire and release low-level category objects.  The whole point of
// this is so that we don't allocate (say) four different _Locale_ctype
// objects for a single locale.

namespace {

struct eqstr {
  bool operator()(const char* s1, const char* s2) const
    { return strcmp(s1, s2) == 0; }
};

struct ptr_hash {
  size_t operator()(const void* p) const
    { return reinterpret_cast<size_t>(p); }
};

// Global hash tables for category objects.

// Look up a category by name
hash_map<const char*, pair<void*, size_t>, hash<char*>, eqstr>* ctype_hash;
hash_map<const char*, pair<void*, size_t>, hash<char*>, eqstr>* numeric_hash;
hash_map<const char*, pair<void*, size_t>, hash<char*>, eqstr>* time_hash;
hash_map<const char*, pair<void*, size_t>, hash<char*>, eqstr>* collate_hash;
hash_map<const char*, pair<void*, size_t>, hash<char*>, eqstr>* monetary_hash;
hash_map<const char*, pair<void*, size_t>, hash<char*>, eqstr>* messages_hash;

// We have a single lock for all of the hash tables.  We may wish to 
// replace it with six different locks.
_STL_mutex_lock category_hash_lock __STL_MUTEX_INITIALIZER;

template <class Category>
Category*
acquire_category(const char* name,
                 Category* (*create)(const char*),
                 const char* (*get_default_name)(char*),
                 hash_map<const char*, pair<void*, size_t>,
                          hash<char*>, eqstr>*& M)
{
  typedef hash_map<const char*, pair<void*, size_t>, hash<char*>, eqstr>
          Category_Map;
  pair<typename Category_Map::iterator, bool> result;
  _STL_auto_lock sentry(category_hash_lock);

  if (!M)
    M = new Category_Map;

  // Find what name to look for.  Be careful if user requests the default.
  char buf[_Locale_MAX_SIMPLE_NAME];
  if (name == 0 || name[0] == 0)
    name = get_default_name(buf);
  if (name == 0 || name[0] == 0)
    name = "C";

  // Look for an existing entry with that name.
  typedef const char* key_type; 
  pair<const key_type, pair<void*,size_t> > __e(name, pair<void*,size_t>(0,0));
  result = M->insert_noresize(__e);

  // There was no entry in the map already.  Create the category.
  if (result.second) 
    (*(result.first)).second.first = create(name);

  // Increment the reference count.
  ++((*(result.first)).second.second);

  return static_cast<Category*>((*(result.first)).second.first);
}

template <class Category>
void 
release_category(Category* cat,
                 void (*destroy)(Category*),
                 char* (*get_name)(const Category*, char*),
                 hash_map<const char*, pair<void*, size_t>, 
                          hash<char*>, eqstr>* M)
{
  typedef hash_map<const char*, pair<void*, size_t>, hash<char*>, eqstr>
          Category_Map;
  _STL_auto_lock sentry(category_hash_lock);

  if (cat && M) {
    // Find the name of the category object.
    char buf[_Locale_MAX_SIMPLE_NAME + 1];
    char* name = get_name(cat, buf);

    if (name != 0) {
      typename Category_Map::iterator it = M->find(name);
      if (it != M->end()) {
        // Decrement the ref count.  If it goes to zero, delete this category
        // from the map.
        if (--((*it).second.second) == 0) {
          Category* cat = static_cast<Category*>((*it).second.first);
          destroy(cat);
          M->erase(it);
        }
      }
    }
  }
}

} // Close unnamed namespace

_Locale_ctype* __acquire_ctype(const char* name) {
  return acquire_category(name,
                          _Locale_ctype_create, _Locale_ctype_default,
                          ctype_hash);
}
_Locale_numeric* __acquire_numeric(const char* name) {
  return acquire_category(name,
                          _Locale_numeric_create, _Locale_numeric_default,
                          numeric_hash);
}
_Locale_time* __acquire_time(const char* name) {
  return acquire_category(name,
                          _Locale_time_create, _Locale_time_default,
                          time_hash);
}
_Locale_collate* __acquire_collate(const char* name) {
  return acquire_category(name,
                          _Locale_collate_create, _Locale_collate_default,
                          collate_hash);
}
_Locale_monetary* __acquire_monetary(const char* name) {
  return acquire_category(name,
                          _Locale_monetary_create, _Locale_monetary_default,
                          monetary_hash);
}
_Locale_messages* __acquire_messages(const char* name) {
  return acquire_category(name,
                          _Locale_messages_create, _Locale_messages_default,
                          messages_hash);
}


void __release_ctype(_Locale_ctype* cat) {
  release_category(cat, _Locale_ctype_destroy, _Locale_ctype_name,
                   ctype_hash);
}
void __release_numeric(_Locale_numeric* cat) {
  release_category(cat, _Locale_numeric_destroy, _Locale_numeric_name,
                   numeric_hash);
}
void __release_time(_Locale_time* cat) {
  release_category(cat, _Locale_time_destroy, _Locale_time_name,
                   time_hash);
}
void __release_collate(_Locale_collate* cat) {
  release_category(cat, _Locale_collate_destroy, _Locale_collate_name,
                   collate_hash);
}
void __release_monetary(_Locale_monetary* cat) {
  release_category(cat, _Locale_monetary_destroy, _Locale_monetary_name,
                   monetary_hash);
}
void __release_messages(_Locale_messages* cat) {
  release_category(cat, _Locale_messages_destroy, _Locale_messages_name,
                   messages_hash);
}

//----------------------------------------------------------------------
// Declarations of (non-template) facets' static data members


locale::id collate<char>::id;
locale::id collate<wchar_t>::id;

locale::id ctype<char>::id;
locale::id ctype<wchar_t>::id;

locale::id codecvt<char, char, mbstate_t>::id;
locale::id codecvt<wchar_t, char, mbstate_t>::id;

locale::id moneypunct<char, true>::id;
locale::id moneypunct<char, false>::id;
locale::id moneypunct<wchar_t, true>::id;
locale::id moneypunct<wchar_t, false>::id;

locale::id numpunct<char>::id;
locale::id numpunct<wchar_t>::id;

locale::id messages<char>::id;
locale::id messages<wchar_t>::id;

__STL_END_NAMESPACE
