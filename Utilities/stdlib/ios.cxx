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

#include <stdlib.h>
#include <string>
#include <algorithm>
#include <ios>
#include <streambuf>
#include <stl_threads.h>

__STL_BEGIN_NAMESPACE

//----------------------------------------------------------------------
// ios_base members

// class ios_base::failure, a subclass of exception.  It's used solely
// for reporting errors.

ios_base::failure::failure(const string& s) 
  : __Named_exception(s)
{}

ios_base::failure::~failure() __STL_NOTHROW {}

#ifndef __STL_STATIC_CONST_INIT_BUG
// Definitions of ios_base's formatting flags.
const ios_base::fmtflags ios_base::left;
const ios_base::fmtflags ios_base::right;
const ios_base::fmtflags ios_base::internal;
const ios_base::fmtflags ios_base::dec;
const ios_base::fmtflags ios_base::hex;
const ios_base::fmtflags ios_base::oct;
const ios_base::fmtflags ios_base::fixed;
const ios_base::fmtflags ios_base::scientific;
const ios_base::fmtflags ios_base::boolalpha;
const ios_base::fmtflags ios_base::showbase;
const ios_base::fmtflags ios_base::showpoint;
const ios_base::fmtflags ios_base::showpos;
const ios_base::fmtflags ios_base::skipws;
const ios_base::fmtflags ios_base::unitbuf;
const ios_base::fmtflags ios_base::uppercase;
const ios_base::fmtflags ios_base::adjustfield;
const ios_base::fmtflags ios_base::basefield;
const ios_base::fmtflags ios_base::floatfield;

// Definitions of ios_base's state flags.
const ios_base::iostate ios_base::goodbit;
const ios_base::iostate ios_base::badbit;
const ios_base::iostate ios_base::eofbit;
const ios_base::iostate ios_base::failbit;

// Definitions of ios_base's openmode flags.
const ios_base::openmode ios_base::app;
const ios_base::openmode ios_base::ate;
const ios_base::openmode ios_base::binary;
const ios_base::openmode ios_base::in;
const ios_base::openmode ios_base::out;
const ios_base::openmode ios_base::trunc;

// Definitions of ios_base's seekdir flags.
const ios_base::seekdir ios_base::beg;
const ios_base::seekdir ios_base::cur;
const ios_base::seekdir ios_base::end;
#endif

// Internal functions used for managing exponentially-growing arrays of
// POD types.
namespace {

// array is a pointer to N elements of type PODType.  Expands the array,
// if necessary, so that array[index] is meaningful.  All new elements are
// initialized to zero.  Returns a pointer to the new array, and the new
// size.
template <class PODType>
pair<PODType*, size_t> expand_array(PODType* array, size_t N, int index)
{
  if (int(N) < index + 1) {
    size_t new_N = max(2 * N, size_t(index + 1));
    PODType* new_array
      = static_cast<PODType*>(realloc(array, new_N * sizeof(PODType)));
    if (new_array) {
      fill(new_array + N, new_array + new_N, PODType());
      return make_pair(new_array, new_N);
    }
    else 
      return pair<PODType*, size_t>(static_cast<PODType*>(0), 0);
  }
  else
    return make_pair(array, N);
}

// array is a pointer to N elements of type PODType.  Allocate a new
// array of N elements, copying the values from the old array to the new.
// Return a pointer to the new array.  It is assumed that array is non-null
// and N is nonzero.
template <class PODType>
PODType* copy_array(const PODType* array, size_t N) {
  PODType* result = static_cast<PODType*>(malloc(N * sizeof(PODType)));
  if (result)
    copy(array, array + N, result);
  return result;
}

} // Close unnamed namespace.

locale ios_base::imbue(const locale& loc) {
    locale previous = _M_locale;
    _M_locale = loc;
    _M_invoke_callbacks(imbue_event);
    return previous;
}

int ios_base::_S_index = 0;

int ios_base::xalloc()
{
  static _STL_mutex_lock L __STL_MUTEX_INITIALIZER;
  _STL_auto_lock sentry(L);
  return _S_index++;
}

long& ios_base::iword(int index) {
  static long dummy = 0;

  pair<long*, size_t> tmp = expand_array(_M_iwords, _M_num_iwords, index);
  if (tmp.first) {              // The allocation, if any, succeeded.
    _M_iwords = tmp.first;
    _M_num_iwords = tmp.second;
    return _M_iwords[index];
  }
  else {
    _M_setstate_nothrow(badbit);
    _M_check_exception_mask();
    return dummy;
  }
}

 
void*& ios_base::pword(int index) {
  static void* dummy = 0;

  pair<void**, size_t> tmp = expand_array(_M_pwords, _M_num_pwords, index);
  if (tmp.first) {              // The allocation, if any, succeeded.
    _M_pwords = tmp.first;
    _M_num_pwords = tmp.second;
    return _M_pwords[index];
  }
  else {
    _M_setstate_nothrow(badbit);
    _M_check_exception_mask();
    return dummy;
  }
}

void ios_base::register_callback(event_callback __fn, int index) {
  pair<pair<event_callback, int>*, size_t> tmp
    = expand_array(_M_callbacks, _M_num_callbacks, _M_callback_index);
  if (tmp.first) {
    _M_callbacks = tmp.first;
    _M_num_callbacks = tmp.second;
    _M_callbacks[_M_callback_index++] = make_pair(__fn, index);
  }
  else {
    _M_setstate_nothrow(badbit);
    _M_check_exception_mask();
  }
}

// Invokes all currently registered callbacks for a particular event.
// Behaves correctly even if one of the callbacks adds a new callback.
void ios_base::_M_invoke_callbacks(event E) {
  for (size_t i = _M_callback_index; i > 0; --i) {
    event_callback f = _M_callbacks[i-1].first;
    int n = _M_callbacks[i-1].second;
    f(E, *this, n);
  }
}

// This function is called if the state, rdstate(), has a bit set
// that is also set in the exception mask exceptions().
void ios_base::_M_throw_failure() {
  char buffer[256];

  sprintf(buffer, "ios failure: rdstate = 0x%04x, mask = 0x%04x",
          static_cast<unsigned int>(_M_iostate),
          static_cast<unsigned int>(_M_exception_mask));
  throw failure(buffer);
}

// Copy x's state to *this.  This member function is used in the 
// implementation of basic_ios::copyfmt.  Does not copy _M_exception_mask
// or _M_iostate.  
void ios_base::_M_copy_state(const ios_base& x) {
  _M_fmtflags  = x._M_fmtflags; // Copy the flags, except for _M_iostate
  _M_openmode  = x._M_openmode; // and _M_exception_mask.
  _M_seekdir   = x._M_seekdir;
  _M_precision = x._M_precision;
  _M_width     = x._M_width;
  _M_locale    = x._M_locale;

  if (x._M_callbacks) {
    pair<event_callback, int>* tmp
      = copy_array(x._M_callbacks, x._M_callback_index);
    if (tmp) {
      free(_M_callbacks);
      _M_callbacks = tmp;
      _M_num_callbacks = _M_callback_index = x._M_callback_index;
    }
    else {
      _M_setstate_nothrow(badbit);
      _M_check_exception_mask();
    }
  }

  if (x._M_iwords) {
    long* tmp = copy_array(x._M_iwords, x._M_num_iwords);
    if (tmp) {
      free(_M_iwords);
      _M_iwords = tmp;
      _M_num_iwords = x._M_num_iwords;
    }
    else {
      _M_setstate_nothrow(badbit);
      _M_check_exception_mask();
    }
  }

  if (x._M_pwords) {
    void** tmp = copy_array(x._M_pwords, x._M_num_pwords);
    if (tmp) {
      free(_M_pwords);
      _M_pwords = tmp;
      _M_num_pwords = x._M_num_pwords;
    }
    else {
      _M_setstate_nothrow(badbit);
      _M_check_exception_mask();
    }
  }
}


// ios's (protected) default constructor.  The standard says that all 
// fields have indeterminate values; we initialize them to zero for
// simplicity.  The only thing that really matters is that the arrays
// are all initially null pointers, and the array element counts are all
// initially zero.
ios_base::ios_base()
  : _M_fmtflags(0), _M_iostate(0), _M_openmode(0), _M_seekdir(0),
    _M_exception_mask(0),
    _M_precision(0), _M_width(0),
    _M_locale(),
    _M_callbacks(0), _M_num_callbacks(0), _M_callback_index(0),
    _M_iwords(0), _M_num_iwords(0),
    _M_pwords(0),
    _M_num_pwords(0)
{ }

// ios's destructor.
ios_base::~ios_base() {
  _M_invoke_callbacks(erase_event);
  free(_M_callbacks);
  free(_M_iwords);
  free(_M_pwords);
}


//----------------------------------------------------------------------
// Force instantiation of basic_ios

template class basic_ios<char>;

#ifdef INSTANTIATE_WIDE_STREAMS
template class basic_ios<wchar_t>;
#endif /* INSTANTIATE_WIDE_STREAMS */

__STL_END_NAMESPACE

// Local Variables:
// mode:C++
// End:
