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

#include <stl_config.h>
#include <stl_locale.h>
#include <stl_ctype.h>
#include <stl_threads.h>

__STL_BEGIN_NAMESPACE

_Locale_ctype* __acquire_ctype(const char* name); 
void __release_ctype(_Locale_ctype* cat);

// Some helper functions used in ctype<>::scan_is and scan_is_not.

namespace {

  struct Is_mask {
    typedef char argument_type;
    typedef bool result_type;

    ctype_base::mask M;
    const ctype_base::mask* table;
    
    Is_mask(ctype_base::mask m, const ctype_base::mask* t) : M(m), table(t) {}
    bool operator()(char c) const
      { return (table[(unsigned char) c] & M) != 0; }
  };


  struct wIs_mask {
    typedef wchar_t argument_type;
    typedef bool    result_type;

    ctype_base::mask M;
    const ctype_base::mask* table;
    
    wIs_mask(ctype_base::mask m, const ctype_base::mask* t)
      : M(m), table(t) {}
    bool operator()(wchar_t c) const
      { return c >= 0 
        && size_t(c) < ctype<char>::table_size
        && (table[c] & M); }
  };

  struct byname_wIs_mask {
    typedef wchar_t argument_type;
    typedef bool    result_type;

    ctype_base::mask M;
    _Locale_ctype* ctp;

    byname_wIs_mask(ctype_base::mask m, _Locale_ctype* c) : M(m), ctp(c) {}
    bool operator()(wchar_t c) const
      { return (M & _Locale_wchar_ctype(ctp, c)) != 0; }
  };

} // Close unnamed namespace

//----------------------------------------------------------------------
// ctype<char>

ctype<char>::ctype(const mask* tab, bool del, size_t refs)
  : locale::facet(refs), _M_ctype_table(0)
{
  _M_ctype_table = tab ? tab : classic_table();
  _M_delete = tab && del;
}

ctype<char>::~ctype() {
  if (_M_delete)
    delete[] const_cast<mask*>(_M_ctype_table);
  // The const_cast is a workaround for compilers that do not
  // support deleting const qualified pointers
}

const char*
ctype<char>::is(const char* low, const char* high, mask* vec) const {
  const char* p = low;
  while (p != high)
    *vec++ = table()[p++ - low];
  return high;
}

const char*
ctype<char>::scan_is(mask m, const char* low, const char* high) const {
  return find_if(low, high, Is_mask(m, this->table()));
}

const char*
ctype<char>::scan_not(mask m, const char* low, const char* high) const {
  return find_if(low, high, not1(Is_mask(m, this->table())));
}

const char* ctype<char>::do_toupper(char* low, const char* high) const {
  for ( ; low < high; ++low)
    *low = (char) _S_upper[(unsigned char) *low];
  return high;
}

const char* ctype<char>::do_tolower(char* low, const char* high) const {
  for ( ; low < high; ++low)
    *low = (char) _S_lower[(unsigned char) *low];
  return high;
}

// The classic table: static data members.
#ifndef __STL_STATIC_CONST_INIT_BUG
const size_t ctype<char>::table_size;
#endif

// Ctype table for the ASCII character set.
// There are 257 entries in this table.  The first is EOF (-1).  
// That is, the "table" seen by ctype<char> member functions is
// _S_classic_table + 1.
const ctype_base::mask 
ctype<char>::_S_classic_table[257] = 
{
  mask(0) /* EOF */,
  cntrl /* null */,
  cntrl /* ^A */,
  cntrl /* ^B */,
  cntrl /* ^C */,
  cntrl /* ^D */,
  cntrl /* ^E */,
  cntrl /* ^F */,
  cntrl /* ^G */,
  cntrl /* ^H */,
  mask(space | cntrl) /* tab */,
  mask(space | cntrl) /* LF */,
  mask(space | cntrl) /* ^K */,
  mask(space | cntrl) /* FF */,
  mask(space | cntrl) /* ^M */,
  cntrl /* ^N */,
  cntrl /* ^O */,
  cntrl /* ^P */,
  cntrl /* ^Q */,
  cntrl /* ^R */,
  cntrl /* ^S */,
  cntrl /* ^T */,
  cntrl /* ^U */,
  cntrl /* ^V */,
  cntrl /* ^W */,
  cntrl /* ^X */,
  cntrl /* ^Y */,
  cntrl /* ^Z */,
  cntrl /* esc */,
  cntrl /* ^\ */,
  cntrl /* ^] */,
  cntrl /* ^^ */,
  cntrl /* ^_ */,
  space /*  */,
  punct /* ! */,
  punct /* " */,
  punct /* # */,
  punct /* $ */,
  punct /* % */,
  punct /* & */,
  punct /* ' */,
  punct /* ( */,
  punct /* ) */,
  punct /* * */,
  punct /* + */,
  punct /* , */,
  punct /* - */,
  punct /* . */,
  punct /* / */,
  mask(digit | xdigit) /* 0 */,
  mask(digit | xdigit) /* 1 */,
  mask(digit | xdigit) /* 2 */,
  mask(digit | xdigit) /* 3 */,
  mask(digit | xdigit) /* 4 */,
  mask(digit | xdigit) /* 5 */,
  mask(digit | xdigit) /* 6 */,
  mask(digit | xdigit) /* 7 */,
  mask(digit | xdigit) /* 8 */,
  mask(digit | xdigit) /* 9 */,
  punct /* : */,
  punct /* ; */,
  punct /* < */,
  punct /* = */,
  punct /* > */,
  punct /* ? */,
  punct /* ! */,
  mask(alpha | upper | xdigit) /* A */,
  mask(alpha | upper | xdigit) /* B */,
  mask(alpha | upper | xdigit) /* C */,
  mask(alpha | upper | xdigit) /* D */,
  mask(alpha | upper | xdigit) /* E */,
  mask(alpha | upper | xdigit) /* F */,
  mask(alpha | upper) /* G */,
  mask(alpha | upper) /* H */,
  mask(alpha | upper) /* I */,
  mask(alpha | upper) /* J */,
  mask(alpha | upper) /* K */,
  mask(alpha | upper) /* L */,
  mask(alpha | upper) /* M */,
  mask(alpha | upper) /* N */,
  mask(alpha | upper) /* O */,
  mask(alpha | upper) /* P */,
  mask(alpha | upper) /* Q */,
  mask(alpha | upper) /* R */,
  mask(alpha | upper) /* S */,
  mask(alpha | upper) /* T */,
  mask(alpha | upper) /* U */,
  mask(alpha | upper) /* V */,
  mask(alpha | upper) /* W */,
  mask(alpha | upper) /* X */,
  mask(alpha | upper) /* Y */,
  mask(alpha | upper) /* Z */,
  punct /* [ */,
  punct /* \ */,
  punct /* ] */,
  punct /* ^ */,
  punct /* _ */,
  punct /* ` */,
  mask(alpha | lower | xdigit) /* a */,
  mask(alpha | lower | xdigit) /* b */,
  mask(alpha | lower | xdigit) /* c */,
  mask(alpha | lower | xdigit) /* d */,
  mask(alpha | lower | xdigit) /* e */,
  mask(alpha | lower | xdigit) /* f */,
  mask(alpha | lower) /* g */,
  mask(alpha | lower) /* h */,
  mask(alpha | lower) /* i */,
  mask(alpha | lower) /* j */,
  mask(alpha | lower) /* k */,
  mask(alpha | lower) /* l */,
  mask(alpha | lower) /* m */,
  mask(alpha | lower) /* n */,
  mask(alpha | lower) /* o */,
  mask(alpha | lower) /* p */,
  mask(alpha | lower) /* q */,
  mask(alpha | lower) /* r */,
  mask(alpha | lower) /* s */,
  mask(alpha | lower) /* t */,
  mask(alpha | lower) /* u */,
  mask(alpha | lower) /* v */,
  mask(alpha | lower) /* w */,
  mask(alpha | lower) /* x */,
  mask(alpha | lower) /* y */,
  mask(alpha | lower) /* x */,
  punct /* { */,
  punct /* | */,
  punct /* } */,
  punct /* ~ */,
  cntrl /* del (0x7f)*/,
    /* ASCII is a 7-bit code, so everything else is non-ASCII */
mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),
mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),
mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),
mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),
mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),
mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),
mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),
mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),
mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),
mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),
mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),
mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),
mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),
mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),
mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),
mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0),  mask(0)
};

// For every c in the range 0 <= c < 256, _S_upper[c] is the
// uppercased version of c and _S_lower[c] is the lowercased
// version.  As before, these two tables assume the ASCII character
// set.

const unsigned char ctype<char>::_S_upper[256] =
{
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
  0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
  0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
  0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
  0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
  0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
  0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f,
  0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
  0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f,
  0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
  0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f,
  0x60, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
  0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f,
  0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
  0x58, 0x59, 0x5a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f,
  0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
  0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
  0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
  0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
  0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
  0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
  0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7,
  0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf,
  0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
  0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
  0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
  0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
  0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
  0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
  0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7,
  0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff
};

const unsigned char ctype<char>::_S_lower[256] =
{
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
  0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
  0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
  0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
  0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
  0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
  0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f,
  0x40, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
  0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f,
  0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
  0x78, 0x79, 0x7a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f,
  0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
  0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f,
  0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
  0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f,
  0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
  0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
  0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
  0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
  0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
  0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
  0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7,
  0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf,
  0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
  0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
  0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
  0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
  0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
  0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
  0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7,
  0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff
};



//----------------------------------------------------------------------
// ctype_byname<char>

ctype_byname<char>::ctype_byname(const char* name, size_t refs)
#ifdef __GNUC__
  : ctype<char>(_M_byname_table, false, refs), // JGS, the +1 not needed 
#else
  : ctype<char>(_M_byname_table + 1, false, refs),
#endif
    _M_ctype(__acquire_ctype(name))
{

  if (!_M_ctype)
    locale::_M_throw_runtime_error();

  // We have to do this, instead of just pointer twiddling, because
  // ctype_base::mask isn't the same type as _Locale_mask_t.  

  _Locale_mask_t* p = _Locale_ctype_table(_M_ctype);
  if (!p)
    locale::_M_throw_runtime_error();

  for (size_t i = 0; i < table_size + 1; ++i) {
    _Locale_mask_t __m = p[i];
    if (__m & (upper | lower))
      __m |= alpha;
    _M_byname_table[i] = mask(__m);
  }

}

ctype_byname<char>::~ctype_byname()
{
  __release_ctype(_M_ctype);
}

char ctype_byname<char>::do_toupper(char c) const 
{
  return _Locale_toupper(_M_ctype, c);
}

char ctype_byname<char>::do_tolower(char c) const 
{
  return _Locale_tolower(_M_ctype, c);
}

const char*
ctype_byname<char>::do_toupper(char* first, const char* last) const
{
  for ( ; first != last ; ++first) 
    *first = _Locale_toupper(_M_ctype, *first);
  return last;
}

const char*
ctype_byname<char>::do_tolower(char* first, const char* last) const
{
  for ( ; first != last ; ++first) 
    *first = _Locale_tolower(_M_ctype, *first);
  return last;
}

//----------------------------------------------------------------------
// ctype<wchar_t>

ctype<wchar_t>::ctype(size_t refs)
  : locale::facet(refs)
{ }


ctype<wchar_t>::~ctype() { }

bool ctype<wchar_t>::do_is(mask m, wchar_t c) const
{
  const mask* table = ctype<char>::classic_table();
  return c >= 0 && size_t(c) < ctype<char>::table_size && (m & table[c]);
}

const wchar_t* ctype<wchar_t>::do_is(const wchar_t* low, const wchar_t* high,
                                     mask* vec) const
{
  const mask* table = ctype<char>::classic_table();
  for ( ; low < high; ++low, ++vec) {
    wchar_t c = *low;
    *vec = c >= 0 && size_t(c) < ctype<char>::table_size ? table[c] : mask(0);
  }
  return high;
}

const wchar_t*
ctype<wchar_t>::do_scan_is(mask m,
                           const wchar_t* low, const wchar_t* high) const
{
  return find_if(low, high, wIs_mask(m, ctype<char>::classic_table()));
}

const wchar_t*
ctype<wchar_t>::do_scan_not(mask m,
                            const wchar_t* low, const wchar_t* high) const
{
  return find_if(low, high, not1(wIs_mask(m, ctype<char>::classic_table())));
}

wchar_t ctype<wchar_t>::do_toupper(wchar_t c) const
{
  return c >= 0 && size_t(c) < ctype<char>::table_size
    ? (wchar_t) ctype<char>::_S_upper[c]
    : c;
}

const wchar_t* 
ctype<wchar_t>::do_toupper(wchar_t* low, const wchar_t* high) const
{
  for ( ; low < high; ++low) {
    wchar_t c = *low;
    *low = c >= 0 && size_t(c) < ctype<char>::table_size
      ? (wchar_t) ctype<char>::_S_upper[c]
      : c;
  }
  return high;
}

wchar_t ctype<wchar_t>::do_tolower(wchar_t c) const
{
  return c >= 0 && size_t(c) < ctype<char>::table_size
    ? (wchar_t) ctype<char>::_S_lower[c]
    : c;
}

const wchar_t* 
ctype<wchar_t>::do_tolower(wchar_t* low, const wchar_t* high) const
{
  for ( ; low < high; ++low) {
    wchar_t c = *low;
    *low = c >= 0 && size_t(c) < ctype<char>::table_size
      ? (wchar_t) ctype<char>::_S_lower[c]
      : c;
  }
  return high;
}

wchar_t ctype<wchar_t>::do_widen(char c) const 
{
  return (wchar_t) c;
}

const char* 
ctype<wchar_t>::do_widen(const char* low, const char* high,
                         wchar_t* dest) const
{
  while (low != high)
    *dest++ = (wchar_t) *low++;
  return high;
}

char ctype<wchar_t>::do_narrow(wchar_t c, char dfault) const 
{
  return (char) c == c ? c : dfault;
}

const wchar_t* ctype<wchar_t>::do_narrow(const wchar_t* low,
                                         const wchar_t* high,
                                         char dfault, char* dest) const
{
  while (low != high) {
    wchar_t c = *low++;
    *dest++ = (char) c == c ? c : dfault;
  }

  return high;
}

// ctype_byname<wchar_t>

ctype_byname<wchar_t>::ctype_byname(const char* name, size_t refs)
  : ctype<wchar_t>(refs),
    _M_ctype(__acquire_ctype(name))
{
  if (!_M_ctype)
    locale::_M_throw_runtime_error();    
}

ctype_byname<wchar_t>::~ctype_byname()
{
  __release_ctype(_M_ctype);
}

bool ctype_byname<wchar_t>::do_is(mask m, wchar_t c) const
{
  return (m & _Locale_wchar_ctype(_M_ctype, c)) != 0;
}

const wchar_t*
ctype_byname<wchar_t>::do_is(const wchar_t* low, const wchar_t* high,
                             mask* m) const
{
  for ( ; low < high; ++low, ++m)
    *m = mask(_Locale_wchar_ctype(_M_ctype, *low));
  return high;
}

    
const wchar_t*
ctype_byname<wchar_t>
  ::do_scan_is(mask m, const wchar_t* low, const wchar_t* high) const
{
  return find_if(low, high, byname_wIs_mask(m, _M_ctype));
}

const wchar_t*
ctype_byname<wchar_t>
  ::do_scan_not(mask m, const wchar_t* low, const wchar_t* high) const
{
  return find_if(low, high, not1(byname_wIs_mask(m, _M_ctype)));
}

wchar_t ctype_byname<wchar_t>::do_toupper(wchar_t c) const 
{
  return _Locale_wchar_toupper(_M_ctype, c);
}

const wchar_t* 
ctype_byname<wchar_t>::do_toupper(wchar_t* low, const wchar_t* high) const
{
  for ( ; low < high; ++low)
    *low = _Locale_wchar_toupper(_M_ctype, *low);
  return high;
}

wchar_t ctype_byname<wchar_t>::do_tolower(wchar_t c) const 
{
  return _Locale_wchar_tolower(_M_ctype, c);
}

const wchar_t* 
ctype_byname<wchar_t>::do_tolower(wchar_t* low, const wchar_t* high) const
{
  for ( ; low < high; ++low)
    *low = _Locale_wchar_tolower(_M_ctype, *low);
  return high;
}

__STL_END_NAMESPACE

// Local Variables:
// mode:C++
// End:

