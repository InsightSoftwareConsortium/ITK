/*
 * Copyright (c) 1999
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

#include <c_locale.h>
#include <limits.h>

// This is a "stub" implementation of the <c_locale.h> interface,
// intended for operating systems where we have not yet written
// a real implementation.  A C++ library using this stub implementation
// is still standard-conforming, since the C++ standard does not require
// that any locales other than "C" be supported.

// Framework functions

struct _Locale_ctype    { };
struct _Locale_numeric  { };
struct _Locale_time     { };
struct _Locale_collate  { };
struct _Locale_monetary { };
struct _Locale_messages { };

extern "C" struct _Locale_ctype* _Locale_ctype_create(const char *)
  { return 0; }
extern "C" struct _Locale_numeric* _Locale_numeric_create(const char *)
  { return 0; }
extern "C" struct _Locale_time* _Locale_time_create(const char *)
  { return 0; }
extern "C" struct _Locale_collate* _Locale_collate_create(const char *)
  { return 0; }
extern "C" struct _Locale_monetary* _Locale_monetary_create(const char *)
  { return 0; }
extern "C" struct _Locale_messages* _Locale_messages_create(const char *)
  { return 0; }

extern "C" const char* _Locale_ctype_default(char*)    { return 0; }
extern "C" const char* _Locale_numeric_default(char *) { return 0; }
extern "C" const char* _Locale_time_default(char*)     { return 0; }
extern "C" const char* _Locale_collate_default(char*)  { return 0; }
extern "C" const char* _Locale_monetary_default(char*) { return 0; }
extern "C" const char* _Locale_messages_default(char*) { return 0; }

extern "C" char* _Locale_ctype_name(const struct _Locale_ctype*, char*)
  { return 0; }
extern "C" char* _Locale_numeric_name(const struct _Locale_numeric*, char*)
  { return 0; }
extern "C" char* _Locale_time_name(const struct _Locale_time*, char*)
  { return 0; }
extern "C" char* _Locale_collate_name(const struct _Locale_collate*, char*)
  { return 0; }
extern "C" char* _Locale_monetary_name(const struct _Locale_monetary*, char*)
  { return 0; }
extern "C" char* _Locale_messages_name(const struct _Locale_messages*, char*)
  { return 0; }

extern "C" void _Locale_ctype_destroy(struct _Locale_ctype*)       {}
extern "C" void _Locale_numeric_destroy(struct _Locale_numeric*)   {}
extern "C" void _Locale_time_destroy(struct _Locale_time*)         {}
extern "C" void _Locale_collate_destroy(struct _Locale_collate*)   {}
extern "C" void _Locale_monetary_destroy(struct _Locale_monetary*) {}
extern "C" void _Locale_messages_destroy(struct _Locale_messages*) {}


extern "C" char* _Locale_extract_ctype_name(const char*, char*) 
  { return 0; }
extern "C" char* _Locale_extract_numeric_name(const char*, char*) 
  { return 0; }
extern "C" char* _Locale_extract_time_name(const char*, char*) 
  { return 0; }
extern "C" char* _Locale_extract_collate_name(const char*, char*) 
  { return 0; }
extern "C" char* _Locale_extract_monetary_name(const char*, char*) 
  { return 0; }
extern "C" char* _Locale_extract_messages_name(const char*, char*) 
  { return 0; }

char* _Locale_compose_name(char*, const char*, const char*, const char*,
                                  const char*, const char*, const char*)
  { return 0; }


// ctype

extern "C" _Locale_mask_t* _Locale_ctype_table(struct _Locale_ctype*)
  { return 0; }
extern "C" int _Locale_toupper(struct _Locale_ctype*, int) { return 0; }
extern "C" int _Locale_tolower(struct _Locale_ctype*, int) { return 0; }

extern "C" _Locale_mask_t _Locale_wchar_ctype(struct _Locale_ctype*, wint_t)
  { return 0; }
extern "C" wint_t _Locale_wchar_tolower(struct _Locale_ctype*, wint_t)
  { return 0; }
extern "C" wint_t _Locale_wchar_toupper(struct _Locale_ctype*, wint_t)
  { return 0; }


extern "C" int _Locale_mb_cur_max (struct _Locale_ctype *) { return 0; }
extern "C" int _Locale_mb_cur_min (struct _Locale_ctype *) { return 0; }
extern "C" int _Locale_is_stateless (struct _Locale_ctype *) { return 1; }
extern "C" wint_t _Locale_btowc(struct _Locale_ctype *, int c) { return 0; }
extern "C" int _Locale_wctob(struct _Locale_ctype *, wint_t c) { return 0; }
extern "C" size_t _Locale_mbtowc(struct _Locale_ctype *,
                                 wchar_t *to,
                                 const char *from, size_t n,
                                 mbstate_t *shift_state) {
  return (size_t) -1; 
}
extern "C" size_t _Locale_wctomb(struct _Locale_ctype *,
                                 char *to, size_t n,
                                 const wchar_t c,
                                 mbstate_t *shift_state) {
  return (size_t) -1;
}
extern "C" size_t _Locale_unshift(struct _Locale_ctype *,
                                  mbstate_t *st,
                                  char *buf, size_t n, char **next) {
  return (size_t) -1;
}


// Collate
extern "C" int _Locale_strcmp(struct _Locale_collate*,
                              const char*, size_t, const char*, size_t) {
  return 0;
}
extern "C" int _Locale_strwcmp(struct _Locale_collate*,
                               const wchar_t*, size_t,
                               const wchar_t*, size_t) {
  return 0;
}

extern "C" size_t _Locale_strxfrm(struct _Locale_collate*,
                                  char*, size_t, const char*, size_t) {
  return 0;
}
 extern "C" size_t _Locale_strwxfrm(struct _Locale_collate*,
                                    wchar_t*, size_t, const wchar_t*, size_t)
{
  return 0;
}


// Numeric

extern "C" char _Locale_decimal_point(struct _Locale_numeric*) { return '.'; }
extern "C" char _Locale_thousands_sep(struct _Locale_numeric*) { return ','; }
extern "C" const char* _Locale_grouping(struct _Locale_numeric *) 
  { return ""; }

extern "C" const char * _Locale_true(struct _Locale_numeric *)  { return 0; }
extern "C" const char * _Locale_false(struct _Locale_numeric *) { return 0; }


// Monetary

extern "C" const char* _Locale_int_curr_symbol(struct _Locale_monetary *)
  { return 0; }
extern "C" const char* _Locale_currency_symbol(struct _Locale_monetary *)
  { return 0; }
extern "C" char        _Locale_mon_decimal_point(struct _Locale_monetary *)
  { return '.'; }
extern "C" char        _Locale_mon_thousands_sep(struct _Locale_monetary *)
  { return ','; }
extern "C" const char* _Locale_mon_grouping(struct _Locale_monetary *)
  { return ""; }
extern "C" const char* _Locale_positive_sign(struct _Locale_monetary *)
  { return ""; }
extern "C" const char* _Locale_negative_sign(struct _Locale_monetary *)
  { return ""; }
extern "C" char        _Locale_int_frac_digits(struct _Locale_monetary *)
  { return CHAR_MAX; }
extern "C" char        _Locale_frac_digits(struct _Locale_monetary *)
  { return CHAR_MAX; }
extern "C" int         _Locale_p_cs_precedes(struct _Locale_monetary *)
  { return CHAR_MAX; }
extern "C" int         _Locale_p_sep_by_space(struct _Locale_monetary *)
  { return CHAR_MAX; }
extern "C" int         _Locale_p_sign_posn(struct _Locale_monetary *)
  { return CHAR_MAX; }
extern "C" int         _Locale_n_cs_precedes(struct _Locale_monetary *)
  { return CHAR_MAX; }
extern "C" int          _Locale_n_sep_by_space(struct _Locale_monetary *)
  { return CHAR_MAX; }
extern "C" int          _Locale_n_sign_posn(struct _Locale_monetary *)
  { return CHAR_MAX; }


// Time
extern "C" const char ** _Locale_full_monthname(struct _Locale_time *)
  { return 0; }
extern "C" const char ** _Locale_abbrev_monthname(struct _Locale_time *)
  { return 0; }
extern "C" const char ** _Locale_full_dayofweek(struct _Locale_time *)
  { return 0; }
extern "C" const char ** _Locale_abbrev_dayofweek(struct _Locale_time *)
  { return 0; }

extern "C" const char* _Locale_d_t_fmt(struct _Locale_time*)    { return 0; }
extern "C" const char* _Locale_d_fmt(struct _Locale_time*)      { return 0; }
extern "C" const char* _Locale_t_fmt(struct _Locale_time*)      { return 0; }
extern "C" const char* _Locale_am_str(struct _Locale_time*)     { return 0; }
extern "C" const char* _Locale_pm_str(struct _Locale_time*)     { return 0; }
extern "C" const char* _Locale_t_fmt_ampm(struct _Locale_time*) { return 0; }

// Messages

extern "C" int _Locale_catopen(struct _Locale_messages*, const char*)
  { return -1; }
extern "C" void _Locale_catclose(struct _Locale_messages*, int catalog) {}
extern "C" const char* _Locale_catgets(struct _Locale_messages*, int,
                                       int, int,
                                       const char *dfault)
  { return dfault; }


// Local Variables:
// mode:C++
// End:
