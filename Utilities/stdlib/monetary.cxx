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

#include <iterator>
#include <string.h>
#include <stdlib.h>

#include <locale>
#include <algorithm> // for copy

__STL_BEGIN_NAMESPACE


template class money_get<char>;
template class money_get<char, const char*>;
template class money_put<char>;
template class money_put<char, char*>;

#ifdef INSTANTIATE_WIDE_STREAMS
template class money_get<wchar_t>;
template class money_get<wchar_t, const wchar_t*>;
template class money_put<wchar_t>;
template class money_put<wchar_t, wchar_t*>;
#endif /* INSTANTIATE_WIDE_STREAMS */

_Locale_monetary* __acquire_monetary(const char* name);
void __release_monetary(_Locale_monetary* mon);

void __init_monetary_formats(money_base::pattern& pos_format,
			     money_base::pattern& neg_format) {
  pos_format.field[0] = (char) money_base::symbol;
  pos_format.field[1] = (char) money_base::sign;
  pos_format.field[2] = (char) money_base::none;
  pos_format.field[3] = (char) money_base::value;

  neg_format.field[0] = (char) money_base::symbol;
  neg_format.field[1] = (char) money_base::sign;
  neg_format.field[2] = (char) money_base::none;
  neg_format.field[3] = (char) money_base::value;
}

void __init_monetary_formats(money_base::pattern& pos_format,
			     money_base::pattern& neg_format,
			     _Locale_monetary * monetary) {
  switch (_Locale_p_sign_posn(monetary)) {
    case 0: case 1:
      pos_format.field[0] = (char) money_base::sign;
      if (_Locale_p_cs_precedes(monetary)) {
	pos_format.field[1] = (char) money_base::symbol;
	if (_Locale_p_sep_by_space(monetary)) {
	  pos_format.field[2] = (char) money_base::space;
	  pos_format.field[3] = (char) money_base::value;
	}
	else {
	  pos_format.field[2] = (char) money_base::value;
	  pos_format.field[3] = (char) money_base::none;
	}
      }
      else {
	pos_format.field[2] = (char) money_base::value;
	if (_Locale_p_sep_by_space(monetary)) {
	  pos_format.field[2] = (char) money_base::space;
	  pos_format.field[3] = (char) money_base::symbol;
	}
	else {
	  pos_format.field[2] = (char) money_base::symbol;
	  pos_format.field[3] = (char) money_base::none;
	}
      }       
      break;
    case 2:
      if (_Locale_p_cs_precedes(monetary)) {
	pos_format.field[0] = (char) money_base::symbol;
	if (_Locale_p_sep_by_space(monetary)) {
	  pos_format.field[1] = (char) money_base::space;
	  pos_format.field[2] = (char) money_base::value;
	  pos_format.field[3] = (char) money_base::sign;
	}
	else {
	  pos_format.field[1] = (char) money_base::value;
	  pos_format.field[2] = (char) money_base::sign;
	  pos_format.field[3] = (char) money_base::none;
	}
      }
      else {
	pos_format.field[1] = (char) money_base::value;
	if (_Locale_p_sep_by_space(monetary)) {
	  pos_format.field[1] = (char) money_base::space;
	  pos_format.field[2] = (char) money_base::symbol;
	  pos_format.field[3] = (char) money_base::sign;
	}
	else {
	  pos_format.field[1] = (char) money_base::symbol;
	  pos_format.field[2] = (char) money_base::sign;
	  pos_format.field[3] = (char) money_base::none;
	}
      }
      break;
    case 3:
      if (_Locale_p_cs_precedes(monetary)) {
	pos_format.field[0] = (char) money_base::sign;
	pos_format.field[1] = (char) money_base::symbol;
	if (_Locale_p_sep_by_space(monetary)) {
	  pos_format.field[2] = (char) money_base::space;
	  pos_format.field[3] = (char) money_base::value;
	}
	else {
	  pos_format.field[2] = (char) money_base::value;
	  pos_format.field[3] = (char) money_base::none;
	}
      }
      else {
	pos_format.field[0] = (char) money_base::value;
	pos_format.field[1] = (char) money_base::sign;
	pos_format.field[2] = (char) money_base::symbol;
	pos_format.field[3] = (char) money_base::none;
      }
      break;
    case 4: default:
      if (_Locale_p_cs_precedes(monetary)) {
	pos_format.field[0] = (char) money_base::symbol;
	pos_format.field[1] = (char) money_base::sign;
	pos_format.field[2] = (char) money_base::value;
	pos_format.field[3] = (char) money_base::none;
      }
      else {
	pos_format.field[0] = (char) money_base::value;
	if (_Locale_p_sep_by_space(monetary)) {
	  pos_format.field[1] = (char) money_base::space;
	  pos_format.field[2] = (char) money_base::symbol;
	  pos_format.field[3] = (char) money_base::sign;
	}
        else {
	  pos_format.field[1] = (char) money_base::symbol;
	  pos_format.field[2] = (char) money_base::sign;
	  pos_format.field[3] = (char) money_base::none;
	}
      }
    break;
  }

  switch (_Locale_n_sign_posn(monetary)) {
    case 0: case 1:
      neg_format.field[0] = (char) money_base::sign;
      if (_Locale_n_cs_precedes(monetary)) {
        neg_format.field[1] = (char) money_base::symbol;
	if (_Locale_n_sep_by_space(monetary)) {
	  neg_format.field[2] = (char) money_base::space;
	  neg_format.field[3] = (char) money_base::value;
	}
	else {
	  neg_format.field[2] = (char) money_base::value;
	  neg_format.field[3] = (char) money_base::none;
	}
      }
      else {
	neg_format.field[2] = (char) money_base::value;
	if (_Locale_n_sep_by_space(monetary)) {
	  neg_format.field[2] = (char) money_base::space;
	  neg_format.field[3] = (char) money_base::symbol;
	}
	else {
	  neg_format.field[2] = (char) money_base::symbol;
	  neg_format.field[3] = (char) money_base::none;
	}
      }       
      break;
    case 2:
      if (_Locale_n_cs_precedes(monetary)) {
	neg_format.field[0] = (char) money_base::symbol;
	if (_Locale_n_sep_by_space(monetary)) {
	  neg_format.field[1] = (char) money_base::space;
	  neg_format.field[2] = (char) money_base::value;
	  neg_format.field[3] = (char) money_base::sign;
	}
	else {
	  neg_format.field[1] = (char) money_base::value;
	  neg_format.field[2] = (char) money_base::sign;
	  neg_format.field[3] = (char) money_base::none;
	}
      }
      else {
	neg_format.field[1] = (char) money_base::value;
	if (_Locale_n_sep_by_space(monetary)) {
	  neg_format.field[1] = (char) money_base::space;
	  neg_format.field[2] = (char) money_base::symbol;
	  neg_format.field[3] = (char) money_base::sign;
	}
	else {
	  neg_format.field[1] = (char) money_base::symbol;
	  neg_format.field[2] = (char) money_base::sign;
	  neg_format.field[3] = (char) money_base::none;
	}
      }
      break;
    case 3:
      if (_Locale_n_cs_precedes(monetary)) {
	neg_format.field[0] = (char) money_base::sign;
	neg_format.field[1] = (char) money_base::symbol;
	if (_Locale_n_sep_by_space(monetary)) {
	  neg_format.field[2] = (char) money_base::space;
	  neg_format.field[3] = (char) money_base::value;
	}
	else {
	  neg_format.field[2] = (char) money_base::value;
	  neg_format.field[3] = (char) money_base::none;
	}
      }
      else {
	neg_format.field[0] = (char) money_base::value;
	neg_format.field[1] = (char) money_base::sign;
	neg_format.field[2] = (char) money_base::symbol;
	neg_format.field[3] = (char) money_base::none;
      }
      break;
    case 4: default:
      if (_Locale_n_cs_precedes(monetary)) {
	neg_format.field[0] = (char) money_base::symbol;
	neg_format.field[1] = (char) money_base::sign;
	neg_format.field[2] = (char) money_base::value;
	neg_format.field[3] = (char) money_base::none;
      }
      else {
	neg_format.field[0] = (char) money_base::value;
	if (_Locale_n_sep_by_space(monetary)) {
	  neg_format.field[1] = (char) money_base::space;
	  neg_format.field[2] = (char) money_base::symbol;
	  neg_format.field[3] = (char) money_base::sign;
	}
        else {
	  neg_format.field[1] = (char) money_base::symbol;
	  neg_format.field[2] = (char) money_base::sign;
	  neg_format.field[3] = (char) money_base::none;
	}
      }
      break;
  }
}

moneypunct<char, true>::moneypunct(size_t refs)
  : locale::facet(refs) 
{
  __init_monetary_formats(_M_pos_format, _M_neg_format);
}

moneypunct<char, true>::~moneypunct() {}

char moneypunct<char, true>::do_decimal_point() const {return ' ';}
char moneypunct<char, true>::do_thousands_sep() const {return ' ';}
string moneypunct<char, true>::do_grouping() const {return "";}
string moneypunct<char, true>::do_curr_symbol() const {return "";}
string moneypunct<char, true>::do_positive_sign() const {return "";}
string moneypunct<char, true>::do_negative_sign() const {return "";}
int moneypunct<char, true>::do_frac_digits() const {return 0;}
money_base::pattern moneypunct<char, true>::do_pos_format() const
  {return _M_pos_format;}
money_base::pattern moneypunct<char, true>::do_neg_format() const
  {return _M_neg_format;}

moneypunct<char, false>::moneypunct(size_t refs): locale::facet(refs) {
  __init_monetary_formats(_M_pos_format, _M_neg_format);
}

moneypunct<char, false>::~moneypunct() {}

char moneypunct<char, false>::do_decimal_point() const {return ' ';}
char moneypunct<char, false>::do_thousands_sep() const {return ' ';}
string moneypunct<char, false>::do_grouping() const {return "";}
string moneypunct<char, false>::do_curr_symbol() const {return "";}
string moneypunct<char, false>::do_positive_sign() const {return "";}
string moneypunct<char, false>::do_negative_sign() const {return "";}
int moneypunct<char, false>::do_frac_digits() const {return 0;}
money_base::pattern moneypunct<char, false>::do_pos_format() const
  {return _M_pos_format;}
money_base::pattern moneypunct<char, false>::do_neg_format() const
  {return _M_neg_format;}

moneypunct<wchar_t, true>::moneypunct(size_t refs)
  : locale::facet(refs)
{
  __init_monetary_formats(_M_pos_format, _M_neg_format);
}

moneypunct<wchar_t, true>::~moneypunct()  {}

wchar_t moneypunct<wchar_t, true>::do_decimal_point() const {return L' ';}
wchar_t moneypunct<wchar_t, true>::do_thousands_sep() const {return L' ';}
string moneypunct<wchar_t, true>::do_grouping() const {return "";}
basic_string<wchar_t> moneypunct<wchar_t, true>::do_curr_symbol() const
  {return L"";}
basic_string<wchar_t> moneypunct<wchar_t, true>::do_positive_sign() const
  {return L"";}
basic_string<wchar_t> moneypunct<wchar_t, true>::do_negative_sign() const
  {return L"";}
int moneypunct<wchar_t, true>::do_frac_digits() const {return 0;}
money_base::pattern moneypunct<wchar_t, true>::do_pos_format() const
  {return _M_pos_format;}
money_base::pattern moneypunct<wchar_t, true>::do_neg_format() const
  {return _M_neg_format;}

moneypunct<wchar_t, false>::moneypunct(size_t refs): locale::facet(refs) {
  __init_monetary_formats(_M_pos_format, _M_neg_format);
}

moneypunct<wchar_t, false>::~moneypunct() {}

wchar_t moneypunct<wchar_t, false>::do_decimal_point() const {return L' ';}
wchar_t moneypunct<wchar_t, false>::do_thousands_sep() const {return L' ';}
string moneypunct<wchar_t, false>::do_grouping() const {return "";}
basic_string<wchar_t> moneypunct<wchar_t, false>::do_curr_symbol() const
  {return L"";}
basic_string<wchar_t> moneypunct<wchar_t, false>::do_positive_sign() const
  {return L"";}
basic_string<wchar_t> moneypunct<wchar_t, false>::do_negative_sign() const
  {return L"";}
int moneypunct<wchar_t, false>::do_frac_digits() const {return 0;}
money_base::pattern moneypunct<wchar_t, false>::do_pos_format() const
  {return _M_pos_format;}
money_base::pattern moneypunct<wchar_t, false>::do_neg_format() const
  {return _M_neg_format;}

moneypunct_byname<char, true>::moneypunct_byname(const char * name,
						 size_t refs):
  moneypunct<char, true>(refs),
  _M_monetary(__acquire_monetary(name))
{
  if (!_M_monetary)
    locale::_M_throw_runtime_error();
  __init_monetary_formats(_M_pos_format, _M_neg_format, _M_monetary);
}

moneypunct_byname<char, true>::~moneypunct_byname()
{
  __release_monetary(_M_monetary);
}

char moneypunct_byname<char, true>::do_decimal_point() const 
  {return _Locale_mon_decimal_point(_M_monetary);}

char moneypunct_byname<char, true>::do_thousands_sep() const
  {return _Locale_mon_thousands_sep(_M_monetary);}

string moneypunct_byname<char, true>::do_grouping() const
  {return _Locale_mon_grouping(_M_monetary);}

string moneypunct_byname<char, true>::do_curr_symbol() const
  {return _Locale_int_curr_symbol(_M_monetary);}

string moneypunct_byname<char, true>::do_positive_sign() const
  {return _Locale_positive_sign(_M_monetary);}

string moneypunct_byname<char, true>::do_negative_sign() const
  {return _Locale_negative_sign(_M_monetary);}

int moneypunct_byname<char, true>::do_frac_digits() const 
  {return _Locale_int_frac_digits(_M_monetary);}

moneypunct_byname<char, false>::moneypunct_byname(const char * name,
						  size_t refs):
  moneypunct<char, false>(refs),
  _M_monetary(__acquire_monetary(name))
{
  if (!_M_monetary)
    locale::_M_throw_runtime_error();
  __init_monetary_formats(_M_pos_format, _M_neg_format, _M_monetary);
}

moneypunct_byname<char, false>::~moneypunct_byname()
{
  __release_monetary(_M_monetary);
}

char moneypunct_byname<char, false>::do_decimal_point() const
  {return _Locale_mon_decimal_point(_M_monetary);}

char moneypunct_byname<char, false>::do_thousands_sep() const
  {return _Locale_mon_thousands_sep(_M_monetary);}

string moneypunct_byname<char, false>::do_grouping() const
  {return _Locale_mon_grouping(_M_monetary);}

string moneypunct_byname<char, false>::do_curr_symbol() const
  {return _Locale_currency_symbol(_M_monetary);}

string moneypunct_byname<char, false>::do_positive_sign() const
  {return _Locale_positive_sign(_M_monetary);}

string moneypunct_byname<char, false>::do_negative_sign() const
  {return _Locale_negative_sign(_M_monetary);}

int moneypunct_byname<char, false>::do_frac_digits() const 
  {return _Locale_frac_digits(_M_monetary);}

moneypunct_byname<wchar_t, true>::moneypunct_byname(const char * name,
						 size_t refs):
  moneypunct<wchar_t, true>(refs),
  _M_monetary(__acquire_monetary(name))
{
  if (!_M_monetary)
    locale::_M_throw_runtime_error();
  __init_monetary_formats(_M_pos_format, _M_neg_format, _M_monetary);
}

moneypunct_byname<wchar_t, true>::~moneypunct_byname() 
{
  __release_monetary(_M_monetary);
}

wchar_t moneypunct_byname<wchar_t, true>::do_decimal_point() const
  {return _Locale_mon_decimal_point(_M_monetary);}

wchar_t moneypunct_byname<wchar_t, true>::do_thousands_sep() const
  {return _Locale_mon_thousands_sep(_M_monetary);}

string moneypunct_byname<wchar_t, true>::do_grouping() const
  {return _Locale_mon_grouping(_M_monetary);}

basic_string<wchar_t> moneypunct_byname<wchar_t, true>::do_curr_symbol() const
{
  string str = _Locale_int_curr_symbol(_M_monetary);
  wstring result;
  result.resize(str.size());

  // The use of assign() would be more appropriate here,
  // but some compilers have trouble with template member
  // functions, so we use copy instead
  copy(str.begin(), str.end(), result.begin());
  return result;
}

basic_string<wchar_t> moneypunct_byname<wchar_t, true>::do_positive_sign() const
{
  string str = _Locale_positive_sign(_M_monetary);
  wstring result;
  result.resize(str.size());
  copy(str.begin(), str.end(), result.begin());
  return result;
}


basic_string<wchar_t> moneypunct_byname<wchar_t, true>::do_negative_sign() const
{
  string str = _Locale_negative_sign(_M_monetary);
  wstring result;
  result.resize(str.size());
  copy(str.begin(), str.end(), result.begin());
  return result;
}

int moneypunct_byname<wchar_t, true>::do_frac_digits() const 
  {return _Locale_int_frac_digits(_M_monetary);}

moneypunct_byname<wchar_t, false>::moneypunct_byname(const char * name,
						 size_t refs):
  moneypunct<wchar_t, false>(refs),
  _M_monetary(__acquire_monetary(name))
{
  if (!_M_monetary)
    locale::_M_throw_runtime_error() ;
  __init_monetary_formats(_M_pos_format, _M_neg_format, _M_monetary);
}

moneypunct_byname<wchar_t, false>::~moneypunct_byname()
{
  __release_monetary(_M_monetary);
}

wchar_t moneypunct_byname<wchar_t, false>::do_decimal_point() const
  {return _Locale_mon_decimal_point(_M_monetary);}

wchar_t moneypunct_byname<wchar_t, false>::do_thousands_sep() const
  {return _Locale_mon_thousands_sep(_M_monetary);}

string moneypunct_byname<wchar_t, false>::do_grouping() const
  {return _Locale_mon_grouping(_M_monetary);}

basic_string<wchar_t> moneypunct_byname<wchar_t, false>::do_curr_symbol() const
{
  string str =  _Locale_currency_symbol(_M_monetary);
  wstring result;
  result.resize(str.size());
  copy(str.begin(), str.end(), result.begin());
  return result;
}

basic_string<wchar_t> moneypunct_byname<wchar_t, false>::do_positive_sign() const
{
  string str = _Locale_positive_sign(_M_monetary);
  wstring result;
  result.resize(str.size());
  copy(str.begin(), str.end(), result.begin());
  return result;
}

basic_string<wchar_t> moneypunct_byname<wchar_t, false>::do_negative_sign() const
{
  string str = _Locale_negative_sign(_M_monetary);
  wstring result;
  result.resize(str.size());
  copy(str.begin(), str.end(), result.begin());
  return result;
}

int moneypunct_byname<wchar_t, false>::do_frac_digits() const 
  {return _Locale_frac_digits(_M_monetary);}

char* __write_monetary_value(long double units, char* buf,
                             const locale&)
{
  return buf + sprintf(buf, "%Lf", units);
}

wchar_t* __write_monetary_value(long double units, wchar_t* buf,
                                const locale& loc)
{
  char nbuf[64];
  size_t n = sprintf(nbuf, "%Lf", units);
  use_facet<ctype<wchar_t> >(loc).widen(nbuf, nbuf + n, buf);
  return buf + n;
}

__STL_END_NAMESPACE  

// Local Variables:
// mode:C++
// End:
