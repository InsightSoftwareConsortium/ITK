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

#include <complex>

__STL_BEGIN_NAMESPACE

// Force instantiation of complex I/O functions

template basic_istream<char>&
operator>>(basic_istream<char>&, complex<float>&);

template basic_istream<char>&
operator>>(basic_istream<char>&, complex<double>&);

template basic_istream<char>&
operator>>(basic_istream<char>&, complex<long double>&);

template basic_ostream<char>&
operator<<(basic_ostream<char>&, const complex<float>&);

template basic_ostream<char>&
operator<<(basic_ostream<char>&, const complex<double>&);

template basic_ostream<char>&
operator<<(basic_ostream<char>&, const complex<long double>&);

#ifdef INSTANTIATE_WIDE_STREAMS

template basic_istream<wchar_t>& 
operator>>(basic_istream<wchar_t>&, complex<float>&);

template basic_istream<wchar_t>& 
operator>>(basic_istream<wchar_t>&, complex<double>&);

template basic_istream<wchar_t>& 
operator>>(basic_istream<wchar_t>&, complex<long double>&);

template basic_ostream<wchar_t>& 
operator<<(basic_ostream<wchar_t>&, const complex<float>&);

template basic_ostream<wchar_t>& 
operator<<(basic_ostream<wchar_t>&, const complex<double>&);

template basic_ostream<wchar_t>& 
operator<<(basic_ostream<wchar_t>&, const complex<long double>&);

#endif /* INSTANTIATE_WIDE_STREAMS */

__STL_END_NAMESPACE

// Local Variables:
// mode:C++
// End:
