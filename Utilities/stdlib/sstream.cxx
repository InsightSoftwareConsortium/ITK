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

#include <sstream>

__STL_BEGIN_NAMESPACE

// Force instantiation of stringstream classes.

template class basic_string<char>;
template class basic_stringbuf<char>;
template class basic_ostringstream<char>;
template class basic_istringstream<char>;
template class basic_stringstream<char>;

#ifdef INSTANTIATE_WIDE_STREAMS
template class basic_string<wchar_t>;
template class basic_stringbuf<wchar_t>;
template class basic_ostringstream<wchar_t>;
template class basic_istringstream<wchar_t>;
template class basic_stringstream<wchar_t>;
#endif /* INSTANTIATE_WIDE_STREAMS */

__STL_END_NAMESPACE

// Local Variables:
// mode:C++
// End:
