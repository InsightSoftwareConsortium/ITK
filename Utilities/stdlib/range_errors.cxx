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
#include <stl_range_errors.h>
#include <stdexcept>

#if defined(__STL_THROW_RANGE_ERRORS) && \
    defined(__sgi) && !defined(__GNUC__) && \
    _COMPILER_VERSION >= 730 && defined(_STANDARD_C_PLUS_PLUS)

__STL_BEGIN_NAMESPACE

void __stl_throw_range_error(const char* msg)
{
  throw range_error(msg);
}

void __stl_throw_length_error(const char* msg)
{
  throw length_error(msg);
}

__STL_END_NAMESPACE

#endif

// Local Variables:
// mode:C++
// End:
