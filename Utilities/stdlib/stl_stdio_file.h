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


#ifndef __SGI_STL_STDIO_FILE_H
#define __SGI_STL_STDIO_FILE_H

// This file provides a low-level interface between the internal 
// representation of struct FILE, from the C stdio library, and 
// the C++ I/O library.  The C++ I/O library views a FILE object as
// a collection of three pointers: the beginning of the buffer, the
// current read/write position, and the end of the buffer.

// The interface:
// - char* _FILE_[IO]_begin(const FILE& __f);
//       Returns a pointer to the beginning of the buffer.
// - char* _FILE_[IO]_next(const FILE& __f);
//       Returns the current read/write position within the buffer.
// - char* _FILE_[IO]_end(const FILE& __f);
//       Returns a pointer immediately past the end of the buffer.
// - char* _FILE_[IO]_avail(const FILE& __f);
//       Returns the number of characters remaining in the buffer, i.e.
//       _FILE_[IO]_end(__f) - _FILE_[IO]_next(__f).
// - char& _FILE_[IO]_preincr(FILE& __f)
//       Increments the current read/write position by 1, returning the 
//       character at the old position.
// - char& _FILE_[IO]_postincr(FILE& __f)
//       Increments the current read/write position by 1, returning the 
//       character at the old position.
// - char& _FILE_[IO]_predecr(FILE& __f)
//       Decrements the current read/write position by 1, returning the 
//       character at the old position.
// - char& _FILE_[IO]_postdecr(FILE& __f)
//       Decrements the current read/write position by 1, returning the 
//       character at the old position.
// - void _FILE_[IO]_bump(FILE& __f, int __n)
//       Increments the current read/write position by __n.
// - void _FILE_[IO]_set(FILE& __f, char* __begin, char* __next, char* __end);
//       Sets the beginning of the bufer to __begin, the current read/write
//       position to __next, and the buffer's past-the-end pointer to __end.
//       If any of those pointers is null, then all of them must be null.

// Each function comes in two versions, one for a FILE used as an input
// buffer and one for a FILE used as an output buffer.  In some stdio
// implementations the two functions are identical, but in others they are
// not.

#include <stdio.h>

__STL_BEGIN_NAMESPACE

//----------------------------------------------------------------------
// Implementation for the IRIX C library.
// Microsoft interface looks to be identical, as do several other
// versions of libc.
#if ( defined(__sgi) && !defined(__GNUC__)) || \
    defined (_MSC_VER) || defined(__sun__)

#ifdef _MSC_VER
typedef  char*          _FILE_internal_ptr_type;
#else
typedef  unsigned char* _FILE_internal_ptr_type;
#endif

inline char* _FILE_I_begin(const FILE& __f) { return (char*) __f._base; }
inline char* _FILE_I_next(const FILE& __f) { return (char*) __f._ptr; }  
inline char* _FILE_I_end(const FILE& __f)
  { return (char*) __f._ptr + __f._cnt; }

inline ptrdiff_t _FILE_I_avail(const FILE& __f) { return __f._cnt; }

inline char& _FILE_I_preincr(FILE& __f)
  { --__f._cnt; return *(char*) (++__f._ptr); }
inline char& _FILE_I_postincr(FILE& __f)
  { --__f._cnt; return *(char*) (__f._ptr++); }
inline char& _FILE_I_predecr(FILE& __f)
  { ++__f._cnt; return *(char*) (--__f._ptr); }
inline char& _FILE_I_postdecr(FILE& __f)
  { ++__f._cnt; return *(char*) (__f._ptr--); }
inline void  _FILE_I_bump(FILE& __f, int __n)
  { __f._ptr += __n; __f._cnt -= __n; }

inline void _FILE_I_set(FILE& __f, char* __begin, char* __next, char* __end) {
  __f._base = (_FILE_internal_ptr_type) __begin;
  __f._ptr  = (_FILE_internal_ptr_type) __next;
  __f._cnt  = __end - __next;
}

// For IRIX stdio, input and output FILE manipulation is identical.
inline char* _FILE_O_begin(const FILE& __f) { return _FILE_I_begin(__f); }
inline char* _FILE_O_next(const FILE& __f)  { return _FILE_I_next(__f); }
inline char* _FILE_O_end(const FILE& __f)   { return _FILE_I_end(__f); }

inline ptrdiff_t _FILE_O_avail(const FILE& __f) { return _FILE_I_avail(__f); }

inline char& _FILE_O_preincr(FILE& __f)  { return _FILE_I_preincr(__f); }
inline char& _FILE_O_postincr(FILE& __f) { return _FILE_I_postincr(__f); }
inline char& _FILE_O_predecr(FILE& __f)  { return _FILE_I_predecr(__f); }
inline char& _FILE_O_postdecr(FILE& __f) { return _FILE_I_postdecr(__f); }

inline void  _FILE_O_bump(FILE& __f, int __n) { _FILE_I_bump(__f, __n); }
inline void _FILE_O_set(FILE& __f, char* __begin, char* __next, char* __end)
  { _FILE_I_set(__f, __begin, __next, __end); }


// "linux" is a slight misnomer.  The real dependence here is on gnu libc.
#elif defined(__GNUC__) && defined(__linux__) 

inline char* _FILE_I_begin(const FILE& __f) { return __f._IO_read_base; }
inline char* _FILE_I_next(const FILE& __f)  { return __f._IO_read_ptr; }
inline char* _FILE_I_end(const FILE& __f)   { return __f._IO_read_end; }

inline ptrdiff_t _FILE_I_avail(const FILE& __f) 
  { return __f._IO_read_end - __f._IO_read_ptr; }

inline char& _FILE_I_preincr(FILE& __f)  { return *++__f._IO_read_ptr; }
inline char& _FILE_I_postincr(FILE& __f) { return *__f._IO_read_ptr++; }
inline char& _FILE_I_predecr(FILE& __f)  { return *--__f._IO_read_ptr; }
inline char& _FILE_I_postdecr(FILE& __f) { return *__f._IO_read_ptr--; }
inline void  _FILE_I_bump(FILE& __f, int __n) { __f._IO_read_ptr += __n; }

inline void _FILE_I_set(FILE& __f, char* __begin, char* __next, char* __end) {
  __f._IO_read_base = __begin; 
  __f._IO_read_ptr  = __next; 
  __f._IO_read_end  = __end; 
}

inline char* _FILE_O_begin(const FILE& __f) { return __f._IO_write_base; }
inline char* _FILE_O_next(const FILE& __f)  { return __f._IO_write_ptr; }
inline char* _FILE_O_end(const FILE& __f)   { return __f._IO_write_end; }

inline ptrdiff_t _FILE_O_avail(const FILE& __f) 
  { return __f._IO_write_end - __f._IO_write_ptr; }

inline char& _FILE_O_preincr(FILE& __f)  { return *++__f._IO_write_ptr; }
inline char& _FILE_O_postincr(FILE& __f) { return *__f._IO_write_ptr++; }
inline char& _FILE_O_predecr(FILE& __f)  { return *--__f._IO_write_ptr; }
inline char& _FILE_O_postdecr(FILE& __f) { return *__f._IO_write_ptr--; }
inline void  _FILE_O_bump(FILE& __f, int __n) { __f._IO_write_ptr += __n; }

inline void _FILE_O_set(FILE& __f, char* __begin, char* __next, char* __end) {
  __f._IO_write_base = __begin; 
  __f._IO_write_ptr  = __next; 
  __f._IO_write_end  = __end; 

}

#else /* A C library that we don't have an implementation for. */
#error The C++ I/O library is not configured for this operating system 
#endif

__STL_END_NAMESPACE

#endif /* __SGI_STL_STDIO_FILE_H */

// Local Variables:
// mode:C++
// End:
