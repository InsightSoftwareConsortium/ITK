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

#include <stdio_streambuf>
#include <stl_config.h>
#include <stdio.h>
#ifdef __unix
#include <sys/types.h>
#include <sys/stat.h>
#endif /* __unix */

// This is an extension.  It is in namespace SGI, not namespace std
namespace SGI {

  using __STD::ios_base;
  using __STD::streamsize;

//----------------------------------------------------------------------
// Class stdio_streambuf_base

stdio_streambuf_base::stdio_streambuf_base(FILE* file)
  : _Base(file, 0),
  _M_file(file)
  {}

stdio_streambuf_base::~stdio_streambuf_base()
{
  fflush(_M_file);
}

__STD::streambuf* stdio_streambuf_base::setbuf(char* s, streamsize n)
{
  setvbuf(_M_file, s, (s == 0 && n == 0) ? _IONBF : _IOFBF, n);
  return this;
}

stdio_streambuf_base::pos_type
stdio_streambuf_base::seekoff(off_type off, ios_base::seekdir dir,
                              ios_base::openmode /* mode */)
{
  int whence;
  switch(dir) {
  case ios_base::beg:
    whence = SEEK_SET;
    break;
  case ios_base::cur:
    whence = SEEK_CUR;
    break;
  case ios_base::end:
    whence = SEEK_END;
    break;
  default:
    return pos_type(-1);
  }
      
  if (fseek(_M_file, off, whence) == 0) {
    fpos_t pos;
    fgetpos(_M_file, &pos);
    return pos_type(pos);
  }
  else
    return pos_type(-1);
}
 
stdio_streambuf_base::pos_type
stdio_streambuf_base::seekpos(pos_type pos, ios_base::openmode mode)   
{
  fpos_t p(pos);

  if (fsetpos(_M_file, &p) == 0)
    return pos;
  else
    return pos_type(-1);
}

int stdio_streambuf_base::sync()
{
  return fflush(_M_file) == 0 ? 0 : -1;
}

//----------------------------------------------------------------------
// Class stdio_istreambuf

// Defined in fstream.cxx
streamsize __remaining_characters(FILE*);

streamsize stdio_istreambuf::showmanyc()
{
  return feof(_M_file) ? -1 : __remaining_characters(_M_file);
}

stdio_istreambuf::int_type stdio_istreambuf::underflow()
{
  int c = getc(_M_file);
  if (c != EOF) {
    ungetc(c, _M_file);
    return c;
  }
  else
    return traits_type::eof();
}

stdio_istreambuf::int_type stdio_istreambuf::uflow()
{
  int c = getc(_M_file);
  return c != EOF ? c : traits_type::eof();
}

stdio_istreambuf::int_type stdio_istreambuf::pbackfail(int_type c)
{
  if (c != traits_type::eof()) {
    int result = ungetc(c, _M_file);
    return result != EOF ? result : traits_type::eof();
  }
  else{
    if (this->eback() < this->gptr()) {
      this->gbump(-1);
      return traits_type::not_eof(c);
    }
    else
      return traits_type::eof();
  }
}

//----------------------------------------------------------------------
// Class stdio_ostreambuf

streamsize stdio_ostreambuf::showmanyc()
{
  return -1;
}

stdio_ostreambuf::int_type stdio_ostreambuf::overflow(int_type c)
{
  // Write the existing buffer, without writing any additional character.
  if (c == traits_type::eof()) {
    // Do we have a buffer to write?
    ptrdiff_t unwritten = this->pptr() - this->pbase();
    if (unwritten != 0) {
      fflush(_M_file);
      // Test if the write succeeded.
      if (this->pptr() - this->pbase() < unwritten)
        return traits_type::not_eof(c);
      else
        return traits_type::eof();
    }

    // We always succeed if we don't have to do anything.
    else
      return traits_type::not_eof(c);
  }

  // Write the character c, and whatever else might be in the buffer.
  else {
    int result = putc(c, _M_file);
    return result != EOF ? result : traits_type::eof();
  }
}

} // Close namespace SGI.

// Local Variables:
// mode:C++
// End:
