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

#include <istream>
#include <ostream>
#include <fstream>
#include <locale>
#include <stdio_streambuf>

__STL_BEGIN_NAMESPACE

template class basic_istream<char>;
template class basic_ostream<char>;
template class basic_iostream<char>;

#ifdef INSTANTIATE_WIDE_STREAMS
template class basic_istream<wchar_t>;
template class basic_ostream<wchar_t>;
template class basic_iostream<wchar_t>;
#endif /* INSTANTIATE_WIDE_STREAMS */

// This file handles iostream initialization.  It is inherently
// nonportable, since the C++ language definition provides no mechanism
// for controlling order of initialization of nonlocal objects.  
// Initialization has three parts, which must be performed in the following
// order:
//  (1) Initialize the locale system by calling __initialize_locales()
//  (2) Call the constructors for the eight global stream objects.
//  (3) Create streambufs for the global stream objects, and initialize
//      the stream objects by calling the init() member function.



#if _MSC_VER

// Definitions of the eight global I/O objects that are declared in 
// <iostream>. For VC++ we use the init_seg pragma to put the global I/O
// objects into an intitialization segement that will not
// be executed. We then explicitly invoke the constructors
// with placement new in ios_base::_S_initialize() 

#pragma init_seg("SGI_STL_NO_INIT")

istream cin(0);
ostream cout(0);
ostream cerr(0);
ostream clog(0);

wistream wcin(0);
wostream wcout(0);
wostream wcerr(0);
wostream wclog(0);

#else

// Definitions of the eight global I/O objects that are declared in 
// <iostream>.  Disgusting hack: we deliberately define them with the
// wrong types so that the constructors don't get run automatically.
// We need special tricks to make sure that these objects are struct-
// aligned rather than byte-aligned.

// This is not portable.  Declaring a variable with different types in
// two translations units is "undefined", according to the C++ standard.
// Most compilers, however, silently accept this instead of diagnosing
// it as an error.

namespace {
  template <size_t N>
  union aligned_buffer {
    char buf[N];
    struct { double a; double b; } padding;
  };
}

aligned_buffer<sizeof(istream)> cin;
aligned_buffer<sizeof(ostream)> cout;
aligned_buffer<sizeof(ostream)> cerr;
aligned_buffer<sizeof(ostream)> clog;

aligned_buffer<sizeof(wistream)> wcin;
aligned_buffer<sizeof(wostream)> wcout;
aligned_buffer<sizeof(wostream)> wcerr;
aligned_buffer<sizeof(wostream)> wclog;
#endif

// Get the file number associated with an stdio stream.  On Unix
// systems there's a fileno() function in stdio.h, with Microsoft
// libc it's called _fileno.
namespace {
#if defined(_MSC_VER)
  int get_fileno(FILE* f) { return _fileno(f); }
#else
  int get_fileno(FILE* f) { return fileno(f); }
#endif
}

// Member functions from class ios_base and ios_base::Init

long ios_base::Init::_S_count = 0;

void ios_base::_S_initialize()
{
  using SGI::stdio_istreambuf;
  using SGI::stdio_ostreambuf;

  // Initialize the locale system.
  locale::_S_initialize();

  try {
    // Run constructors for the four narrow stream objects.

    istream* ptr_cin  = new(&cin)  istream(0);
    ostream* ptr_cout = new(&cout) ostream(0);
    ostream* ptr_cerr = new(&cerr) ostream(0);
    ostream* ptr_clog = new(&clog) ostream(0);

    // Initialize the four narrow stream objects.
    ptr_cin->init(new stdio_istreambuf(stdin));
    ptr_cout->init(new stdio_ostreambuf(stdout));
    ptr_cerr->init(new stdio_ostreambuf(stderr));
    ptr_clog->init(new stdio_ostreambuf(stderr));

    ptr_cin->tie(ptr_cout);
    ptr_cerr->setf(ios_base::unitbuf);

    // Run constructors for the four wide stream objects.
    wistream* ptr_wcin  = new(&wcin)  wistream(0);
    wostream* ptr_wcout = new(&wcout) wostream(0);
    wostream* ptr_wcerr = new(&wcerr) wostream(0);
    wostream* ptr_wclog = new(&wclog) wostream(0);

    // Initialize the four wide stream objects.
    wfilebuf* win  = new wfilebuf;
    wfilebuf* wout = new wfilebuf;
    wfilebuf* werr = new wfilebuf;
    wfilebuf* wlog = new wfilebuf;

    if (win->open(get_fileno(stdin)) == 0) {
      delete win;
      win = 0;
    }
    if (wout->open(get_fileno(stdout)) == 0) {
      delete wout;
      wout = 0;
    }
    if (werr->open(get_fileno(stderr)) == 0) {
      delete werr;
      werr = 0;
    }
    if (wlog->open(get_fileno(stderr)) == 0) {
      delete wlog;
      wlog = 0;
    }

    ptr_wcin->init(win);
    ptr_wcout->init(wout);
    ptr_wcerr->init(werr);
    ptr_wclog->init(wlog);

    ptr_wcin->tie(ptr_wcout);
    ptr_wcerr->setf(ios_base::unitbuf);
  }
  catch(...) {}
}

void ios_base::_S_uninitialize()
{
  // Note that destroying output streambufs flushes the buffers.

  istream* ptr_cin  = reinterpret_cast<istream*>(&cin);
  ostream* ptr_cout = reinterpret_cast<ostream*>(&cout);
  ostream* ptr_cerr = reinterpret_cast<ostream*>(&cerr);
  ostream* ptr_clog = reinterpret_cast<ostream*>(&clog);

  wistream* ptr_wcin  = reinterpret_cast<wistream*>(&wcin);
  wostream* ptr_wcout = reinterpret_cast<wostream*>(&wcout);
  wostream* ptr_wcerr = reinterpret_cast<wostream*>(&wcerr);
  wostream* ptr_wclog = reinterpret_cast<wostream*>(&wclog);

  delete ptr_cin->rdbuf(0);
  delete ptr_cout->rdbuf(0);
  delete ptr_cerr->rdbuf(0);
  delete ptr_clog->rdbuf(0);

  // Call destructors virtually to make sure that construction
  // displacements are correctly calculated.
  ptr_cin->~istream();
  ptr_cout->~ostream();
  ptr_cerr->~ostream();
  ptr_clog->~ostream();

  delete ptr_wcin->rdbuf(0);
  delete ptr_wcout->rdbuf(0);
  delete ptr_wcerr->rdbuf(0);
  delete ptr_wclog->rdbuf(0);

  ptr_wcin->~wistream();
  ptr_wcout->~wostream();
  ptr_wcerr->~wostream();
  ptr_wclog->~wostream();

  // Shut down the locale subsystem.
  locale::_S_uninitialize();
}

namespace {
  filebuf* create_filebuf(FILE* f, ios_base::openmode mode)
  {
    filebuf* result = new filebuf;
    try {
      result->open(get_fileno(f));
    }
    catch(...) {}

    if (!result->is_open()) {
      delete result;
      result = 0;
    }

    return result;
  }
}

bool ios_base::sync_with_stdio(bool sync) {
  using SGI::stdio_istreambuf;
  using SGI::stdio_ostreambuf;

  istream* ptr_cin  = reinterpret_cast<istream*>(&cin);
  ostream* ptr_cout = reinterpret_cast<ostream*>(&cout);
  ostream* ptr_cerr = reinterpret_cast<ostream*>(&cerr);
  ostream* ptr_clog = reinterpret_cast<ostream*>(&clog);

  streambuf* old_cin  = ptr_cin->rdbuf();
  streambuf* old_cout = ptr_cout->rdbuf();
  streambuf* old_cerr = ptr_cerr->rdbuf();
  streambuf* old_clog = ptr_clog->rdbuf();
  bool was_synced = old_cin != 0 &&
                    dynamic_cast<stdio_istreambuf*>(old_cin) != 0;

  streambuf* new_cin  = 0;
  streambuf* new_cout = 0;
  streambuf* new_cerr = 0;
  streambuf* new_clog = 0;

  try {
    if (sync && !was_synced) {
      new_cin  = new stdio_istreambuf(stdin);
      new_cout = new stdio_ostreambuf(stdout);
      new_cerr = new stdio_ostreambuf(stderr);
      new_clog = new stdio_ostreambuf(stderr);
    }
    else if (!sync && was_synced) {
      new_cin  = create_filebuf(stdin, ios_base::in);
      new_cout = create_filebuf(stdout, ios_base::out);
      new_cerr = create_filebuf(stderr, ios_base::out);
      new_clog = create_filebuf(stderr, ios_base::out);
    }
  }
  catch(...) {}

  if (new_cin && new_cout && new_cerr && new_clog) {
    ptr_cin->rdbuf(new_cin);
    ptr_cout->rdbuf(new_cout);
    ptr_cerr->rdbuf(new_cerr);
    ptr_clog->rdbuf(new_clog);

    delete old_cin;
    delete old_cout;
    delete old_cerr;
    delete old_clog;
  }
  else {
    delete new_cin;
    delete new_cout;
    delete new_cerr;
    delete new_clog;
  }

  return was_synced;
}


__STL_END_NAMESPACE


// Local Variables:
// mode:C++
// End:
