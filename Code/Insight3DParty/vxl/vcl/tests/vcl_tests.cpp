// vcl_tests.cpp : Run all vcl tests from one app.
//  I think this is prefereable to having many vcl_test_* projects.
//  awf, mar 2000
//

#define main test_vcl_compiler
#include "test_vcl_compiler.cxx"
#undef main

#define main test_vcl_long_double
#include "test_vcl_long_double.cxx"
#undef main

#define main test_vcl_rel_ops
#include "test_vcl_rel_ops.cxx"
#undef main

#define main test_vcl_iterator
#include "test_vcl_iterator.cxx"
#undef main

#define main test_vcl_iostream
#include "test_vcl_iostream.cxx"
#undef main

#define main test_vcl_fstream
#include "test_vcl_fstream.cxx"
#undef main

#define main test_vcl_map
#include "test_vcl_map.cxx"
#undef main

#define main test_vcl_multimap
#include "test_vcl_multimap.cxx"
#undef main

#define main test_vcl_vector
#include "test_vcl_vector.cxx"
#undef main

#define main test_vcl_list
#include "test_vcl_list.cxx"
#undef main

#define main test_vcl_string
#include "test_vcl_string.cxx"
#undef main

#define main test_vcl_set
#include "test_vcl_set.cxx"
#undef main

#define main test_vcl_deque
#include "test_vcl_deque.cxx"
#undef main

#define main test_vcl_complex
#include "test_vcl_complex.cxx"
#undef main

#define main test_vcl_algorithm
#include "test_vcl_algorithm.cxx"
#undef main

#define main test_vcl_new
#include "test_vcl_new.cxx"
#undef main

#define main test_vcl_cmath
#include "test_vcl_cmath.cxx"
#undef main

int main(int argc, char* argv[])
{
  vcl_cout << "Hello World!\n";
  test_vcl_compiler();
  test_vcl_long_double();
  test_vcl_rel_ops();
  test_vcl_iterator();
  test_vcl_iostream();
  test_vcl_fstream();
  test_vcl_map();
  test_vcl_multimap();
  test_vcl_vector();
  test_vcl_list();
  test_vcl_string();
  test_vcl_set();
  test_vcl_deque();
  test_vcl_complex();
  test_vcl_algorithm();
  test_vcl_new();
  test_vcl_cmath();

  return 0;
}

