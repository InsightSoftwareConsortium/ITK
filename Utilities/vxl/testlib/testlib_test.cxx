// This is core/testlib/testlib_test.cxx
#include "testlib_test.h"
//
// Copyright (C) 1991 Texas Instruments Incorporated.
//
// Permission is granted to any individual or institution to use, copy, modify,
// and distribute this software, provided that this complete copyright and
// permission notice is maintained, intact, in all copies and supporting
// documentation.
//
// Texas Instruments Incorporated provides this software "as is" without
// express or implied warranty.
//
// Created: 11-Mar-2001: TFC Copy of vnl_test
// Created: 25-Apr-2002: AGAP Modified copy of testlib_test
//

#include <vcl_cmath.h>
#include <vcl_iostream.h>
#include <vcl_iomanip.h> // for setfill, setw
#include <vcl_complex.h>

static int num_test;
static int tests_passed;
static int tests_failed;
static const char* test_name;

void testlib_test_start(const char* name) {
  num_test = 0;
  tests_passed = 0;
  tests_failed = 0;
  test_name = name;
  vcl_cout << "-----------------------------------------------------------------------------\n"
           << "Start Testing";
  if (test_name != NULL) vcl_cout << " " << test_name;
  vcl_cout << ":\n-----------------------------------------------------------------------------\n";
  vcl_cout.flush();
 }

void testlib_test_begin(const char* msg) {
  num_test++;
  vcl_cout <<" Test "<< vcl_setw(3) << vcl_right << vcl_setfill('0') << num_test
           <<": "<< vcl_setw(53) << vcl_left << vcl_setfill(' ')<< msg <<" --> "
           << vcl_flush;
}

// NOTE: We don't pass in the message (see test_begin) because
//       we want to ensure that the message is printed BEFORE
//       the test is executed.  This way when a test crashes
//       we can tell if it was during a test, or between tests.
void testlib_test_perform(bool success) {
  if (success) {
    tests_passed++;
    vcl_cout << "  PASSED\n";
  } else {
    tests_failed++;
    vcl_cout << "**FAILED**\n";
  }
}

int testlib_test_summary() {
  vcl_cout << "-----------------------------------------------------------------------------\n";
  if (test_name != NULL) vcl_cout << test_name << " ";
  vcl_cout << "Test Summary: ";
  if (tests_failed > 0)
    vcl_cout<<tests_passed<<" tests succeeded, "<<tests_failed<<" tests failed\t\t\t*****";
  else
    vcl_cout<<"All "<<tests_passed<<" tests succeeded";
  vcl_cout << "\n-----------------------------------------------------------------------------\n";
  return tests_failed;
}

void testlib_test_assert(const vcl_string& msg, bool expr)
{
  vcl_cout << msg << " - ";
  testlib_test_perform(expr);
}

void testlib_test_assert_near(const vcl_string& msg, double expr, double target, double tol)
{
  vcl_cout << msg << " should be " << target << ", is " << expr << ", ";
  testlib_test_perform(vcl_abs(expr - target) < tol);
}

void testlib_test_assert_near(const vcl_string& msg, vcl_complex<double> expr, vcl_complex<double> target, double tol)
{
  vcl_cout << msg << " should be " << target << ", is " << expr << ", ";
  testlib_test_perform(vcl_abs(expr - target) < tol);
}

void testlib_test_assert_far(const vcl_string& msg, double expr, double target, double tol)
{
  vcl_cout << msg << " should not be " << target << ", is " << expr << ", ";
  testlib_test_perform(vcl_abs(expr - target) > tol);
}

void testlib_test_assert_far(const vcl_string& msg, vcl_complex<double> expr, vcl_complex<double> target, double tol)
{
  vcl_cout << msg << " should not be " << target << ", is " << expr << ", ";
  testlib_test_perform(vcl_abs(expr - target) > tol);
}
