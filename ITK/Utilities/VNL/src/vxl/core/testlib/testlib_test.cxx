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
#include <vcl_cstdlib.h> // for abs(long)
#include <vcl_iostream.h>
#include <vcl_iomanip.h> // for setfill, setw
#include <vcl_complex.h>

static int num_test;
static int tests_passed;
static int tests_failed;
static const char* test_name;

void testlib_test_start(const char* name)
{
  num_test = 0;
  tests_passed = 0;
  tests_failed = 0;
  test_name = name;
  vcl_cout << "-----------------------------------------------------------------------------\n"
           << "Start Testing";
  if (test_name != NULL) vcl_cout << ' ' << test_name;
  vcl_cout << ":\n-----------------------------------------------------------------------------\n" << vcl_flush;
 }

void testlib_test_begin(const char* msg)
{
  num_test++;
  vcl_cout <<" Test "<< vcl_setw(3) << vcl_right << vcl_setfill('0') << num_test
           <<": "<< vcl_setw(53) << vcl_left << vcl_setfill(' ')<< msg <<" --> "
           << vcl_flush;
}

// NOTE: We don't pass in the message (see test_begin) because
//       we want to ensure that the message is printed BEFORE
//       the test is executed.  This way when a test crashes
//       we can tell if it was during a test, or between tests.
void testlib_test_perform(bool success)
{
  if (success) {
    tests_passed++;
    vcl_cout << "  PASSED\n" << vcl_flush;
  } else {
    tests_failed++;
    vcl_cout << "**FAILED**\n" << vcl_flush;
  }
}

int testlib_test_summary()
{
  vcl_cout << "-----------------------------------------------------------------------------\n";
  if (test_name) vcl_cout << test_name << ' ';
  vcl_cout << "Test Summary: ";
  if (tests_failed > 0)
  {
    if (tests_passed == 0)
      vcl_cout << "No tests succeeded";
    else if (tests_passed == 1)
      vcl_cout << "1 test succeeded";
    else
      vcl_cout << tests_passed <<" tests succeeded";
    if (tests_failed == 1)
      vcl_cout <<", 1 test failed";
    else
      vcl_cout <<", "<< tests_failed <<" tests failed";
    vcl_cout<<"\t\t*****";
  }
  else
  {
    if (tests_passed > 1)
      vcl_cout << "All "<< tests_passed <<" tests succeeded";
    else if (tests_passed == 1)
      vcl_cout << "1 test succeeded";
    else
      vcl_cout << "Test succeeded";
  }
  vcl_cout << "\n-----------------------------------------------------------------------------\n" << vcl_flush;
  return tests_failed;
}

void testlib_test_assert(const vcl_string& msg, bool expr)
{
  vcl_cout << msg << " - " << vcl_flush;
  testlib_test_perform(expr);
}

void testlib_test_assert_near(const vcl_string& msg, double expr, double target, double tol)
{
  vcl_cout << msg << " should be " << target << ", is " << expr << ", " << vcl_flush;
  double diff = vcl_abs(expr - target);
  if (target != 0.0 && diff != 0.0)
    vcl_cout << "difference " << diff << ", " << vcl_flush;
  testlib_test_perform(diff <= tol);
}

void testlib_test_assert_near(const vcl_string& msg, vcl_complex<double> expr, vcl_complex<double> target, double tol)
{
  vcl_cout << msg << " should be " << target << ", is " << expr << ", " << vcl_flush;
  double diff = vcl_abs(expr - target);
  if (target != vcl_complex<double>(0,0) && diff != 0.0)
    vcl_cout << "difference " << diff << ", " << vcl_flush;
  testlib_test_perform(diff <= tol);
}

void testlib_test_assert_near_relative(const vcl_string& msg, double expr, double target, double tol)
{
  vcl_cout << msg << " should be " << target << ", is " << expr << ", " << vcl_flush;
  double max = vcl_abs(target); if (vcl_abs(expr) > max) max = vcl_abs(expr);
  if (max==0.0 || target==0.0) max=1.0;
  double diff = vcl_abs(expr - target) / max;
  if (target != 0.0 && diff != 0.0)
    vcl_cout << "relative difference " << diff << ", " << vcl_flush;
  testlib_test_perform(diff <= tol);
}

void testlib_test_assert_near_relative(const vcl_string& msg, vcl_complex<double> expr, vcl_complex<double> target, double tol)
{
  vcl_cout << msg << " should be " << target << ", is " << expr << ", " << vcl_flush;
  double max = vcl_abs(target); if (vcl_abs(expr) > max) max = vcl_abs(expr);
  if (max==0.0 || target==vcl_complex<double>(0,0)) max=1.0;
  double diff = vcl_abs(expr - target) / max;
  if (target != vcl_complex<double>(0,0) && diff != 0.0)
    vcl_cout << "relative difference " << diff << ", " << vcl_flush;
  testlib_test_perform(diff <= tol);
}

void testlib_test_assert_far(const vcl_string& msg, double expr, double target, double tol)
{
  vcl_cout << msg << " should not be " << target << ", is " << expr << ", " << vcl_flush;
  double diff = vcl_abs(expr - target);
  if (target != 0.0 && diff != 0.0)
    vcl_cout << "difference " << diff << ", " << vcl_flush;
  testlib_test_perform(diff > tol);
}

void testlib_test_assert_far(const vcl_string& msg, vcl_complex<double> expr, vcl_complex<double> target, double tol)
{
  vcl_cout << msg << " should not be " << target << ", is " << expr << ", " << vcl_flush;
  double diff = vcl_abs(expr - target);
  if (target != vcl_complex<double>(0,0) && diff != 0.0)
    vcl_cout << "difference " << diff << ", " << vcl_flush;
  testlib_test_perform(diff > tol);
}

void testlib_test_assert_equal(const vcl_string& msg, long expr, long target)
{
  vcl_cout << msg << " should be " << target << ", is " << expr << ", " << vcl_flush;
  long diff = vcl_abs(expr - target);
  if (target != 0 && diff != 0)
    vcl_cout << "difference " << diff << ", " << vcl_flush;
  testlib_test_perform(diff == 0);
}

