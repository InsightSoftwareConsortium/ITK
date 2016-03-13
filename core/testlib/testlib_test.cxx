// This is core/testlib/testlib_test.cxx
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <iomanip>
#include <complex>
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
#include <vcl_compiler.h>

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
  std::cout << "-----------------------------------------------------------------------------\n"
           << "Start Testing";
  if (test_name != VXL_NULLPTR) std::cout << ' ' << test_name;
  std::cout << ":\n-----------------------------------------------------------------------------\n" << std::flush;
 }

void testlib_test_begin(const char* msg)
{
  num_test++;
  std::cout <<" Test "<< std::setw(3) << std::right << std::setfill('0') << num_test
           <<": "<< std::setw(53) << std::left << std::setfill(' ')<< msg <<" --> "
           << std::flush;
}

// NOTE: We don't pass in the message (see test_begin) because
//       we want to ensure that the message is printed BEFORE
//       the test is executed.  This way when a test crashes
//       we can tell if it was during a test, or between tests.
void testlib_test_perform(bool success)
{
  if (success) {
    tests_passed++;
    std::cout << "  PASSED\n" << std::flush;
  } else {
    tests_failed++;
    std::cout << "**FAILED**\n" << std::flush;
  }
}

int testlib_test_summary()
{
  std::cout << "-----------------------------------------------------------------------------\n";
  if (test_name) std::cout << test_name << ' ';
  std::cout << "Test Summary: ";
  if (tests_failed > 0)
  {
    if (tests_passed == 0)
      std::cout << "No tests succeeded";
    else if (tests_passed == 1)
      std::cout << "1 test succeeded";
    else
      std::cout << tests_passed <<" tests succeeded";
    if (tests_failed == 1)
      std::cout <<", 1 test failed";
    else
      std::cout <<", "<< tests_failed <<" tests failed";
    std::cout<<"\t\t*****";
  }
  else
  {
    if (tests_passed > 1)
      std::cout << "All "<< tests_passed <<" tests succeeded";
    else if (tests_passed == 1)
      std::cout << "1 test succeeded";
    else
      std::cout << "Test succeeded";
  }
  std::cout << "\n-----------------------------------------------------------------------------\n" << std::flush;
  return tests_failed;
}

void testlib_test_assert(const std::string& msg, bool expr)
{
  std::cout << msg << " - " << std::flush;
  testlib_test_perform(expr);
}

void testlib_test_assert_near(const std::string& msg, double expr, double target, double tol)
{
  std::cout << msg << " should be " << target << ", is " << expr << ", " << std::flush;
  double diff = std::abs(expr - target);
  if (target != 0.0 && diff != 0.0)
    std::cout << "difference " << diff << ", " << std::flush;
  testlib_test_perform(diff <= tol);
}

void testlib_test_assert_near(const std::string& msg, std::complex<double> expr, std::complex<double> target, double tol)
{
  std::cout << msg << " should be " << target << ", is " << expr << ", " << std::flush;
  double diff = std::abs(expr - target);
  if (target != std::complex<double>(0,0) && diff != 0.0)
    std::cout << "difference " << diff << ", " << std::flush;
  testlib_test_perform(diff <= tol);
}

void testlib_test_assert_near_relative(const std::string& msg, double expr, double target, double tol)
{
  std::cout << msg << " should be " << target << ", is " << expr << ", " << std::flush;
  double max = std::abs(target); if (std::abs(expr) > max) max = std::abs(expr);
  if (max==0.0 || target==0.0) max=1.0;
  double diff = std::abs(expr - target) / max;
  if (target != 0.0 && diff != 0.0)
    std::cout << "relative difference " << diff << ", " << std::flush;
  testlib_test_perform(diff <= tol);
}

void testlib_test_assert_near_relative(const std::string& msg, std::complex<double> expr, std::complex<double> target, double tol)
{
  std::cout << msg << " should be " << target << ", is " << expr << ", " << std::flush;
  double max = std::abs(target); if (std::abs(expr) > max) max = std::abs(expr);
  if (max==0.0 || target==std::complex<double>(0,0)) max=1.0;
  double diff = std::abs(expr - target) / max;
  if (target != std::complex<double>(0,0) && diff != 0.0)
    std::cout << "relative difference " << diff << ", " << std::flush;
  testlib_test_perform(diff <= tol);
}

void testlib_test_assert_far(const std::string& msg, double expr, double target, double tol)
{
  std::cout << msg << " should not be " << target << ", is " << expr << ", " << std::flush;
  double diff = std::abs(expr - target);
  if (target != 0.0 && diff != 0.0)
    std::cout << "difference " << diff << ", " << std::flush;
  testlib_test_perform(diff > tol);
}

void testlib_test_assert_far(const std::string& msg, std::complex<double> expr, std::complex<double> target, double tol)
{
  std::cout << msg << " should not be " << target << ", is " << expr << ", " << std::flush;
  double diff = std::abs(expr - target);
  if (target != std::complex<double>(0,0) && diff != 0.0)
    std::cout << "difference " << diff << ", " << std::flush;
  testlib_test_perform(diff > tol);
}

void testlib_test_assert_equal(const std::string& msg, long expr, long target)
{
  std::cout << msg << " should be " << target << ", is " << expr << ", " << std::flush;
  long diff = std::abs(expr - target);
  if (target != 0 && diff != 0)
    std::cout << "difference " << diff << ", " << std::flush;
  testlib_test_perform(diff == 0);
}

