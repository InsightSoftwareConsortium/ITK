// This is core/vnl/algo/tests/test_sparse_matrix.cxx
#include <ctime>
#include <iostream>
#include "vnl/vnl_sparse_matrix.h"
#include <vnl/algo/vnl_symmetric_eigensystem.h>
#include <vnl/algo/vnl_generalized_eigensystem.h>
#include "testlib/testlib_test.h"

// Test the sparse matrix operations.
void
doTest1()
{
  constexpr unsigned int n = 20;

  vnl_sparse_matrix<double> m1(n, n);
  for (unsigned i = 0; i < n; i++)
  {
    m1(i, i) = 2.0;
    m1(i, (i + 3) % n) = 1.0;
  }

  std::cout << "m1:\n";
  for (unsigned i = 0; i < n; i++)
  {
    for (unsigned j = 0; j < n; j++)
      std::cout << m1(i, j) << ' ';
    std::cout << std::endl;
  }

  vnl_sparse_matrix<double> m2(n, n);
  for (unsigned i = 0; i < n; i++)
  {
    m2(i, i) = 2.0;
    m2(i, (i + n - 3) % n) = 1.0;
  }

  std::cout << "m2:\n";
  for (unsigned i = 0; i < n; i++)
  {
    for (unsigned j = 0; j < n; j++)
      std::cout << m2(i, j) << ' ';
    std::cout << std::endl;
  }

  vnl_sparse_matrix<double> prod = m1 * m2;

  std::cout << "prod:\n";
  for (unsigned i = 0; i < n; i++)
  {
    for (unsigned j = 0; j < n; j++)
      std::cout << prod(i, j) << ' ';
    std::cout << std::endl;
  }

  vnl_sparse_matrix<double> sum = m1 + m2;

  std::cout << "sum:\n";
  for (unsigned i = 0; i < n; i++)
  {
    for (unsigned j = 0; j < n; j++)
      std::cout << sum(i, j) << ' ';
    std::cout << std::endl;
  }

  vnl_sparse_matrix<double> diff = m1 - m2;

  std::cout << "diff:\n";
  for (unsigned i = 0; i < n; i++)
  {
    for (unsigned j = 0; j < n; j++)
      std::cout << diff(i, j) << ' ';
    std::cout << std::endl;
  }
}

void
doTest2()
{
  std::clock_t t = std::clock();
  for (unsigned int n = 1000; n < 4000; n += 1000)
  {
    vnl_sparse_matrix<double> m1(n, n);
    for (unsigned i = 0; i < n; i++)
    {
      m1(i, i) = 2.0;
      m1(i, (i + 3) % n) = 1.0;
    }

    vnl_sparse_matrix<double> m2(n, n);
    for (unsigned i = 0; i < n; i++)
    {
      m2(i, i) = 2.0;
      m2(i, (i + n - 3) % n) = 1.0;
    }

    const vnl_sparse_matrix<double> prod = m1 * m2;

    const std::clock_t tn = std::clock();
    std::cout << n << ' ' << tn - t << std::endl;
    t = tn;
  }
}

static void
test_sparse_matrix()
{
  std::cout << "Starting test 1\n";
  doTest1();
  std::cout << "Starting test 2\n";
  doTest2();
  std::cout << "Starting test 3\n";
  std::cout << "Starting test 4\n";
  std::cout << "Starting test 5\n";
  std::cout << "Starting test 6\n";
}

TESTMAIN(test_sparse_matrix);
