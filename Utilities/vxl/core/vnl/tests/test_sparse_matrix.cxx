// This is core/vnl/tests/test_sparse_matrix.cxx
#include <vcl_ctime.h>
#include <vcl_iostream.h>
#include <vcl_cmath.h>
#include <vnl/vnl_sparse_matrix.h>
#include <vnl/algo/vnl_sparse_symmetric_eigensystem.h>
#include <vnl/algo/vnl_symmetric_eigensystem.h>

#include <testlib/testlib_test.h>

// Test the sparse matrix operations.
void doTest1()
{
  const unsigned int n = 20;

  vnl_sparse_matrix<double> m1(n,n);
  for (unsigned i=0; i<n; i++) {
    m1(i,i) = 2.0;
    m1(i,(i+3)%n) = 1.0;
  }

  vcl_cout << "m1:\n";
  for (unsigned i=0; i<n; i++) {
    for (unsigned j=0; j<n; j++)
      vcl_cout << m1(i,j) << ' ';
    vcl_cout << vcl_endl;
  }

  vnl_sparse_matrix<double> m2(n,n);
  for (unsigned i=0; i<n; i++) {
    m2(i,i) = 2.0;
    m2(i,(i+n-3)%n) = 1.0;
  }

  vcl_cout << "m2:\n";
  for (unsigned i=0; i<n; i++) {
    for (unsigned j=0; j<n; j++)
      vcl_cout << m2(i,j) << ' ';
    vcl_cout << vcl_endl;
  }

  vnl_sparse_matrix<double> prod;
  m1.mult(m2,prod);

  vcl_cout << "prod:\n";
  for (unsigned i=0; i<n; i++) {
    for (unsigned j=0; j<n; j++)
      vcl_cout << prod(i,j) << ' ';
    vcl_cout << vcl_endl;
  }

  vnl_sparse_matrix<double> sum;
  m1.add(m2,sum);

  vcl_cout << "sum:\n";
  for (unsigned i=0; i<n; i++) {
    for (unsigned j=0; j<n; j++)
      vcl_cout << sum(i,j) << ' ';
    vcl_cout << vcl_endl;
  }

  vnl_sparse_matrix<double> diff;
  m1.subtract(m2,diff);

  vcl_cout << "diff:\n";
  for (unsigned i=0; i<n; i++) {
    for (unsigned j=0; j<n; j++)
      vcl_cout << diff(i,j) << ' ';
    vcl_cout << vcl_endl;
  }
}

void doTest2()
{
  vcl_clock_t t = vcl_clock();
  for (unsigned int n = 1000; n<4000; n+=1000) {
    vnl_sparse_matrix<double> m1(n,n);
    for (unsigned i=0; i<n; i++) {
      m1(i,i) = 2.0;
      m1(i,(i+3)%n) = 1.0;
    }

    vnl_sparse_matrix<double> m2(n,n);
    for (unsigned i=0; i<n; i++) {
      m2(i,i) = 2.0;
      m2(i,(i+n-3)%n) = 1.0;
    }

    vnl_sparse_matrix<double> prod;
    m1.mult(m2,prod);

    vcl_clock_t tn = vcl_clock();
    vcl_cout << n << ' ' << tn - t << vcl_endl;
    t = tn;
  }
}

void doTest3()
{
  const unsigned int n = 20;

  vnl_sparse_matrix<double> ms(n,n);
  vnl_matrix<double> md(n,n); md = 0.0; // Initialise to all zeros
  // The matrix must be symmetric
  for (unsigned i=0; i<n; i++) {
    ms(i,i)         = md(i,i)         = i+1.0;
    ms(i,(i+3)%n)   = md(i,(i+3)%n)   = 1.0;
    ms(i,(i+n-3)%n) = md(i,(i+n-3)%n) = 1.0;
    // ms(i,i) = md(i,i) = 1.0*(i+1)*(i+1);
  }

  vcl_cout << "ms:\n";
  for (unsigned i=0; i<n; i++) {
    for (unsigned j=0; j<n; j++)
      vcl_cout << ms(i,j) << ' ';
    vcl_cout << vcl_endl;
  }
  vcl_cout << "md:\n" << md << vcl_endl;

  const unsigned int nvals = 2;
  vnl_symmetric_eigensystem<double> ed(md);
  vnl_sparse_symmetric_eigensystem es;
  TEST ("vnl_sparse_symmetric_eigensystem::CalculateNPairs()",
        es.CalculateNPairs(ms,nvals,true,20), 0);

  // Report 'em.
  for (unsigned i=0; i<nvals; i++) {
    vcl_cout << "Dense[" << i << "] : " << ed.D(i,i) << " -> "
             << ed.get_eigenvector(i) << vcl_endl
             << "Sparse[" << i << "]: " << es.get_eigenvalue(i) << " -> "
             << es.get_eigenvector(i) << vcl_endl;
  }
}

void doTest4()
{
  const unsigned int n = 20;

  vnl_sparse_matrix<double> ms(n,n);
  vnl_matrix<double> md(n,n); md = 0.0; // Initialise to all zeros
  // The matrix must be symmetric
  for (unsigned i=0; i<n; i++) {
    ms(i,i)         = md(i,i)         = i+1.0;
    ms(i,(i+3)%n)   = md(i,(i+3)%n)   = 1.0;
    ms(i,(i+n-3)%n) = md(i,(i+n-3)%n) = 1.0;
    // ms(i,i) = md(i,i) = 1.0*(i+1)*(i+1);
  }

  const unsigned int nvals = 3;
  vnl_symmetric_eigensystem<double> ed(md);
  vnl_sparse_symmetric_eigensystem es;
  TEST("vnl_sparse_symmetric_eigensystem::CalculateNPairs() succeeded",
       es.CalculateNPairs(ms,nvals), 0);

  // Report 'em.
  for (unsigned i=0; i<nvals; i++) {
    double dense = ed.D(i,i);
    double sparse = es.get_eigenvalue(i);
    vcl_cout << "Dense[" << i << "] : " << dense << vcl_endl
             << "Sparse[" << i << "]: " << sparse << vcl_endl;
    double err = vcl_fabs(dense - sparse);
    vcl_cout << "Error: " << err << vcl_endl;
    testlib_test_assert("vnl_sparse_symmetric_eigensystem eigenvalue error", err < 1e-10);
  }
}

static void test_sparse_matrix()
{
  vcl_cout << "Starting test 1\n";
  doTest1();
  vcl_cout << "Starting test 2\n";
  doTest2();
  vcl_cout << "Starting test 3\n";
  doTest3();
  vcl_cout << "Starting test 4\n";
  doTest4();
}

TESTMAIN(test_sparse_matrix);
