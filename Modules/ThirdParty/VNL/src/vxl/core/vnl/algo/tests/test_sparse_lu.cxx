// This is core/vnl/algo/tests/test_sparse_lu.cxx
#include <iostream>
#include <testlib/testlib_test.h>
#include <vcl_compiler.h>
#include <vnl/vnl_sparse_matrix.h>
#include <vnl/algo/vnl_sparse_lu.h>
#include "test_util.h"

static void test_sparse_lu()
{
  //mat0 of Kenneth S. Kunder's Sparse 1.3a release
  vnl_sparse_matrix<double> A(4,4);
  std::vector<int> cols0(2), cols1(3), cols2(3), cols3(2);
  std::vector<double> vals0(2), vals1(3), vals2(3), vals3(2);
  cols0[0]=0;   cols0[1]=1;
  vals0[0]=2.0; vals0[1]=-1.0;
  A.set_row(0, cols0, vals0);
  cols1[0]=0; cols1[1]=1; cols1[2]=2;
  vals1[0]=-1.0; vals1[1]=3.0; vals1[2]=-1;
  A.set_row(1, cols1, vals1);
  cols2[0]=1; cols2[1]= 2; cols2[2]= 3;
  vals2[0]=-1.0; vals2[1]=3.0; vals2[2]=-1.0;
  A.set_row(2, cols2, vals2);
  cols3[0]=2; cols3[1]=3;
  vals3[0]=-1.0; vals3[1]=3.0;
  A.set_row(3, cols3, vals3);
  for (A.reset(); A.next();)
    std::cout << "A[" << A.getrow() << "][" << A.getcolumn()
             << "]= " << A.value() << '\n';
  vnl_vector<double> b(4, 0.0), x(4);
  b[0]=34.0;
  vnl_sparse_lu lu(A,vnl_sparse_lu::verbose);
  lu.solve(b, &x);
  for (unsigned i = 0; i<4; ++i)
    std::cout << "x[" << i << "]= " << x[i] << '\n';
  TEST_NEAR("solution of mat0 example", x[0], 21, 1.e-03);
   double det = lu.determinant();
  std::cout << "determinant = " << det << '\n';
  TEST_NEAR("determinant of mat0 example", det, 34, 1.e-03);
  lu.solve_transpose(b,&x);
  std::cout << "transpose solution\n";
  for (unsigned i = 0; i<4; ++i)
  std::cout << "x[" << i << "]= " << x[i] << '\n';
  TEST_NEAR("transpose solution of mat0 example", x[2], 3, 1.e-03);
  //mat5 of sparse test data
  vnl_sparse_matrix<double> Ap(3,3);
  Ap(0,1)=1; Ap(1,2)=1; Ap(2,0)=1;
  vnl_vector<double> bp(3), xp(3);
  bp[0]=2.0; bp[1]=3.0; bp[2]=1.0;
  vnl_sparse_lu lup(Ap,vnl_sparse_lu::verbose);
  lup.solve(bp, &xp);
  for (unsigned i = 0; i<3; ++i)
    std::cout << "xp[" << i << "]= " << xp[i] << '\n';
  TEST_NEAR("solution of mat5 example", xp[2], 3, 1.e-03);

  //test matrix derived from Poisson birth-death queue
  double s = -0.01, l = 0.5, m = 0.5;
  vnl_sparse_matrix<double> S(6,6);
  S(0,0)=s+l; S(0,1)=-l;
  S(1,0)=-m; S(1,1)=s+l+m; S(1,2)=-l;
  S(2,1)=-m; S(2,2)=s+l+m;
  S(3,3)=s+l+m; S(3,4)=-l;
  S(4,3)=-m; S(4,4)=s+l+m; S(4,5)=-l;
  S(5,4)=-m; S(5,5)=m+s;
  vnl_vector<double> bbd(6),xbd(6);
  bbd[0]=0; bbd[1]=0; bbd[2]=l; bbd[3]=m; bbd[4]=0; bbd[5]=0;
  vnl_sparse_lu lubd(S,vnl_sparse_lu::estimate_condition_verbose);
  lubd.solve(bbd, &xbd);
  for (unsigned i = 0; i<6; ++i)
    std::cout << "xbd[" << i << "]= " << xbd[i] << '\n';
  TEST_NEAR("test solution of birth-death matrix", xbd[2], 1.06622, 1.e-04);
  det = lubd.determinant();
  std::cout << "birth-death determinant = " << det << '\n';
  double cond = lubd.rcond();
  std::cout << "birth-death condition number = " << cond << '\n';
  TEST_NEAR("birth-death matrix condition number", cond, 0.03756, 1.e-04);
  double upbnd = lubd.max_error_bound();
  std::cout << "birth-death upper error bound = " << upbnd << '\n';
  TEST_NEAR("birth-death upper error", upbnd, 5.923e-015, 1.e-016);
}

TESTMAIN(test_sparse_lu);
