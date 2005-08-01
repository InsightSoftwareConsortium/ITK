#include <vcl_iostream.h>
#include <vnl/vnl_real_npolynomial.h>
#include <vnl/vnl_double_2.h>
#include <vnl/vnl_double_3.h>
#include <vnl/algo/vnl_rnpoly_solve.h>
#include <testlib/testlib_test.h>

static void test_rnpoly_roots()
{
  // Intersection of two unit circles, centered in (0,0) and in (1,0):

  vnl_double_3 f1(1.0,1.0,-1.0);
  vnl_matrix<unsigned int> p1(3,2, 0); p1(0,0) = 2; p1(1,1) = 2;
  vnl_real_npolynomial poly1(f1,p1); vcl_cout << poly1; // X^2 +Y^2 -1

  vnl_double_2 f2(1.0,-1.0);
  vnl_matrix<unsigned int> p2(2,2, 0); p2(0,0) = 1;
  vnl_real_npolynomial monom1(f2,p2); vcl_cout << monom1; // X-1

  vnl_real_npolynomial poly2 = monom1 * monom1; // (X-1)^2
  poly2 = poly2 - 1;

  vnl_vector<double> f3(1, 1.0);
  vnl_matrix<unsigned int> p3(1,2, 0); p3(0,1) = 2;
  vnl_real_npolynomial monom3(f3,p3); // Y^2

  poly2 = poly2 + monom3; vcl_cout << poly2; // (X-1)^2 +Y^2 -1 = X^2 -2X +Y^2

  vcl_vector<vnl_vector<double>*>::iterator rp, ip;

  vcl_vector<vnl_real_npolynomial*> l(1, &poly1); l.push_back(&poly2);
  vnl_rnpoly_solve solver(l);

  vcl_vector<vnl_vector<double>*> r = solver.realroots();
  TEST("There should be two real roots", r.size(), 2);
  for (rp = r.begin(); rp != r.end(); ++rp) {
    vnl_vector<double>& root = *(*rp);
    vcl_cout << root << vcl_endl;
    TEST_NEAR("x==0.5", root[0], 0.5, 1e-9);
    TEST_NEAR("y==sqrt(0.75)", root[1]*root[1], 0.75, 1e-9);
  }
  vcl_vector<vnl_vector<double>*> roots_r = solver.real();
  vcl_vector<vnl_vector<double>*> roots_i = solver.imag();
  TEST("and no more finite imaginary roots", roots_r.size(), 2);
  TEST("and equally many imaginary parts", roots_i.size(), 2);
  for (rp=roots_r.begin(),ip=roots_i.begin(); rp!=roots_r.end(); ++rp,++ip)
    vcl_cout << "  REAL " << *((*rp)) << " IMAG " << *((*ip)) << vcl_endl;

  // Real intersection of two ellipses, both centered in (0,0):

  f1(0) = 1;   f1(1) = 2;
  vnl_real_npolynomial poly3(f1,p1); vcl_cout << poly3; // X^2 +2 Y^2 -1

  f1(0) = 2;   f1(1) = 1;
  vnl_real_npolynomial poly4(f1,p1); vcl_cout << poly4; // 2 X^2 +Y^2 -1

  l.clear(); l.push_back(&poly3); l.push_back(&poly4);
  vnl_rnpoly_solve solver2(l);

  r = solver2.realroots();
  TEST("There should be four real roots", r.size(), 4);
  for (rp = r.begin(); rp != r.end(); ++rp)
  {
    vnl_vector<double>& root = *(*rp);
    vcl_cout << root << vcl_endl;
    TEST_NEAR("x==sqrt(1/3)", 3*root[0]*root[0], 1.0, 1e-9);
    TEST_NEAR("y==sqrt(1/3)", 3*root[1]*root[1], 1.0, 1e-9);
  }
  roots_r = solver2.real(); roots_i = solver2.imag();
  TEST("and no more imaginary roots", roots_r.size(), 4);
  TEST("and equally many imaginary parts", roots_i.size(), 4);
  for (rp=roots_r.begin(),ip=roots_i.begin(); rp!=roots_r.end(); ++rp,++ip)
    vcl_cout << "  REAL " << *((*rp)) << " IMAG " << *((*ip)) << vcl_endl;

  // Imaginary intersection of two ellipses, both centered in (0,0):

  f1(0) = 2;   f1(1) = 3;
  vnl_real_npolynomial poly5(f1,p1); vcl_cout << poly5; // 2 X^2 +3 Y^2 -1

  l.clear(); l.push_back(&poly3); l.push_back(&poly5);
  vnl_rnpoly_solve solver3(l);

  r = solver3.realroots();
  TEST("There should be no real roots", r.size(), 0);
  TEST("and four imaginary roots", solver3.real().size(), 4);
  TEST("and equally many imaginary parts", solver3.imag().size(), 4);
  roots_r = solver3.real(); roots_i = solver3.imag();
  for (rp=roots_r.begin(),ip=roots_i.begin(); rp!=roots_r.end(); ++rp,++ip)
    vcl_cout << "  REAL " << *((*rp)) << " IMAG " << *((*ip)) << vcl_endl;
}

TESTMAIN(test_rnpoly_roots);
