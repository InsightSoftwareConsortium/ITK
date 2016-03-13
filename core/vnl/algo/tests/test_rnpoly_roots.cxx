#include <iostream>
#include <iomanip>
#include <vcl_compiler.h>
#include <vnl/vnl_real_npolynomial.h>
#include <vnl/vnl_vector.h>
#include <vnl/algo/vnl_rnpoly_solve.h>
#include <testlib/testlib_test.h>

static void print_roots(vnl_rnpoly_solve& solver)
{
  std::vector<vnl_vector<double>*> re = solver.real();
  std::vector<vnl_vector<double>*> im = solver.imag();
  const unsigned int dim = re[0]->size();
  std::cout<<"Roots are:"<<std::endl;
  std::vector<vnl_vector<double>*>::iterator rp, ip;
  for (rp=re.begin(),ip=im.begin(); rp!=re.end(); ++rp,++ip)
  {
    for (unsigned int i=0; i<dim; ++i)
    {
      vnl_vector<double>& rootr = *(*rp);
      vnl_vector<double>& rooti = *(*ip);
      std::cout<<"\t "<<rootr[i] <<" +"<<rooti[i]<<" i";
    }
    std::cout << std::endl;
  }
}

static void unit_circles_intersect()
{
  // Intersection of two unit circles, centered in (0,0) and in (1,0):
  const unsigned int dim = 2; // two-dimensional problem setting

  double f1_data[] = {1.0,1.0,-1.0}; vnl_vector<double> f1(f1_data, 3);
  vnl_matrix<unsigned int> p1(3,dim, 0); p1(0,0) = 2; p1(1,1) = 2;
  vnl_real_npolynomial poly1(f1,p1); std::cout << poly1; // X^2 +Y^2 -1

  double f2_data[] = {1.0,-1.0}; vnl_vector<double> f2(f2_data, 2);
  vnl_matrix<unsigned int> p2(2,2, 0); p2(0,0) = 1;
  vnl_real_npolynomial monom1(f2,p2); std::cout << monom1; // X-1

  vnl_real_npolynomial poly2 = monom1 * monom1; // (X-1)^2
  poly2 = poly2 - 1;

  vnl_vector<double> f3(1, 1.0);
  vnl_matrix<unsigned int> p3(1,dim, 0); p3(0,1) = 2;
  vnl_real_npolynomial monom3(f3,p3); // Y^2

  poly2 = poly2 + monom3; std::cout << poly2; // (X-1)^2 +Y^2 -1 = X^2 -2X +Y^2

  std::vector<vnl_vector<double>*>::iterator rp, ip;

  std::vector<vnl_real_npolynomial*> l(1, &poly1); l.push_back(&poly2);
  vnl_rnpoly_solve solver(l);

  std::vector<vnl_vector<double>*> r = solver.realroots();
  TEST("There should be two real roots", r.size(), 2);
  TEST("Dimensions should match", r[0]->size(), dim);
  for (rp = r.begin(); rp != r.end(); ++rp) {
    vnl_vector<double>& root = *(*rp);
    std::cout << root << std::endl;
    TEST_NEAR("x==0.5", root[0], 0.5, 1e-9);
    TEST_NEAR("y==sqrt(0.75)", root[1]*root[1], 0.75, 1e-9);
  }
  std::vector<vnl_vector<double>*> roots_r = solver.real();
  std::vector<vnl_vector<double>*> roots_i = solver.imag();
  TEST("and no more finite imaginary roots", roots_r.size(), 2);
  TEST("and equally many imaginary parts", roots_i.size(), 2);
  print_roots(solver);
}

static void ellipses_intersect()
{
  // Real intersection of two ellipses, both centered in (0,0):
  const unsigned int dim = 2; // two-dimensional problem setting

  double f1_data[] = {1.0,2.0,-1.0}; vnl_vector<double> f1(f1_data, 3);
  vnl_matrix<unsigned int> p1(3,dim, 0); p1(0,0) = 2; p1(1,1) = 2;
  vnl_real_npolynomial poly3(f1,p1); std::cout << poly3; // X^2 +2 Y^2 -1

  f1(0) = 2;   f1(1) = 1;
  vnl_real_npolynomial poly4(f1,p1); std::cout << poly4; // 2 X^2 +Y^2 -1

  std::vector<vnl_vector<double>*>::iterator rp, ip;

  std::vector<vnl_real_npolynomial*> l(1, &poly3); l.push_back(&poly4);
  vnl_rnpoly_solve solver(l);

  std::vector<vnl_vector<double>*> r = solver.realroots();
  TEST("There should be four real roots", r.size(), 4);
  TEST("Dimensions should match", r[0]->size(), dim);
  for (rp = r.begin(); rp != r.end(); ++rp)
  {
    vnl_vector<double>& root = *(*rp);
    std::cout << root << std::endl;
    TEST_NEAR("x==sqrt(1/3)", 3*root[0]*root[0], 1.0, 1e-9);
    TEST_NEAR("y==sqrt(1/3)", 3*root[1]*root[1], 1.0, 1e-9);
  }
  std::vector<vnl_vector<double>*> roots_r = solver.real();
  std::vector<vnl_vector<double>*> roots_i = solver.imag();
  TEST("and no more imaginary roots", roots_r.size(), 4);
  TEST("and equally many imaginary parts", roots_i.size(), 4);
  TEST("Dimensions should match", r[0]->size(), dim);
  print_roots(solver);

  // Imaginary intersection of two ellipses, both centered in (0,0):

  f1(0) = 2;   f1(1) = 3;
  vnl_real_npolynomial poly5(f1,p1); std::cout << poly5; // 2 X^2 +3 Y^2 -1

  l.clear(); l.push_back(&poly3); l.push_back(&poly5);
  vnl_rnpoly_solve solver3(l);

  r = solver3.realroots();
  TEST("There should be no real roots", r.size(), 0);
  TEST("and four imaginary roots", solver3.real().size(), 4);
  TEST("and equally many imaginary parts", solver3.imag().size(), 4);
  roots_r = solver3.real(); roots_i = solver3.imag();
  print_roots(solver);
}

static void single_fourth_degree()
{
  const unsigned int dim = 1; // one-dimensional problem setting
  vnl_vector<double> coeffs(5);
  // Coefficients from generating co-variance matrix for the following set of points:
  //   .9  -25.3 -118.7
  // -4.5  -22.2  -74.0
  // -9.8  -22.3  -75.0
  coeffs[0]=1;
  coeffs[1]=0;
  coeffs[2]=-40614629.07703703;
  coeffs[3]=0.0000678;
  coeffs[4]=412195996534712.2688;

  vnl_matrix<unsigned int> pol(5,dim, 0);
  pol(0,0) = 4; pol(1,0) = 3;  pol(2,0) = 2;  pol(3,0) = 1;  pol(4,0) = 0;
  vnl_real_npolynomial monom1(coeffs,pol);
  std::vector<vnl_real_npolynomial*> l(1, &monom1);
  vnl_rnpoly_solve solver(l);
  std::vector<vnl_vector<double>*> realVal = solver.real();
  std::vector<vnl_vector<double>*> imagVal = solver.imag();

  TEST("Real part of roots has size 4", realVal.size(), 4);
  TEST("Imag part of roots has size 4", imagVal.size(), 4);
  TEST("Dimensions should match", realVal[0]->size(), dim);

  std::cout << std::setprecision(2) << std::fixed;
  print_roots(solver);
  std::cout<<"Actual roots should be\n"
          "\t-4457.60 +0.00 i\n"
          "\t-4554.60 +0.00 i\n"
          "\t 4454.60 +0.00 i\n"
          "\t 4557.60 +0.00 i"<<std::endl;
}

static void scaled_fourth_degree()
{
  const unsigned int dim = 1; // one-dimensional problem setting
  vnl_vector<double> coeffs(5);
  // Coefficients from generating co-variance matrix for the following set of points:
  //   9  -253 -1187
  // -45  -222  -740
  // -98  -223  -750
  coeffs[0]=1;
  coeffs[1]=0;
  coeffs[2]=-4061462907.703703;
  coeffs[3]=0.0678;
  coeffs[4]=4121959965347122688.0;

  // Scale before computing roots:
  double factor = std::sqrt(std::sqrt(coeffs[4]/coeffs[0]));
  // this is the 4th root of the scale difference between first and last coef
  double mfactor = 1.0;
  for (int i=0; i<=4; ++i) coeffs[i]/=mfactor, mfactor*=factor;

  vnl_matrix<unsigned int> pol(5,dim, 0);
  pol(0,0) = 4; pol(1,0) = 3;  pol(2,0) = 2;  pol(3,0) = 1;  pol(4,0) = 0;
  vnl_real_npolynomial monom1(coeffs,pol);
  std::vector<vnl_real_npolynomial*> l(1, &monom1);
  vnl_rnpoly_solve solver(l);
  std::vector<vnl_vector<double>*> realVal = solver.real();
  std::vector<vnl_vector<double>*> imagVal = solver.imag();

  // Scale back the roots:
  std::vector<vnl_vector<double>*>::iterator rp, ip;
  for (rp=realVal.begin(),ip=imagVal.begin(); rp!=realVal.end(); ++rp,++ip)
  {
    for (unsigned int i=0; i<dim; ++i)
    {
      vnl_vector<double>& rootr = *(*rp); rootr *= factor;
      vnl_vector<double>& rooti = *(*ip); rooti *= factor;
    }
  }

  TEST("Real part of roots has size 4", realVal.size(), 4);
  TEST("Imag part of roots has size 4", imagVal.size(), 4);
  TEST("Dimensions should match", realVal[0]->size(), dim);

  std::cout << std::setprecision(2) << std::fixed;
  print_roots(solver);
  std::cout<<"Actual roots should be\n"
          "\t-44576.0 +0.00 i\n"
          "\t-45546.0 +0.00 i\n"
          "\t 44546.0 +0.00 i\n"
          "\t 45576.0 +0.00 i"<<std::endl;
}


static void test_rnpoly_roots()
{
  std::cout << "=========================== unit_circles_intersect ===========================\n";
  unit_circles_intersect();
  std::cout << "============================= ellipses_intersect =============================\n";
  ellipses_intersect();
  std::cout << "============================ single_fourth_degree ============================\n";
  single_fourth_degree();
  std::cout << "============================ scaled_fourth_degree ============================\n";
  scaled_fourth_degree();
}

TESTMAIN(test_rnpoly_roots);
