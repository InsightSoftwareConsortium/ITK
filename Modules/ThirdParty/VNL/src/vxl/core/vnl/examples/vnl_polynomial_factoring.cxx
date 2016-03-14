// This is core/vnl/examples/vnl_polynomial_factoring.cxx

//:
// \file
// \brief Find all integer-coefficient factors of an integer polynomial
// \author Peter Vanroose, ABIS Leuven
// \date   August 2011
//-----------------------------------------------------------------------------

#include <iostream>
#include <cstdlib>
#include <vector>
#include <vcl_compiler.h>
#include <vnl/vnl_polynomial.h>

int main(int argc, char* argv[])
{
  --argc; ++argv;

  // Read coefficients from stdin, or from command line
  std::vector<long> coef(argc);
  if (argc == 0) {
    std::cout << "Give the polynomial coefficients, starting with the constant term, and end with EOF (CTRL-Z or CTRL-D)\n";
    long onecoef; while (std::cin >> onecoef) coef.push_back(onecoef);
  }
  else
    for (int i=0; i<argc; ++i)
      coef[i] = std::atoi(argv[i]);

  vnl_polynomial<long> poly(coef);
  std::cout << "Polynomial = " << poly << std::endl;

  if (poly.degree() < 2)  {
    std::cout << "Factor of degree " << poly.degree() << ":  " << poly << std::endl;
    return 0;
  }

  vnl_polynomial<long> X(1); X[0]=0L; X[1]=1L; // the monomial X.

  std::vector<long> constant_terms;
  for (long i=1; i*i <= coef.front(); ++i)
    if (i*(coef.front()/i) == coef.front())
      constant_terms.push_back(i);

  std::vector<long> highorder_terms;
  for (long i=1; i*i <= coef.back(); ++i)
    if (i*(coef.back()/i) == coef.back())
      highorder_terms.push_back(i);

  while (poly.degree() > 0 && poly[0] == 0L) {
    std::cout << "Factor of degree 1:  " << X << std::endl;
    poly /= X;
#ifdef DEBUG
    std::cerr << "Remainder:           " << poly << std::endl;
#endif
  }

  vnl_polynomial<long> p(1);

  std::vector<long>::const_iterator it1, it2;
  for (it1=constant_terms.begin() ; it1<constant_terms.end(); ++it1) {
    p[0] = *it1;
    for (it2=highorder_terms.begin() ; it2<highorder_terms.end(); ++it2) {
      p[1] = *it2;
      while (poly / p * p == poly) {
        std::cout << "Factor of degree 1:  " << p << std::endl;
#ifdef DEBUG
        std::cerr << "Remainder:           " << poly / p << std::endl;
#endif
        poly /= p;
      }
    }
  }

  p = vnl_polynomial<long>(2);

  for (it1=constant_terms.begin() ; it1<constant_terms.end(); ++it1) {
    p[0] = *it1;
    for (it2=highorder_terms.begin() ; it2<highorder_terms.end(); ++it2) {
      p[2] = *it2;
      for (long m=-1000L; m<=1000L; ++m) {
        p[1] = m;
        while (poly.degree() > 3 && poly / p * p == poly) {
          std::cout << "Factor of degree 2:  " << p << std::endl;
#ifdef DEBUG
          std::cerr << "Remainder:           " << poly / p << std::endl;
#endif
          poly /= p;
        }
      }
    }
  }

  p = vnl_polynomial<long>(3);

  for (it1=constant_terms.begin() ; it1<constant_terms.end(); ++it1) {
    p[0] = *it1;
    for (it2=highorder_terms.begin() ; it2<highorder_terms.end(); ++it2) {
      p[3] = *it2;
      for (long m1=-100L; m1<=100L; ++m1) {
        p[1] = m1;
        for (long m2=-100L; m2<=100L; ++m2) {
          p[2] = m2;
          while (poly.degree() > 3 && poly / p * p == poly) {
            std::cout << "Factor of degree 3:  " << p << std::endl;
#ifdef DEBUG
            std::cerr << "Remainder:           " << poly / p << std::endl;
#endif
            poly /= p;
          }
        }
      }
    }
  }

  if (poly.degree() > 0 && poly.degree() < 8)
    std::cout << "Factor of degree " << poly.degree() << ":  " << poly << std::endl;
  else
    std::cout << "Remainder (not yet factorized):  " << poly << std::endl;

  return 0;
}
