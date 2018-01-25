// This is core/vnl/examples/vnl_polynomial_RPN.cxx

//:
// \file
// \brief Performs operations (+, -, *, / or %) on polynomials
//
// The input should consist of lines with either one of these operations,
// or a list of n numbers (n at least 1) which will be interpreted as the
// coefficients of a polynomial, highest order first.
// Lines will be processed in order; a polynomial will be put on a stack
// (and pretty-printed) while an operation will replace the last two stack
// entries by the result of the operation on those two.
// An additional operation "." is available to push a copy of the last stack
// element to the stack.
//
// \author Peter Vanroose, ABIS Leuven
// \date   August 2011
//-----------------------------------------------------------------------------

#include <iostream>
#include <sstream>
#include <cstring>
#include <vector>
#include <vnl/vnl_polynomial.h>
#include <vcl_compiler.h>

std::vector<vnl_polynomial<double> > stack;

vnl_polynomial<double> operation(char op) {
  if (op == '.') { return stack.back(); }
  vnl_polynomial<double> p = stack.size() ? stack.back() : 0.0;
  if (stack.size()) stack.pop_back();
  vnl_polynomial<double> p2 = stack.size() ? stack.back() : 0.0;
  if (op == '+') p += p2;
  else if (op == '-') p -= p2;
  else if (op == '*') p *= p2;
  else if (op == '/') p /= p2;
  else if (op == '%') p %= p2;
  else { std::cerr << "Unknown operator " << op << std::endl; return p; }
  if (stack.size()) stack.pop_back();
  return p;
}

vnl_polynomial<double> polynomial(char* txt) {
  std::vector<double> coef;
  std::stringstream ss(txt);
  double onecoef;
  while (ss >> onecoef) coef.insert(coef.begin(), 1, onecoef);
  while (coef.size() && coef.back() == 0.0) coef.pop_back(); // highest order coeff should not be zero!
  return vnl_polynomial<double>(coef);
}

int main()
{
  char l[65000];
  vnl_polynomial<double> p;
  while (std::cin.getline(l, 65000)) {
    int n = std::strlen(l) - 1;
    // strip trailing blanks:
    while (l[n] == ' ' || l[n] == '\t' || l[n] == '\r' || l[n] == '\n') l[n--] = '\0';
    if (n<0) continue;
    else if (l[n] >= '0' && l[n] <= '9') p=polynomial(l);
    else if (n == 0) p=operation(l[0]);
    else p=polynomial(l);
    stack.push_back(p);
    std::cout << p << std::endl;
  }
  return 0;
}
