// This is core/vnl/examples/vnl_polyroots.cxx

//:
// \file
// \brief Find all roots of a polynomial
// \author Peter Vanroose, KULeuven, ESAT/PSI.
// \date   February 2000
//-----------------------------------------------------------------------------

#include <iostream>
#include <cstdlib>
#include <vcl_compiler.h>
#include <vnl/vnl_vector.h>
#include <vnl/algo/vnl_rpoly_roots.h>

int main(int argc, char* argv[])
{
  --argc; ++argv;

  // Read coefficients from stdin, or from command line
  vnl_vector<double> pts(argc);
  if (argc == 0) {
    std::cout << "Give the polynomial coefficients, and end with EOF (CTRL-Z)\n";
    std::cin >> pts;
  }
  else
    for (int i=0; i<argc; ++i)
      pts[i] = std::atof(argv[i]);

  std::cout << "Coefficients = [ " << pts << " ]\n"
           << "Polynomial = ";
  for (unsigned i=0; i+2<pts.size(); ++i) if (pts[i] != 0)
    std::cout << pts[i] << " X^" << pts.size()-i-1 << " + ";
  std::cout << pts[pts.size()-2] << " X + " << pts[pts.size()-1] << std::endl;

  vnl_rpoly_roots r(pts);

  std::cout << "Roots = [ " << r.roots() << " ]\n";
  return 0;
}
