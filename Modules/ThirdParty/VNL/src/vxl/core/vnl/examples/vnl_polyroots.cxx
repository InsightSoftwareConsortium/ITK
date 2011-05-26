// This is core/vnl/examples/vnl_polyroots.cxx

//:
// \file
// \brief Find all roots of a polynomial
// \author Peter Vanroose, KULeuven, ESAT/PSI.
// \date   February 2000
//-----------------------------------------------------------------------------

#include <vcl_iostream.h>
#include <vcl_cstdlib.h> // for atof()
#include <vnl/vnl_vector.h>
#include <vnl/algo/vnl_rpoly_roots.h>

int main(int argc, char* argv[])
{
  --argc; ++argv;

  // Read coefficients from stdin, or from command line
  vnl_vector<double> pts(argc);
  if (argc == 0) {
    vcl_cout << "Give the polynomial coefficients, and end with EOF (CTRL-Z)\n";
    vcl_cin >> pts;
  }
  else
    for (int i=0; i<argc; ++i)
      pts[i] = vcl_atof(argv[i]);

  vcl_cout << "Coefficients = [ " << pts << " ]\n"
           << "Polynomial = ";
  for (unsigned i=0; i+2<pts.size(); ++i) if (pts[i] != 0)
    vcl_cout << pts[i] << " X^" << pts.size()-i-1 << " + ";
  vcl_cout << pts[pts.size()-2] << " X + " << pts[pts.size()-1] << vcl_endl;

  vnl_rpoly_roots r(pts);

  vcl_cout << "Roots = [ " << r.roots() << " ]\n";
  return 0;
}
