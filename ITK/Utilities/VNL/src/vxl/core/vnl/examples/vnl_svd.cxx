// Solve LS problem M x = B, warning if M is nearly singular.
// Peter Vanroose, February 2000

#include <vcl_iostream.h>
#include <vnl/algo/vnl_svd.h>

template <class D>  // D is often double or float
vnl_matrix<D> solve_with_warning(vnl_matrix<D>const& M,
                                 vnl_matrix<D>const& B)
{
  // Take svd of vnl_matrix<D> M, setting singular values
  // smaller than 1e-8 to 0, and hold the result.
  vnl_svd<D> svd(M, 1e-8);
  // Check for rank-deficiency
  if (svd.singularities() > 1)
    vcl_cerr << "Warning: Singular matrix, condition = " << svd.well_condition() << vcl_endl;
  return svd.solve(B);
}

template vnl_matrix<double> solve_with_warning(vnl_matrix<double>const&,vnl_matrix<double>const&);

int main()
{
  double data[] = { 1, 1, 1,  1, 2, 3,  1, 3, 6};
  vnl_matrix<double> M (data, 3, 3);
  vnl_matrix<double> B (3, 1, 7.0); // column vector [7 7 7]^T
  vnl_matrix<double> result = solve_with_warning(M,B);
  vcl_cerr << result << vcl_endl;
  M(2,2)=5; result = solve_with_warning(M,B);
  vcl_cerr << result << vcl_endl;
  return 0;
}
