#include <iostream>
#include <cassert>
#include "vnl_discrete_diff.h"
#include "vnl/vnl_least_squares_function.h"

/*
  fsm
*/

bool
vnl_discrete_diff_fwd(vnl_least_squares_function * lsf, double h_, const vnl_vector<double> & x, vnl_matrix<double> & J)
{
  vnl_vector<double> y(lsf->get_number_of_residuals());
  lsf->f(x, y);
  if (lsf->failure)
    return false;
  vnl_vector<double> h(lsf->get_number_of_unknowns());
  h.fill(h_);
  return vnl_discrete_diff_fwd(lsf, h, x, y, J);
}

bool
vnl_discrete_diff_fwd(vnl_least_squares_function * lsf,
                      const vnl_vector<double> & h,
                      const vnl_vector<double> & x,
                      vnl_matrix<double> & J)
{
  vnl_vector<double> y(lsf->get_number_of_residuals());
  lsf->f(x, y);
  if (lsf->failure)
    return false;
  return vnl_discrete_diff_fwd(lsf, h, x, y, J);
}

bool
vnl_discrete_diff_fwd(vnl_least_squares_function * lsf,
                      const vnl_vector<double> & h,
                      const vnl_vector<double> & x,
                      const vnl_vector<double> & y,
                      vnl_matrix<double> & J)
{
  const unsigned m = J.rows();
  const unsigned n = J.columns();
  assert(m == lsf->get_number_of_residuals());
  assert(m == y.size());
  assert(n == lsf->get_number_of_unknowns());
  assert(n == h.size());
  assert(n == x.size());

  vnl_vector<double> tx(n);
  vnl_vector<double> ty(m);

  for (unsigned j = 0; j < n; j++)
  {
    tx = x;
    tx(j) += h(j);
    lsf->f(tx, ty);
    if (lsf->failure)
      return false;
    for (unsigned i = 0; i < m; i++)
      J(i, j) = (ty(i) - y(i)) / h(j);
  }
  return true;
}

bool
vnl_discrete_diff_sym(vnl_least_squares_function * lsf, double h_, const vnl_vector<double> & x, vnl_matrix<double> & J)
{
  vnl_vector<double> h(lsf->get_number_of_unknowns());
  h.fill(h_);
  return vnl_discrete_diff_sym(lsf, h, x, J);
}

bool
vnl_discrete_diff_sym(vnl_least_squares_function * lsf,
                      const vnl_vector<double> & h,
                      const vnl_vector<double> & x,
                      vnl_matrix<double> & J)
{
  const unsigned m = J.rows();
  const unsigned n = J.columns();
  assert(m == lsf->get_number_of_residuals());
  assert(n == lsf->get_number_of_unknowns());
  assert(n == h.size());
  assert(n == x.size());

  vnl_vector<double> xp(n);
  vnl_vector<double> xm(n);
  vnl_vector<double> yp(m);
  vnl_vector<double> ym(m);

  for (unsigned j = 0; j < n; j++)
  {
    xp = x;
    xp(j) += h(j);
    lsf->f(xp, yp);
    if (lsf->failure)
      return false;

    xm = x;
    xm(j) -= h(j);
    lsf->f(xm, ym);
    if (lsf->failure)
      return false;

    for (unsigned i = 0; i < m; i++)
      J(i, j) = (yp(i) - ym(i)) / (2 * h(j));
  }
  return true;
}

//----------------------------------------------------------------------

void
vnl_discrete_diff_test_lsf(vnl_least_squares_function * lsf, const vnl_vector<double> & x)
{
  const unsigned int m = lsf->get_number_of_residuals();
  const unsigned int n = lsf->get_number_of_unknowns();
  assert(x.size() == n);

  vnl_matrix<double> J1(m, n);
  lsf->gradf(x, J1);

  vnl_matrix<double> J2(m, n);
  vnl_discrete_diff_sym(lsf, 0.0001, x, J2);

  const double e = (J1 - J2).fro_norm();
  // assert(e <= 1e-3);
  const double t = cos_angle(J1, J2);
  // assert(t >= 0.99);

  std::cerr << __FILE__ ": e = " << e << std::endl << __FILE__ ": t = " << t << std::endl;
}
