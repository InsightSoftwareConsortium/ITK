#include <vnl/vnl_sparse_matrix.txx>

template class vnl_sparse_matrix<double>;

static double vnl_sparse_matrix_double_tickler()
{
  vnl_sparse_matrix<double> md(3, 5);
  vnl_sparse_matrix_double_tickler(); // to avoid compiler warning
  return md(0,0);
}
