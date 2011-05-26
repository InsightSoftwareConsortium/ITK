#include <vnl/vnl_sparse_matrix.txx>

template class vnl_sparse_matrix<int>;

static int vnl_sparse_matrix_float_tickler()
{
  vnl_sparse_matrix<int> md(3, 5);
  vnl_sparse_matrix_float_tickler(); // to avoid compiler warning
  return md(0,0);
}
