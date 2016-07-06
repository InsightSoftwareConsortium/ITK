#include <vnl/vnl_sparse_matrix.hxx>

template class VNL_EXPORT vnl_sparse_matrix<float>;

static float vnl_sparse_matrix_float_tickler()
{
  vnl_sparse_matrix<float> md(3, 5);
  vnl_sparse_matrix_float_tickler(); // to avoid compiler warning
  return md(0,0);
}
