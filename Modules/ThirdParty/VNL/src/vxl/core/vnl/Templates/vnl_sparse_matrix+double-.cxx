#include <vnl/vnl_sparse_matrix.txx>

template class vnl_sparse_matrix<double>;

#if 0 // Clang compiler give a warning about unused variable.
// I'm not sure if the tickler is needed any more?
// This seems to ba a trade off between warning messages
// of different compilers.
static double vnl_sparse_matrix_double_tickler()
{
  vnl_sparse_matrix<double> md(3, 5);
  vnl_sparse_matrix_double_tickler(); // to avoid compiler warning
  return md(0,0);
}
#endif
