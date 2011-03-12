#include <vnl/vnl_rational.h>
#include <vnl/vnl_rational_traits.h>
#include <vnl/vnl_sparse_matrix.txx>

template class vnl_sparse_matrix<vnl_rational>;

#if 0 // Clang compiler give a warning about unused variable.
// I'm not sure if the tickler is needed any more?
// This seems to ba a trade off between warning messages
// of different compilers.
static vnl_rational vnl_sparse_matrix_vnl_rational_tickler()
{
  vnl_sparse_matrix<vnl_rational> md(3, 5);
  vnl_sparse_matrix_vnl_rational_tickler(); // to avoid compiler warning
  return md(0,0);
}
#endif
