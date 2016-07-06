#include <vnl/vnl_bignum.h>
#include <vnl/vnl_bignum_traits.h>
#include <vnl/vnl_sparse_matrix.hxx>

template class VNL_EXPORT vnl_sparse_matrix<vnl_bignum>;

static vnl_bignum vnl_sparse_matrix_vnl_bignum_tickler()
{
  vnl_sparse_matrix<vnl_bignum> md(3, 5);
  vnl_sparse_matrix_vnl_bignum_tickler(); // to avoid compiler warning
  return md(0,0);
}
