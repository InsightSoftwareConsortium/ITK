#include <vcl_vector.txx>
#include <vcl_algorithm.txx>
#include <vnl/vnl_sparse_matrix.txx>

template class vnl_sparse_matrix<double>;

typedef vnl_sparse_matrix_pair<double> spmpd;

VCL_VECTOR_INSTANTIATE(spmpd);
VCL_VECTOR_INSTANTIATE(vcl_vector<spmpd>);

typedef vnl_sparse_matrix_pair<double>::less spmpd_less;

VCL_SORT_INSTANTIATE_CMP(vcl_vector<spmpd>::iterator, spmpd, spmpd_less);

static void vnl_sparse_matrix_instances_tickler()
{
  vnl_sparse_matrix<double> md(3, 5);
}
