#include <vcl_vector.txx>
#include <vcl_algorithm.txx>
#include <vnl/vnl_sparse_matrix.txx>

template class vnl_sparse_matrix<float>;

typedef vnl_sparse_matrix_pair<float> spmpf;

VCL_VECTOR_INSTANTIATE(spmpf);
VCL_VECTOR_INSTANTIATE(vcl_vector<spmpf>);

typedef vnl_sparse_matrix_pair<float>::less spmpf_less;

VCL_SORT_INSTANTIATE_CMP(vcl_vector<spmpf>::iterator, spmpf, spmpf_less);

static void vnl_sparse_matrix_instances_tickler()
{
  vnl_sparse_matrix<float> mf(3, 5);
}
