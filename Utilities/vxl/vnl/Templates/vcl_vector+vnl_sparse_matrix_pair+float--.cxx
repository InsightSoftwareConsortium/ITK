#include <vnl/vnl_sparse_matrix.h>
#include <vcl_vector.txx>

VCL_VECTOR_INSTANTIATE(vnl_sparse_matrix_pair<float>);

#include <vcl_algorithm.txx>

typedef vnl_sparse_matrix_pair<float> spmpf;
VCL_SORT_INSTANTIATE_CMP(vcl_vector<spmpf>::iterator, spmpf, spmpf::less);
