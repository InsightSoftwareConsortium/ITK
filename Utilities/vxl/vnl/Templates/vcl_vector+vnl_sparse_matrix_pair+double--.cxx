#include <vnl/vnl_sparse_matrix.h>
#include <vcl_vector.txx>

VCL_VECTOR_INSTANTIATE(vnl_sparse_matrix_pair<double>);

#include <vcl_algorithm.txx>

typedef vnl_sparse_matrix_pair<double> spmpd;
VCL_SORT_INSTANTIATE_CMP(vcl_vector<spmpd>::iterator, spmpd, spmpd::less);
