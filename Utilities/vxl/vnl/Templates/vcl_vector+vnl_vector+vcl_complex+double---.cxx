#include <vcl_complex.h>
#include <vnl/vnl_vector.h>
#include <vcl_vector.txx>

VCL_VECTOR_INSTANTIATE(vnl_vector<vcl_complex<double> >);

#include <vcl_algorithm.txx>
VCL_CONTAINABLE_INSTANTIATE(vnl_vector<vcl_complex<double> >);
