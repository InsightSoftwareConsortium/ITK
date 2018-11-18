#include <vnl/vnl_complex.h>
#include <vnl/vnl_vector_ref.hxx>
VNL_VECTOR_REF_INSTANTIATE(std::complex<double>);
VNL_VECTOR_REF_INSTANTIATE(std::complex<float>);

VNL_VECTOR_REF_INSTANTIATE(long double);
VNL_VECTOR_REF_INSTANTIATE(double);
VNL_VECTOR_REF_INSTANTIATE(float);
VNL_VECTOR_REF_INSTANTIATE(signed   int);
VNL_VECTOR_REF_INSTANTIATE(unsigned int);
VNL_VECTOR_REF_INSTANTIATE(signed   short);
VNL_VECTOR_REF_INSTANTIATE(unsigned short);
VNL_VECTOR_REF_INSTANTIATE(char);
VNL_VECTOR_REF_INSTANTIATE(unsigned char);
VNL_VECTOR_REF_INSTANTIATE(signed   char);
VNL_VECTOR_REF_INSTANTIATE(unsigned long);
VNL_VECTOR_REF_INSTANTIATE(signed   long);
VNL_VECTOR_REF_INSTANTIATE(unsigned long long);
VNL_VECTOR_REF_INSTANTIATE(signed   long long);

#include <vnl/vnl_bignum.h>
#include <vnl/vnl_bignum_traits.h>
VNL_VECTOR_REF_INSTANTIATE(vnl_bignum);
#include <vnl/vnl_rational.h>
#include <vnl/vnl_rational_traits.h>
VNL_VECTOR_REF_INSTANTIATE(vnl_rational);
