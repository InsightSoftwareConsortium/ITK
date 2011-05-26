#if (defined _MSC_VER) && _MSC_VER == 1200
// disable warning buried deep in the standard library
// warning C4018: '<' : signed/unsigned mismatch: vector(159)
# pragma warning(disable: 4018)
#endif

#include <vnl/vnl_complex.h> // for vnl_math_isfinite(complex)
#include <vnl/vnl_vector.txx>

VNL_VECTOR_INSTANTIATE_COMPLEX(vcl_complex<long double>);
