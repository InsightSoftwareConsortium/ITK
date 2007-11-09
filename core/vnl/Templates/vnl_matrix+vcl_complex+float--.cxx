#if (defined _MSC_VER) && _MSC_VER == 1200
// disable warning buried deep in the standard library
// warning C4018: '<' : signed/unsigned mismatch: vector(159)
# pragma warning(disable: 4018)
#endif

#include <vnl/vnl_complex.h>
#include <vnl/vnl_matrix.txx>

VNL_MATRIX_INSTANTIATE(vcl_complex<float>);
