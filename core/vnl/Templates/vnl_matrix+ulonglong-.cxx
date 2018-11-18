//long long - target type will have width of at least 64 bits. (since C++11)
// Disable warning
#ifdef _MSC_VER
// 4146: unary minus operator applied to unsigned type, result still unsigned
# pragma warning(disable:4146)
#endif //_MSC_VER

#include <vnl/vnl_matrix.hxx>
VNL_MATRIX_INSTANTIATE(unsigned long long);
