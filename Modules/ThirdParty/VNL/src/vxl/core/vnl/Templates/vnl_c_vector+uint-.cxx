// Disable warning
#ifdef _MSC_VER
// 4146: unary minus operator applied to unsigned type, result still unsigned
# pragma warning(disable:4146)
#endif //_MSC_VER
#include <vnl/vnl_c_vector.hxx>
VNL_C_VECTOR_INSTANTIATE_ordered(unsigned int);
