#include <vnl/vnl_matrix_fixed.txx>
template class vnl_matrix_fixed<double,2,2>;
#if defined(VCL_GCC_27)
template ostream& operator<<(ostream&,vnl_matrix_fixed<double,2,2> const&);
typedef vnl_matrix_fixed<double,2,2> T;
VCL_INSTANTIATE_INLINE( T operator*(double const &, T const &) );
#endif
