// This is vxl/vnl/vnl_fwd.h
#ifndef vnl_fwd_h_
#define vnl_fwd_h_

//:
// \file
// \author fsm

template <class T> struct vnl_complex_traits;
template <class T> class vnl_numeric_traits;
template <class T> class vnl_numeric_limits;
template <class T> class vnl_c_vector;

template <class T> class vnl_vector;
template <class T> class vnl_vector_ref;

template <class T> class vnl_matrix;
template <class T> class vnl_matrix_ref;

#ifdef VCL_VC60
// VC 6.0 can't forward declare these without barfing.
// Thereby missing the whole point....
# include <vnl/vnl_vector_fixed.h>
# include <vnl/vnl_matrix_fixed.h>
#else
  template <class T, unsigned int n> class vnl_vector_fixed;
  template <class T, unsigned int num_rows, unsigned int num_cols> class vnl_matrix_fixed;
#endif

template <class T> class vnl_quaternion;
template <class Return, class Argument> class vnl_unary_function;
template <class T> class vnl_diag_matrix;
template <class T> class vnl_fortran_copy;
template <class T> class vnl_identity;

class vnl_cost_function;
class vnl_cross_product_matrix;
class vnl_double_2;
class vnl_double_3;
class vnl_double_4;
class vnl_double_5;
class vnl_double_2x3;
class vnl_double_3x2;
typedef vnl_matrix_fixed<double,2,2> vnl_double_2x2;
typedef vnl_matrix_fixed<double,3,3> vnl_double_3x3;
typedef vnl_matrix_fixed<double,4,4> vnl_double_4x4;
typedef vnl_matrix_fixed<double,3,4> vnl_double_3x4;
typedef vnl_matrix_fixed<double,4,3> vnl_double_4x3;
class vnl_float_2;
class vnl_float_3;
class vnl_float_4;
class vnl_int_2;
class vnl_int_3;
class vnl_int_4;
struct vnl_identity_3x3;
class vnl_least_squares_cost_function;
class vnl_least_squares_function;
class vnl_matlab_readhdr;
class vnl_nonlinear_minimizer;

#endif // vnl_fwd_h_
