// This is core/vnl/vnl_fwd.h
#ifndef vnl_fwd_h_
#define vnl_fwd_h_

//:
// \file
// \author fsm
#include <vcl_compiler.h> // required to check for VCL_VC60

template <class T> struct vnl_complex_traits;
template <class T> class vnl_numeric_traits;
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
// Could not make #including vec_fixed_ref.h and matrix_fixed_ref.h work under VC6 - compiler error
// Just have to leave them out of vnl_fwd.h
#else
  template <class T, unsigned int n> class vnl_vector_fixed;
  template <class T, unsigned int num_rows, unsigned int num_cols> class vnl_matrix_fixed;
  template <class T, unsigned int n> class vnl_vector_fixed_ref;
  template <class T, unsigned int num_rows, unsigned int num_cols> class vnl_matrix_fixed_ref;
  template <class T, unsigned int n> class vnl_vector_fixed_ref_const;
  template <class T, unsigned int num_rows, unsigned int num_cols> class vnl_matrix_fixed_ref_const;
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
class vnl_double_2x3;
class vnl_double_3x2;
typedef vnl_matrix_fixed<double,1,1> vnl_double_1x1;
typedef vnl_matrix_fixed<double,1,2> vnl_double_1x2;
typedef vnl_matrix_fixed<double,2,1> vnl_double_2x1;
typedef vnl_matrix_fixed<double,2,2> vnl_double_2x2;
typedef vnl_matrix_fixed<double,1,3> vnl_double_1x3;
typedef vnl_matrix_fixed<double,3,1> vnl_double_3x1;
typedef vnl_matrix_fixed<double,3,3> vnl_double_3x3;
typedef vnl_matrix_fixed<double,3,4> vnl_double_3x4;
typedef vnl_matrix_fixed<double,4,3> vnl_double_4x3;
typedef vnl_matrix_fixed<double,4,4> vnl_double_4x4;
class vnl_float_2;
class vnl_float_3;
class vnl_float_4;
typedef vnl_matrix_fixed<float,1,2> vnl_float_1x2;
typedef vnl_matrix_fixed<float,2,1> vnl_float_2x1;
typedef vnl_matrix_fixed<float,2,2> vnl_float_2x2;
typedef vnl_matrix_fixed<float,1,3> vnl_float_1x3;
typedef vnl_matrix_fixed<float,3,1> vnl_float_3x1;
typedef vnl_matrix_fixed<float,3,3> vnl_float_3x3;
typedef vnl_matrix_fixed<float,3,4> vnl_float_3x4;
typedef vnl_matrix_fixed<float,4,3> vnl_float_4x3;
typedef vnl_matrix_fixed<float,4,4> vnl_float_4x4;
class vnl_int_2;
class vnl_int_3;
class vnl_int_4;
typedef vnl_matrix_fixed<int,2,2> vnl_int_2x2;
struct vnl_identity_3x3;
class vnl_least_squares_cost_function;
class vnl_least_squares_function;
class vnl_matlab_readhdr;
class vnl_nonlinear_minimizer;

#endif // vnl_fwd_h_
