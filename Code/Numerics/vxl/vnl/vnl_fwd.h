#ifndef vnl_fwd_h_
#define vnl_fwd_h_
#ifdef __GNUC__
#pragma interface
#endif
// This is vxl/vnl/vnl_fwd.h

/*
  fsm@robots.ox.ac.uk
*/

template <class T> class vnl_complex_traits;
template <class T> class vnl_numeric_traits;
template <class T> class vnl_numeric_limits;
template <class T> class vnl_c_vector;

template <class T> class vnl_vector;
template <class T> class vnl_vector_ref;
template <class T, int n> class vnl_vector_fixed;
template <class T, int n> class vnl_vector_fixed_ref;

template <class T> class vnl_matrix;
template <class T> class vnl_matrix_ref;
template <class T, int m, int n> class vnl_matrix_fixed;
template <class T, int m, int n> class vnl_matrix_fixed_ref;

template <class T> class vnl_quaternion;
template <class Return, class Argument> class vnl_unary_function;
template <class T> class vnl_diag_matrix;
template <class T> class vnl_fortran_copy;
template <class T> class vnl_identity;

class vnl_cost_function;
class vnl_cross_product_matrix;
class vnl_double_2;
class vnl_double_3;
class vnl_float_2;
class vnl_float_3;
struct vnl_identity_3x3;
class vnl_least_squares_cost_function;
class vnl_least_squares_function;
class vnl_matlab_readhdr;
class vnl_nonlinear_minimizer;

#endif // vnl_fwd_h_
