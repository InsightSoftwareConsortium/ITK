#ifndef vnl_T_n_h_
#define vnl_T_n_h_
#ifdef __GNUC__
#pragma interface
#endif
/*
  fsm@robots.ox.ac.uk
*/
#include <vnl/vnl_vector_fixed.h>
// Purpose: the vnl_vector_fixed<T,n> template class provides 
// non-mallocing vectors of given size, but it has no constructor
// from n Ts. This macro declares a class vnl_T_n which is derived
// directly from vnl_vector_fixed<T,n> and has such a constructor.
//
// Inspired by the numerous double-3, int-2 things lying around.

// cpp traits!
#define vnl_T_n_aux_1(T) (T x) { data[0] = x; }
#define vnl_T_n_aux_2(T) (T x, T y) { data[0] = x; data[1] = y; }
#define vnl_T_n_aux_3(T) (T x, T y, T z) { data[0] = x; data[1] = y; data[2] = z; }
#define vnl_T_n_aux_4(T) (T x, T y, T z, T w) { data[0] = x; data[1] = y; data[2] = z; data[3] = w; }

//: this macro defines the class.
// e.g. use vnl_T_n_impl(int,2) to implement class vnl_int_2.
#define vnl_T_n_class_impl(T,n) \
class vnl_##T##_##n : public vnl_vector_fixed<T ,n> { \
public: \
  vnl_##T##_##n() { } \
  vnl_##T##_##n(vnl_vector<T > const & rhs) : vnl_vector_fixed<T ,n>(rhs) { } \
  vnl_##T##_##n(vnl_vector_fixed<T ,n> const & rhs) : vnl_vector_fixed<T ,n>(rhs) { } \
  vnl_##T##_##n vnl_T_n_aux_##n(T) \
};

//: some compilers need a bit of help with the overload resolution.
#if defined(VCL_GCC_27)
# define vnl_T_n_funcs_impl(T,n) \
inline bool operator==(vnl_##T##_##n const & a, vnl_##T##_##n const & b) \
{ return (vnl_vector<T > const &)a == (vnl_vector<T > const &)b; } \
 \
inline vcl_ostream & operator<<(vcl_ostream & s, vnl_##T##_##n const & v) \
{ return s << (vnl_vector<T > const&)v; }
#else
# define vnl_T_n_funcs_impl(T,n) /* no need */
#endif

//: clients use this.
#define vnl_T_n_impl(T,n) \
vnl_T_n_class_impl(T,n) \
vnl_T_n_funcs_impl(T,n)

#endif // vnl_T_n_h_
