// This is core/vnl/vnl_T_n.h
#ifndef vnl_T_n_h_
#define vnl_T_n_h_
//:
// \file
// \author fsm
//
// Purpose: the vnl_vector_fixed<T,n> template class provides
// non-mallocing vectors of given size, but it has no constructor
// from n Ts. This macro declares a class vnl_T_n which is derived
// directly from vnl_vector_fixed<T,n> and has such a constructor.
//
// Inspired by the numerous double-3, int-2 things lying around.

#include <vnl/vnl_vector_fixed.h>

//: cpp traits!
#define vnl_T_n_aux_1(T) (T x) { data_[0] = x; }
#define vnl_T_n_aux_2(T) (T x, T y) { data_[0] = x; data_[1] = y; }
#define vnl_T_n_aux_3(T) (T x, T y, T z) { data_[0] = x; data_[1] = y; data_[2] = z; }
#define vnl_T_n_aux_4(T) (T x, T y, T z, T w) { data_[0] = x; data_[1] = y; data_[2] = z; data_[3] = w; }
#define vnl_T_n_aux_5(T) (T x, T y, T z, T w, T u) { data_[0] = x; data_[1] = y; data_[2] = z; data_[3] = w; data_[4]= u; }

//: this macro defines the class.
// e.g. use vnl_T_n_impl(int,2) to implement class vnl_int_2.
#define vnl_T_n_class_impl(T,n) \
class vnl_##T##_##n : public vnl_vector_fixed<T ,n> \
{ \
 public: \
  vnl_##T##_##n() { } \
  vnl_##T##_##n(vnl_vector<T > const & rhs) : vnl_vector_fixed<T ,n>(rhs) { } \
  vnl_##T##_##n(vnl_vector_fixed<T ,n> const & rhs) : vnl_vector_fixed<T ,n>(rhs) { } \
  vnl_##T##_##n vnl_T_n_aux_##n(T) \
};

// some compilers need a bit of help with the overload resolution.
#define vnl_T_n_funcs_impl(T,n) /* no need */

//: clients use this.
#define vnl_T_n_impl(T,n) \
vnl_T_n_class_impl(T,n) \
vnl_T_n_funcs_impl(T,n)

#endif // vnl_T_n_h_
