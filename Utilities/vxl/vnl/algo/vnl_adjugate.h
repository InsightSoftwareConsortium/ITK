// This is vxl/vnl/algo/vnl_adjugate.h
#ifndef vnl_adjugate_h_
#define vnl_adjugate_h_
//:
// \file
// \author fsm

template <class T> class vnl_matrix;

// By definition, the product of A and its adjugate
// is det(A) [times an identity matrix].

template <class T>
void vnl_adjugate(vnl_matrix<T> const &A, vnl_matrix<T> *out);

template <class T>
vnl_matrix<T> vnl_adjugate(vnl_matrix<T> const &A);

#endif // vnl_adjugate_h_
