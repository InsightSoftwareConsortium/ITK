#ifndef vnl_adjugate_h_
#define vnl_adjugate_h_
#ifdef __GNUC__
#pragma interface
#endif
//:
// \file
// \author fsm@robots.ox.ac.uk

template <class T> class vnl_matrix;

// By definition, the product of A and its adjugate
// is det(A) [times an identity matrix].

template <class T>
void vnl_adjugate(vnl_matrix<T> const &A, vnl_matrix<T> *out);

template <class T>
vnl_matrix<T> vnl_adjugate(vnl_matrix<T> const &A);

#endif // vnl_adjugate_h_
