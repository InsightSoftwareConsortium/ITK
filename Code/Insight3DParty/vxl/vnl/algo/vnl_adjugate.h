#ifndef vnl_adjugate_h_
#define vnl_adjugate_h_
#ifdef __GNUC__
#pragma interface
#endif
// .NAME    vnl_adjugate
// .HEADER  vxl package
// .LIBRARY vnl-algo
// .INCLUDE vnl/algo/vnl_adjugate.h
// .FILE    vnl_adjugate.txx
// .SECTION Author
//  fsm@robots.ox.ac.uk

template <class T> class vnl_matrix;

// By definition, the product of A and its adjugate
// is det(A) [times an identity matrix].

template <class T>
void vnl_adjugate(vnl_matrix<T> const &A, vnl_matrix<T> *out);

template <class T>
vnl_matrix<T> vnl_adjugate(vnl_matrix<T> const &A);

#endif // vnl_adjugate_h_
