#ifndef vnl_copy_h_
#define vnl_copy_h_
#ifdef __GNUC__
#pragma interface
#endif
//
// .NAME vnl_copy
// .HEADER vxl package
// .LIBRARY vnl
// .INCLUDE vnl/vnl_copy.h
// .FILE vnl_copy.cxx
// .SECTION Author
//  fsm@robots.ox.ac.uk
//

// purpose: easy conversion between vectors and matrices templated
// over different types.

template <class S, class T>
void vnl_copy(S const *src, T *dst, unsigned n);

template <class S, class T>
void vnl_copy(S const &, T &);

#endif // vnl_copy_h_
