#ifndef vnl_copy_h_
#define vnl_copy_h_
#ifdef __GNUC__
#pragma interface
#endif
// This is vxl/vnl/vnl_copy.h

//:  \file
//   \brief Easy conversion between vectors and matrices templated over different types.
//   \author fsm@robots.ox.ac.uk

//   Modifications
//   LSB (Manchester) 26/3/01 Tidied documentation

//: Easy conversion between vectors and matrices templated over different types.
template <class S, class T>
void vnl_copy(S const *src, T *dst, unsigned n);


//: Easy conversion between vectors and matrices templated over different types.
template <class S, class T>
void vnl_copy(S const &, T &);

#endif // vnl_copy_h_
