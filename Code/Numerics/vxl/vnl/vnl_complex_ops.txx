/*
  fsm@robots.ox.ac.uk
*/
#include "vnl_complex_ops.h"

//--------------------------------------------------------------------------------

template <class T>
void vnl_complexify(T const *src, vnl_complex<T> *dst, unsigned n) {
  for (unsigned i=0; i<n; ++i)
    dst[i] = src[i];
}

template <class T>
void vnl_complexify(T const *re, T const *im, vnl_complex<T> *dst, unsigned n) {
  for (unsigned i=0; i<n; ++i)
    dst[i] = vnl_complex<T>(re[i], im[i]);
}

template <class T> 
vnl_vector<vnl_complex<T> > vnl_complexify(vnl_vector<T> const &v) {
  vnl_vector<vnl_complex<T> > vc(v.size());
  vnl_complexify(v.begin(), vc.begin(), v.size());
  return vc;
}

template <class T>
vnl_matrix<vnl_complex<T> > vnl_complexify(vnl_matrix<T> const &M) {
  vnl_matrix<vnl_complex<T> > Mc(M.rows(), M.cols());
  vnl_complexify(M.begin(), Mc.begin(), M.size());
  return Mc;
}


// -- Vector of absolute values of vnl_complex_vector_t<REAL>.
template <class T>
vnl_vector<T> abs(vnl_vector<vnl_complex<T> > const & C)
{
  vnl_vector<T> ret(C.size());
  for (unsigned i = 0; i < C.size(); ++i)
    ret[i] = vcl_abs(C[i]);
  return ret;
}


// -- Vector of angles of vnl_vector<vnl_complex<T> >.
// atan2(imag(C), real(C))
template <class T>
vnl_vector<T> angle(vnl_vector<vnl_complex<T> > const & C)
{
  vnl_vector<T> ret(C.size());
  for (unsigned i = 0; i < C.size(); ++i)
    ret[i] = atan2(C[i].imag(), C[i].real());
  return ret;
}

// -- Vector of real parts of vnl_vector<vnl_complex<T> >.
template <class T>
vnl_vector<T> real(vnl_vector<vnl_complex<T> > const & C)
{
  vnl_vector<T> ret(C.size());
  for (unsigned i = 0; i < C.size(); ++i)
    ret[i] = C[i].real();
  return ret;
}

// -- Vector of imaginary parts of vnl_vector<vnl_complex<T> >.
template <class T>
vnl_vector<T> imag(vnl_vector<vnl_complex<T> > const & C)
{
  vnl_vector<T> ret(C.size());
  for (unsigned i = 0; i < C.size(); ++i)
    ret[i] = C[i].imag();
  return ret;
}

//--------------------------------------------------------------------------------

// -- Matrix of absolute values of vnl_complex_matrix_t<REAL>.
template <class T>
vnl_matrix<T> abs(vnl_matrix<vnl_complex<T> > const& C)
{
  vnl_matrix<T> ret(C.rows(), C.columns());
  for(unsigned i = 0; i < C.rows(); ++i)
    for(unsigned j = 0; j < C.columns(); ++j)
      ret(i,j) = vcl_abs(C(i,j));
  return ret;
}


// -- Matrix of angles of vnl_complex_matrix_t<T>.
// atan2(imag(C), real(C))
template <class T>
vnl_matrix<T> angle(vnl_matrix<vnl_complex<T> > const& C)
{
  vnl_matrix<T> ret(C.rows(), C.columns());
  for(unsigned i = 0; i < C.rows(); ++i)
    for(unsigned j = 0; j < C.columns(); ++j)
      ret(i,j) = atan2(C(i,j).imag(), C(i,j).real());
  return ret;
}

// -- Matrix of real parts of vnl_complex_matrix_t<T>.
template <class T>
vnl_matrix<T> real(vnl_matrix<vnl_complex<T> > const& C)
{
  vnl_matrix<T> ret(C.rows(), C.columns());
  for(unsigned i = 0; i < C.rows(); ++i)
    for(unsigned j = 0; j < C.columns(); ++j)
      ret(i,j) = C(i,j).real();
  return ret;
}

// -- Matrix of imaginary parts of vnl_complex_matrix_t<T>.
template <class T>
vnl_matrix<T> imag(vnl_matrix<vnl_complex<T> > const& C)
{
  vnl_matrix<T> ret(C.rows(), C.columns());
  for(unsigned i = 0; i < C.rows(); ++i)
    for(unsigned j = 0; j < C.columns(); ++j)
      ret(i,j) = C(i,j).imag();
  return ret;
}

//--------------------------------------------------------------------------------

#define VNL_COMPLEX_OPS_INSTANTIATE(T) \
template void vnl_complexify(T const *, vnl_complex<T > *, unsigned); \
template void vnl_complexify(T const *, T const *, vnl_complex<T > *, unsigned); \
\
template vnl_vector<vnl_complex<T > > vnl_complexify(vnl_vector<T > const &); \
template vnl_matrix<vnl_complex<T > > vnl_complexify(vnl_matrix<T > const &); \
\
template vnl_vector<T> imag (vnl_vector<vnl_complex<T> > const &); \
template vnl_vector<T> angle(vnl_vector<vnl_complex<T> > const &); \
template vnl_vector<T> abs  (vnl_vector<vnl_complex<T> > const &); \
template vnl_vector<T> real (vnl_vector<vnl_complex<T> > const &); \
\
template vnl_matrix<T> imag (vnl_matrix<vnl_complex<T> > const &); \
template vnl_matrix<T> angle(vnl_matrix<vnl_complex<T> > const &); \
template vnl_matrix<T> abs  (vnl_matrix<vnl_complex<T> > const &); \
template vnl_matrix<T> real (vnl_matrix<vnl_complex<T> > const &)
