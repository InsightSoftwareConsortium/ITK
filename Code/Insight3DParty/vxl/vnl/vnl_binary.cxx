/*
  fsm@robots.ox.ac.uk
*/
#ifdef __GNUC__
#pragma implementation
#endif
#include "vnl_binary.h"

#include <vcl_iostream.h>
#include <vcl_new.h>

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_diag_matrix.h>
#include <vnl/vnl_resize.h>

// KAI C++ wants char*, so it must be correct....
#define stream_cast (char*)

// -------------------- vnl_vector

template <class T>
bool vnl_binary_save(vcl_ostream &f, vnl_vector<T> const &v)
{
  int tmp = v.size();
  f.write(stream_cast &tmp, sizeof(tmp));
  if(v.data_block())
    f.write(stream_cast v.data_block(), v.size() * sizeof(T));
  else
    ; // FIXME: vector  has been default-constructed.
  return true;
}

template <class T>
bool vnl_binary_load(vcl_istream &f, vnl_vector<T> &v)
{
  int n = -1;
  f.read(stream_cast &n, sizeof(n));
  if (n > 0) {
    vnl_resize(v, n);
    f.read(stream_cast v.data_block(), v.size() * sizeof(T));
  }
  // FIXME
  else {
    // egcs barfs at v.~vnl_vector()
    // SunPro is merely braindead.
#if defined(VCL_SUNPRO_CC_50)
    vnl_c_vector<T>::deallocate(v.begin(), v.size());
#elif defined(VCL_EGCS)
    (&v)->~vnl_vector();
#else
    v.~vnl_vector();
#endif
    new (&v) vnl_vector<T>;
  }
  return true;
}

// -------------------- vnl_matrix

template <class T>
bool vnl_binary_save(vcl_ostream &f, vnl_matrix<T> const &A)
{
  int tmp;
  tmp = A.rows(); f.write(stream_cast &tmp, sizeof(tmp));
  tmp = A.cols(); f.write(stream_cast &tmp, sizeof(tmp));
  if (A.data_array())
    f.write(stream_cast A.data_block(), A.size() * sizeof(T));
  else
    ; // FIXME: matrix has been default-constructed.
  return true;
}

template <class T>
bool vnl_binary_load(vcl_istream &f, vnl_matrix<T> &A)
{
  int r = -1, c = -1;
  f.read(stream_cast &r, sizeof(r));
  f.read(stream_cast &c, sizeof(c));
  //vcl_cerr << "r c = " << r << ' ' << c << vcl_endl;
  if (r > 0 || c > 0) {
    vnl_resize(A, r, c);
    f.read(stream_cast A.data_block(), A.size() * sizeof(T));
  }
  // FIXME
  else {
#if defined(VCL_SUNPRO_CC_50)
    assert(!"get a proper compiler");
#elif defined(VCL_EGCS)
    (&A)->~vnl_matrix();
#else
    A.~vnl_matrix();
#endif
    new (&A) vnl_matrix<T>;
    return false;
  }
  return true;
}

// -------------------- vnl_diag_matrix

template <class T>
bool vnl_binary_save(vcl_ostream &f, vnl_diag_matrix<T> const &D)
{
  int tmp = D.size();
  f.write(stream_cast &tmp, sizeof(tmp));
  if (D.data_block())
    f.write(stream_cast D.data_block(), D.size() * sizeof(T));
  else
    ; // FIXME: matrix has been default-constructed.
  return true;
}

template <class T>
bool vnl_binary_load(vcl_istream &f, vnl_diag_matrix<T> &D)
{
  int n = -1;
  f.read(stream_cast &n, sizeof(n));
  if (n > 0) {
    vnl_resize(D, n);
    f.read(stream_cast D.data_block(), D.size() * sizeof(T));
  }
  // FIXME
  else {
#if defined(VCL_SUNPRO_CC_50)
    assert(!"get a proper compiler");
#elif defined(VCL_EGCS)
    (&D)->~vnl_diag_matrix();
#else
    D.~vnl_diag_matrix();
#endif
    new (&D) vnl_diag_matrix<T>;
  }
  return true;
}

//------------------------------------------------------------

#define inst(T) \
template bool vnl_binary_save(vcl_ostream &, vnl_vector<T > const &); \
template bool vnl_binary_save(vcl_ostream &, vnl_matrix<T > const &); \
template bool vnl_binary_save(vcl_ostream &, vnl_diag_matrix<T > const &); \
template bool vnl_binary_load(vcl_istream &, vnl_vector<T > &); \
template bool vnl_binary_load(vcl_istream &, vnl_matrix<T > &); \
template bool vnl_binary_load(vcl_istream &, vnl_diag_matrix<T > &); \
;

inst(int);
inst(float);
inst(double);
