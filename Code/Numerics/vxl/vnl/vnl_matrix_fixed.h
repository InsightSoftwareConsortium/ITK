#ifndef vnl_matrix_fixed_h_
#define vnl_matrix_fixed_h_
#ifdef __GNUC__
#pragma interface
#endif
// .NAME	vnl_matrix_fixed<T,m,n> - Fixed size matrix
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_matrix_fixed.h
// .FILE	vnl_matrix_fixed.txx
//
// .SECTION Description
//    vnl_matrix_fixed<T,m,n> - Fixed size matrix.  A subclass of vnl_matrix_fixed_ref,
//    all storage is local and all vnl_matrix operations are valid.
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 04 Aug 96
//
// .SECTION Modifications:
//     Peter Vanroose, 23 Nov 1996:  added explicit copy constructor
//
//-----------------------------------------------------------------------------

#include <vcl_cassert.h>
#include <vnl/vnl_matrix_fixed_ref.h>

template <class T, int m, int n>
class vnl_matrix_fixed : public vnl_matrix_fixed_ref<T,m,n> {
  T space[m*n]; // Local storage
public:

// -- Construct an empty m*n matrix
  vnl_matrix_fixed() : vnl_matrix_fixed_ref<T,m,n>(space) {}

// -- Construct an m*n matrix and fill with value
  vnl_matrix_fixed(const T& value):vnl_matrix_fixed_ref<T,m,n>(space) {
    int i = m*n;
    while (i--)
      space[i] = value;
  }

// -- Construct an m*n Matrix and copy data into it row-wise.
  vnl_matrix_fixed(const T* datablck) : vnl_matrix_fixed_ref<T,m,n>(space) {
    memcpy(space, datablck, m*n*sizeof(T));
  }

// -- Construct an m*n Matrix and copy rhs into it.   Abort if rhs is
// not the same size.
  vnl_matrix_fixed(const vnl_matrix<T>& rhs) : vnl_matrix_fixed_ref<T,m,n>(space) {
    assert(rhs.rows() == m && rhs.columns() == n);
    memcpy(space, rhs.data_block(), m*n*sizeof(T));
  }

// -- Copy a vnl_matrix into this.   Abort if rhs is
// not the same size.
  vnl_matrix_fixed<T,m,n>& operator=(const vnl_matrix<T>& rhs) {
    assert(rhs.rows() == m && rhs.columns() == n);
    memcpy(space, rhs.data_block(), m*n*sizeof(T));
    return *this;
  }

// -- Copy another vnl_matrix_fixed<T,m,n> into this.
  vnl_matrix_fixed<T,m,n>& operator=(const vnl_matrix_fixed<T, m, n>& rhs) {
    memcpy(space, rhs.data_block(), m*n*sizeof(T));
    return *this;
  }

  vnl_matrix_fixed(const vnl_matrix_fixed<T,m,n>& rhs) : vnl_matrix_fixed_ref<T,m,n>(space) {
    memcpy(space, rhs.data_block(), m*n*sizeof(T));
  }
};

#if defined(VCL_GCC_27)
template <class T,int m,int n>
inline vcl_ostream & operator<<(vcl_ostream &os,const vnl_matrix_fixed<T,m,n> &M)
{
  return os << static_cast<vnl_matrix<T> const &>(M);
}
template <class T,int m,int n>
inline vcl_istream & operator>>(vcl_istream &is,vnl_matrix_fixed<T,m,n> &M)
{
  return is >> static_cast<vnl_matrix<T>       &>(M);
}
template <class T,int m,int n>
inline vnl_matrix<T> operator*(const T &s, const vnl_matrix_fixed<T,m,n> &M)
{
  return s * static_cast< const vnl_matrix<T> & >(M);
}
#endif

#ifndef VCL_SUNPRO_CC
 // -- Multiply two conformant vnl_matrix_fixed (M x N) times (N x O)
 template <class T, int M, int N, int O>
 vnl_matrix_fixed<T, M, O> operator*(const vnl_matrix_fixed<T, M, N>& a, const vnl_matrix_fixed<T, N, O>& b)
 {
  vnl_matrix_fixed<T, M, O> out;
  for(int i = 0; i < M; ++i)
    for(int j = 0; j < O; ++j) {
      T accum = a(i,0) * b(0,j);
      for(int k = 1; k < N; ++k)
	accum += a(i,k) * b(k,j);
      out(i,j) = accum;
    }
  return out;
 }
#endif

#define VNL_MATRIX_FIXED_PAIR_INSTANTIATE(T, M, N, O) \
extern "please include vnl/vnl_matrix_fixed.txx instead"

#endif // vnl_matrix_fixed_h_
