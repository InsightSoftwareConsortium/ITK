// This is vxl/vnl/vnl_inverse.h
#ifndef vnl_inverse_h_
#define vnl_inverse_h_
//:
// \file
// \brief Calculates inverse of a small vnl_matrix_fixed (not using svd)
// \author Peter Vanroose
// \date   22 October 2002

#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_det.h>

//: Calculates inverse of a small vnl_matrix_fixed (not using svd)
//  This allows you to write
//
//  x = vnl_inverse(A) * b;
//
// Note that this function is inlined (except for the call to vnl_det()),
// which makes it much faster than the vnl_matrix_inverse class in vnl/algo
// since that one is using svd.

template <class T>
vnl_matrix_fixed<T,1,1> vnl_inverse(vnl_matrix_fixed<T,1,1> const& m)
{
  return vnl_matrix_fixed<T,1,1>(T(1)/m(0,0));
}

template <class T>
vnl_matrix_fixed<T,2,2> vnl_inverse(vnl_matrix_fixed<T,2,2> const& m)
{
  T d[4];
  T det = T(1)/vnl_det(m);
  d[0] = m(1,1)*det; d[1] = - m(0,1)*det;
  d[3] = m(0,0)*det; d[2] = - m(1,0)*det;
  return vnl_matrix_fixed<T,2,2>(d);
}

template <class T>
vnl_matrix_fixed<T,3,3> vnl_inverse(vnl_matrix_fixed<T,3,3> const& m)
{
  T d[9];
  T det = T(1)/vnl_det(m);
  d[0] = (m(1,1)*m(2,2)-m(1,2)*m(2,1))*det;
  d[1] = (m(2,1)*m(0,2)-m(2,2)*m(0,1))*det;
  d[2] = (m(0,1)*m(1,2)-m(0,2)*m(1,1))*det;
  d[3] = (m(1,2)*m(2,0)-m(1,0)*m(2,2))*det;
  d[4] = (m(0,0)*m(2,2)-m(0,2)*m(2,0))*det;
  d[5] = (m(1,0)*m(0,2)-m(1,2)*m(0,0))*det;
  d[6] = (m(1,0)*m(2,1)-m(1,1)*m(2,0))*det;
  d[7] = (m(0,1)*m(2,0)-m(0,0)*m(2,1))*det;
  d[8] = (m(0,0)*m(1,1)-m(0,1)*m(1,0))*det;
  return vnl_matrix_fixed<T,3,3>(d);
}

template <class T>
vnl_matrix_fixed<T,4,4> vnl_inverse(vnl_matrix_fixed<T,4,4> const& m)
{
  // subdivide m in four 2x2 matrices:
  T a[4], b[4], c[4], d[4];
  a[0] = m(0,0); a[1] = m(0,1); a[2] = m(1,0); a[3] = m(1,1);
  b[0] = m(0,2); b[1] = m(0,3); b[2] = m(1,2); b[3] = m(1,3);
  c[0] = m(2,0); c[1] = m(2,1); c[2] = m(3,0); c[3] = m(3,1);
  d[0] = m(2,2); d[1] = m(2,3); d[2] = m(3,2); d[3] = m(3,3);
  vnl_matrix_fixed<T,2,2> A(a),B(b),C(c),D(d);
  // Now solve the matrix eqns A*Ai+B*Ci=I, A*Bi+B*Di=0=C*Ai+D*Ci, C*Bi+D*Di=I:
  vnl_matrix_fixed<T,2,2> DC = vnl_inverse(D)*C,
                          BA = vnl_inverse(B)*A;
  vnl_matrix_fixed<T,2,2> Ai=vnl_inverse(A-B*DC),
                          Bi=vnl_inverse(C-D*BA),
                          Ci=-DC*Ai, Di=-BA*Bi;
  // and fill the results into the matrix to be returned:
  T e[16];
  e[0] = Ai(0,0); e[1] = Ai(0,1); e[4] = Ai(1,0); e[5] = Ai(1,1); 
  e[2] = Bi(0,0); e[3] = Bi(0,1); e[6] = Bi(1,0); e[7] = Bi(1,1); 
  e[8] = Ci(0,0); e[9] = Ci(0,1); e[12]= Ci(1,0); e[13]= Ci(1,1); 
  e[10]= Di(0,0); e[11]= Di(0,1); e[14]= Di(1,0); e[15]= Di(1,1); 
  return vnl_matrix_fixed<T,4,4>(e);
}

#endif // vnl_inverse_h_
