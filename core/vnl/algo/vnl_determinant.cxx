#include "vnl_determinant.h"
#include <vcl_cassert.h>

int vnl_determinant(vnl_matrix<int> const &M, bool balance )
{
  unsigned n = M.rows();
  assert(M.cols() == n);

  switch (n)
  {
   case 1: return M[0][0];
   case 2: return vnl_determinant(M[0], M[1]);
   case 3: return vnl_determinant(M[0], M[1], M[2]);
   case 4: return vnl_determinant(M[0], M[1], M[2], M[3]);
   default:
    vnl_matrix<double> m(n,n);
    for (unsigned int i=0; i<n; ++i)
      for (unsigned int j=0; j<n; ++j)
        m(i,j)=double(M(i,j));
    return int(0.5+vnl_determinant(m, balance)); // round to nearest integer
  }
}
