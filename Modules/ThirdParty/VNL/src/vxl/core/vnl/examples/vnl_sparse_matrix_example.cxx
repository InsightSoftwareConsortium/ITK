#include <iostream>
#include <cstdlib>
#include <vnl/vnl_sparse_matrix.h>
#include <vnl/algo/vnl_sparse_symmetric_eigensystem.h>
#include <vnl/vnl_random.h>

#include <vcl_compiler.h>

int main()
{
  const int N = 100;
  vnl_sparse_matrix<double> a(N, N);
  vnl_random rg;

  for (int i = 0; i < 10; ++i)
  {
    int x = rg.lrand32(N-1);
    int y = rg.lrand32(N-1);
    a(x,y) = a(y,x) = 10*rg.normal(); // symmetric matrix
  }

  a.normalize_rows();
  a = a * a; // i.e., also a * aT

  vnl_sparse_symmetric_eigensystem s;
  int b = s.CalculateNPairs(a, 2, true, 3);

  if (b == 0)
  {
    std::cout<<s.get_eigenvalue(0)<<std::endl;
    std::cout<<s.get_eigenvector(0)<<std::endl;

    std::cout<<s.get_eigenvalue(1)<<std::endl;
    std::cout<<s.get_eigenvector(1)<<std::endl;

  }
  else
  {
    std::cerr<<"b = " << b << std::endl;
  }

  return 0;
}
