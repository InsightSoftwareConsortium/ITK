//-*- c++ -*-------------------------------------------------------------------
// Module: complex-svd
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: 09 May 97
// Converted to vxl by Peter Vanroose, February 2000
//-----------------------------------------------------------------------------

#include <iostream>
#include <complex>
#include <vcl_compiler.h>
#include <vnl/vnl_matlab_print.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_vector.h>
#include <vnl/algo/vnl_svd.h>

int main()
{
  double r[] = {
    2,     7,     5,
    1,     0,     9,
    8,     5,     7,
    4,     0,     7
  };

  double i[] = {
    0,     4,     9,
    1,     4,     7,
    8,     7,     1,
    6,     5,     4
  };

  std::complex<double> cmplx[12];
  for (int k=0; k<12; ++k) cmplx[k] = std::complex<double>(r[k],i[k]);

  vnl_matrix<std::complex<double> > C(cmplx, 4, 3);

  std::cout << "C = " << C << std::endl;

  vnl_svd<std::complex<double> > C_svd(C);

  vnl_matlab_print(std::cout, C_svd.U(), "U");
  vnl_matlab_print(std::cout, C_svd.W(), "W");
  vnl_matlab_print(std::cout, C_svd.V(), "V");

  std::complex<double> rhs[4]; rhs[0]=3; rhs[1]=9; rhs[2]=-2; rhs[3]=-8;
  vnl_vector<std::complex<double> > b(rhs, 4);

  // From "C x = b" find x:
  std::cout << "x = " << C_svd.solve(b) << std::endl;

  return 0;
}
