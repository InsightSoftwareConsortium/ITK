//-*- c++ -*-------------------------------------------------------------------
// Module: Hyperplane fit using orthogonal regression
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: 31 Aug 96
// Converted to vxl by Peter Vanroose, February 2000
//-----------------------------------------------------------------------------

#include <vcl_iostream.h>
#include <vnl/vnl_fastops.h>
#include <vnl/algo/vnl_svd.h>
#include <vnl/algo/vnl_symmetric_eigensystem.h>

int main()
{
  // Read points from stdin
  vnl_matrix<double> pts;
  vcl_cin >> pts;

  // Build design matrix D
  int npts = pts.rows();
  int dim = pts.columns();
  vnl_matrix<double> D(npts, dim+1);
  for (int i = 0; i < npts; ++i) {
    for (int j = 0; j < dim; ++j) D(i,j) = pts(i,j);
    D(i,dim) = 1;
  }

  // 1. Compute using SVD
  {
    vnl_svd<double> svd(D);
    vnl_vector<double> a = svd.nullvector();
    vcl_cout << "SVD residual = " << (D * a).magnitude() << vcl_endl;
  }

  // 2. Compute using eigensystem of D'*D
  {
    vnl_matrix<double> m; vnl_fastops::AtA(m,D);
    vnl_symmetric_eigensystem<double> eig(m);
    vnl_vector<double> a = eig.get_eigenvector(0);
    vcl_cout << "Eig residual = " << (D * a).magnitude() << vcl_endl;
  }

  return 0;
}
