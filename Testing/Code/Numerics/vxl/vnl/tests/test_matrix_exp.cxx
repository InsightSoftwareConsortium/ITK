/*
  fsm@robots.ox.ac.uk
*/
#include <vcl_iostream.h>

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_matrix_exp.h>
#include <vnl/vnl_matlab_print.h>
#include <vnl/vnl_rotation_matrix.h>
#include <vnl/vnl_cross_product_matrix.h>

int test_matrix_exp(int, char * [] )
{
  vnl_vector<double> v(3);
  v[0] =  1.233;
  v[1] = -0.572;
  v[2] =  0.777;

  vnl_matrix<double> X = vnl_cross_product_matrix(v);
  vnl_matlab_print(vcl_cout, X, "[v]");
  
  vnl_matrix<double> expX = vnl_matrix_exp(X);
  vnl_matlab_print(vcl_cout, expX, "matrix exp([v])");

  vnl_matrix<double> rotv = vnl_rotation_matrix(v);
  vnl_matlab_print(vcl_cout, rotv, "rotate exp([v])");

  return 0;
}
