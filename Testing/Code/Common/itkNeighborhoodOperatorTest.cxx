/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkNeighborhoodOperatorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "vnl/vnl_vector.h"
#include "itkDerivativeOperator.h"
#include "itkForwardDifferenceOperator.h"
#include "itkBackwardDifferenceOperator.h"
#include "itkGaussianOperator.h"
#include "itkSize.h"

void println(const char *c) { std::cout << std::endl << c << std::endl; }

int main()
{
  println("Testing derivative operator");
  itk::DerivativeOperator<float, 3, vnl_vector<float> > d;
  d.SetOrder(2);
  d.SetDirection(1);
  d.CreateDirectional();
  d.Print(std::cout);

  println("Testing Gaussian operator");
  itk::GaussianOperator<2, vnl_vector<float> > g;
  g.SetVariance(2.3);
  g.SetMaximumError(.01);
  g.CreateDirectional();
  g.Print(std::cout);

  println("Testing ForwardDifferenceOperator");
  itk::ForwardDifferenceOperator<float, 4, vnl_vector<float> > f;
  itk::Size<4> sz;
  sz[0] = sz[1] = sz[2] = sz[3] = 2;
  f.SetDirection(2);
  f.CreateToRadius(sz);
  f.Print(std::cout);

  println("Testing BackwardDifferenceOperator");
  itk::BackwardDifferenceOperator<float, 2, vnl_vector<float> > b;
  b.SetDirection(0);
  b.CreateDirectional();
  b.Print(std::cout);

  return 0;
}
