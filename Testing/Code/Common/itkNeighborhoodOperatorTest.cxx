/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodOperatorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "vnl/vnl_vector.h"
#include "itkDerivativeOperator.h"
#include "itkForwardDifferenceOperator.h"
#include "itkBackwardDifferenceOperator.h"
#include "itkGaussianOperator.h"
#include "itkLaplacianOperator.h"
#include "itkSobelOperator.h"
#include "itkSize.h"
namespace
{
  void println(const char *c) { std::cout << std::endl << c << std::endl; }
}

int itkNeighborhoodOperatorTest(int, char* [] )
{
  println("Testing derivative operator");
  itk::DerivativeOperator<float, 3, vnl_vector<float> > d;
  d.SetOrder(2);
  d.SetDirection(1);
  d.CreateDirectional();
  d.Print(std::cout);

  println("Testing Gaussian operator");
  itk::GaussianOperator<float,2, vnl_vector<float> > g;
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

  println("Testing LaplacianOperator");
  itk::LaplacianOperator<float, 2, vnl_vector<float> > a;
  a.CreateOperator();
  a.Print(std::cout);

  println("Testing SobelOperator");
  itk::SobelOperator<float, 2, vnl_vector<float> > c;
  c.SetDirection(0);
  c.CreateDirectional();
  c.Print(std::cout);



  return 0;
}
