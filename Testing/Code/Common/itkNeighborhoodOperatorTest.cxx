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
#include "itkDerivativeOperator.h"
#include "itkDerivativeHalfForwardOperator.h"
#include "itkDerivativeHalfBackwardOperator.h"
#include "itkGaussianOperator.h"
#include "itkSize.h"

int main()
{
  itk::DerivativeOperator<float, 3> d;
  d.SetOrder(3);
  d.SetDirection(1);
  d.CreateDirectional();

  itk::GaussianOperator<2> g;
  g.SetVariance(2.3);
  g.SetMaximumError(.01);
  g.CreateDirectional();

  itk::DerivativeHalfForwardOperator<float, 4> f;
  itk::Size<4> sz;
  sz[0] = sz[1] = sz[2] = sz[3] = 2;
  f.SetDirection(2);
  f.CreateToRadius(sz);

  itk::DerivativeHalfBackwardOperator<float, 2> b;
  b.SetDirection(0);
  b.CreateDirectional();

  return 0;
}
