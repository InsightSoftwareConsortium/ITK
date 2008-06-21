/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConformalFlatteningFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConformalFlatteningFunction_h
#define __itkConformalFlatteningFunction_h

#include "itkExceptionObject.h"

#include "vnl/vnl_math.h"
#include "vcl_algorithm.h"

#include <vnl/vnl_cost_function.h>
#include <vnl/algo/vnl_conjugate_gradient.h>

namespace itk
{

class ConformalFlatteningFunction :  public vnl_cost_function
{
public:
  typedef vnl_vector<double>        VectorType;
  typedef vnl_sparse_matrix<double> MatrixType;

  inline unsigned int GetDimension() { return m_Dimension; }

private:
  MatrixType const*    m_Matrix;
  VectorType const*    m_Vector;
  unsigned int         m_Dimension;

public:
ConformalFlatteningFunction(vnl_sparse_matrix<double> const& A,
    VectorType const& b) : vnl_cost_function(b.size())
{
  this->m_Matrix = &A;
  this->m_Vector = &b;
  this->m_Dimension = b.size();

  if( A.rows() != b.size() )
    {
    ExceptionObject excp(__FILE__,__LINE__,
    "The # of rows in A must be the same as the length of b");
    throw excp;
    }
}

double
f(const VectorType& x)
{
  VectorType tmp;
  this->m_Matrix->pre_mult(x, tmp);
  return 0.5 * inner_product(tmp,x) - inner_product(*this->m_Vector,x);
}

void
gradf(const VectorType& x, VectorType& gradient)
{
  VectorType tmp;
  this->m_Matrix->mult(x, tmp);
  gradient = tmp - *this->m_Vector;
}

};

}
#endif

