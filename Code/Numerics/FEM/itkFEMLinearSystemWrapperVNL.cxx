/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLinearSystemWrapperVNL.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkFEMLinearSystemWrapperVNL.h"

namespace itk {
namespace fem {



void LinearSystemWrapperVNL::InitA(int N)
{
  delete m_A;
  m_A=new vnl_sparse_matrix<Float>(N,N);
}



void LinearSystemWrapperVNL::InitB(void)
{
  if(!m_A) throw;
  delete m_B;
  m_B=new vnl_vector<Float>(m_A->rows());
  m_B->fill(0.0);
}



void LinearSystemWrapperVNL::Solve(void)
{
  if(!m_A || !m_B) throw;
  delete m_x;
  m_x=new vnl_vector<Float>(m_A->rows());

  /*
   * Solve the sparse system of linear equation and store the result in vector x_.
   * Here we use the iterative least squares solver.
   */
  vnl_sparse_matrix_linear_system<Float> ls(*m_A,*m_B);
  vnl_lsqr lsq(ls);

  /*
   * Set max number of iterations to 3*size of the K matrix.
   * FIXME: There should be a better way to determine the number of iterations needed.
   */
  lsq.set_max_iterations(3*m_A->rows());
  lsq.minimize(*m_x);

}



LinearSystemWrapperVNL::~LinearSystemWrapperVNL()
{
  delete m_A;
  delete m_B;
  delete m_x;
}



}} // end namespace itk::fem
