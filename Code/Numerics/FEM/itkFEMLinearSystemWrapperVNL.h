/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLinearSystemWrapperVNL.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMLinearSystemWrapperVNL_h
#define __itkFEMLinearSystemWrapperVNL_h

#include "itkFEMLinearSystemWrapper.h"
#include "vnl/vnl_sparse_matrix.h"
#include "vnl/vnl_vector.h"
#include <vnl/vnl_sparse_matrix_linear_system.h>
#include <vxl/vnl/algo/vnl_lsqr.h>




namespace itk {
namespace fem {

/**
 * \class LinearSystemWrapperVNL
 * \brief LinearSystemWrapper class that uses VNL numeric library functions
 *        to define a sparse linear system of equations.
 * \sa LinearSystemWrapper
 */
class LinearSystemWrapperVNL : public LinearSystemWrapper
{
public:
  LinearSystemWrapperVNL() : LinearSystemWrapper(), m_A(0), m_B(0), m_x(0) {}

  virtual void InitA(int N);  
  virtual void InitB(void);

  virtual Float GetA(int i, int j) { return (*m_A)(i,j); }
  virtual void SetA(int i, int j, Float value) { (*m_A)(i,j)=value; }
  virtual void AddA(int i, int j, Float value) { (*m_A)(i,j)+=value; }
  virtual Float GetB(int i) { return (*m_B)(i); }
  virtual void SetB(int i, Float value) { (*m_B)(i)=value; }
  virtual void AddB(int i, Float value) { (*m_B)(i)+=value; }
  virtual Float GetX(int i) { return (*m_x)(i); }
  virtual void SetX(int i, Float value) { (*m_x)(i)=value; }
  virtual void Solve(void);
  
  virtual ~LinearSystemWrapperVNL();

private:
  /** Pointer to VNL sparse matrix */
  vnl_sparse_matrix<Float> *m_A;

  /** Pointer to VNL vector that stores rhs of equation */
  vnl_vector<Float> *m_B;

  /** Pointer to VNL vector that stores the solution */
  vnl_vector<Float> *m_x;

};



}} // end namespace itk::fem

#endif // #ifndef __itkFEMLinearSystemWrapperVNL_h
