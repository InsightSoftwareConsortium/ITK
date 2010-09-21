/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    VNLSparseLUSolverTraits.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef  __VNLSparseLUSolverTraits_h
#define  __VNLSparseLUSolverTraits_h

#include <vnl/vnl_vector.h>
#include <vnl/vnl_sparse_matrix.h>
#include <vnl/vnl_sparse_matrix_linear_system.h>
#include <vnl/algo/vnl_sparse_lu.h>

class VNLSparseLUSolverTraits
{
public:

  typedef double                         ValueType;
  typedef vnl_sparse_matrix< ValueType > MatrixType;
  typedef vnl_vector< ValueType >        VectorType;
  typedef vnl_sparse_lu                  SolverType;

  VNLSparseLUSolverTraits();

  MatrixType InitializeSparseMatrix(const unsigned int & iN)
  {
    return MatrixType(iN, iN);
  }

  VectorType InitializeVector(const unsigned int & iN)
  {
    return VectorType(iN);
  }

  void FillMatrix(MatrixType & iA, const unsigned int & iR, const unsigned int & iC, const ValueType & iV)
  {
    iA(iR, iC) = iV;
  }

  bool Solve(const MatrixType & iA, const VectorType & iB, VectorType & oX)
  {
    vnl_sparse_lu lu_solver(iA);

    return lu_solver.solve(iB);
  }

private:
  MatrixType m_Matrix;
};
