/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef  __VNLSparseLUSolverTraits_h
#define  __VNLSparseLUSolverTraits_h

#include "vnl/vnl_vector.h"
#include "vnl/vnl_sparse_matrix.h"
#include "vnl/vnl_sparse_matrix_linear_system.h"
#include "vnl/algo/vnl_sparse_lu.h"

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
