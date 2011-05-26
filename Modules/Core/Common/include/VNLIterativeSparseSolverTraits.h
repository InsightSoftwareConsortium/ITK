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
#ifndef __VNLIterativeSparseSolverTraits_h
#define __VNLIterativeSparseSolverTraits_h

#include "vnl/vnl_vector.h"
#include "vnl/vnl_sparse_matrix.h"
#include "vnl/vnl_sparse_matrix_linear_system.h"
#include "vnl/algo/vnl_lsqr.h"

template< typename T = double >
class VNLIterativeSparseSolverTraits
{
public:
  typedef T                              ValueType;
  typedef vnl_sparse_matrix< ValueType > MatrixType;
  typedef vnl_vector< ValueType >        VectorType;
  typedef vnl_lsqr                       SolverType;

  VNLIterativeSparseSolverTraits() {}
  ~VNLIterativeSparseSolverTraits() {}

  bool IsDirectSolver() const
  {
    return false;
  }

  MatrixType InitializeSparseMatrix(const unsigned int & iN) const
  {
    return MatrixType(iN, iN);
  }

  VectorType InitializeVector(const unsigned int & iN) const
  {
    return VectorType(iN);
  }

  void FillMatrix(MatrixType & iA, const unsigned int & iR, const unsigned int & iC, const ValueType & iV) const
  {
    iA(iR, iC) = iV;
  }

  void AddToMatrix(MatrixType & iA, const unsigned int & iR, const unsigned int & iC, const ValueType & iV) const
  {
    iA(iR, iC) += iV;
  }

  bool Solve(const MatrixType & iA, const VectorType & iB, VectorType & oX) const
  {
    typedef vnl_sparse_matrix_linear_system< ValueType > SparseLinearSystemType;
    SparseLinearSystemType system(iA, iB);

    SolverType solver(system);
    return solver.minimize(oX);
  }

  // no interest to use this method...
  bool Solve(const MatrixType & iA,
             const VectorType & iBx, const VectorType & iBy,
             VectorType & oX, VectorType & oY) const
  {
    bool result1 = Solve(iA, iBx, oX);
    bool result2 = Solve(iA, iBy, oY);

    return ( result1 && result2 );
  }

  bool Solve(const MatrixType & iA,
             const VectorType & iB,
             const long & iNbIter,
             VectorType & oX) const
  {
    typedef vnl_sparse_matrix_linear_system< ValueType > SparseLinearSystemType;
    SparseLinearSystemType system(iA, iB);

    SolverType solver(system);
    solver.set_max_iterations(iNbIter);
    return solver.minimize(oX);
  }
};

#endif
