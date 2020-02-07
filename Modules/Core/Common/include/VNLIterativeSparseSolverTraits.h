/*=========================================================================
 *
 *  Copyright NumFOCUS
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

/** \class VNLIterativeSparseSolverTraits
 * \brief Generic interface for iterative sparse linear solver.
 *
 * This generic interface (common to several sparse solvers), allow to
 * interchange solver solutions when dealing with sparse linear system. See
 * itk::ParameterizationQuadEdgeMeshFilter for reference.
 *
 * It internally uses the VNL library to represent and deal with vectors
 * (vnl_vector) and sparse matrices (vnl_sparse_matrix). The solver by itself
 * is an iterative one see vnl_sparse_matrix_linear_system for more details on
 * the method used.
 *
 * \ingroup ITKCommon
 *
 * \sa VNLSparseLUSolverTraits
 */
template <typename T = double>
class VNLIterativeSparseSolverTraits
{
public:
  using ValueType = T;
  using MatrixType = vnl_sparse_matrix<ValueType>;
  using VectorType = vnl_vector<ValueType>;
  using SolverType = vnl_lsqr;

  /** \return false (it is not a direct solver, it is an iterative solver) */
  static bool
  IsDirectSolver()
  {
    return false;
  }

  /** \brief initialize a square sparse matrix of size iN x iN */
  static MatrixType
  InitializeSparseMatrix(const unsigned int & iN)
  {
    return MatrixType(iN, iN);
  }

  /** \brief initialize a sparse matrix of size iRow x iCol */
  static MatrixType
  InitializeSparseMatrix(const unsigned int & iRow, const unsigned int & iCol)
  {
    return MatrixType(iRow, iCol);
  }

  /** \brief initialize a vector of size iN */
  static VectorType
  InitializeVector(const unsigned int & iN)
  {
    return VectorType(iN);
  }

  /** \brief iA[iR][iC] = iV */
  static void
  FillMatrix(MatrixType & iA, const unsigned int & iR, const unsigned int & iC, const ValueType & iV)
  {
    iA(iR, iC) = iV;
  }

  /** \brief iA[iR][iC] += iV */
  static void
  AddToMatrix(MatrixType & iA, const unsigned int & iR, const unsigned int & iC, const ValueType & iV)
  {
    iA(iR, iC) += iV;
  }

  /** \brief Solve the linear system \f$ iA \cdot oX = iB \f$ */
  static bool
  Solve(const MatrixType & iA, const VectorType & iB, VectorType & oX)
  {
    using SparseLinearSystemType = vnl_sparse_matrix_linear_system<ValueType>;
    SparseLinearSystemType system(iA, iB);

    SolverType solver(system);
    return solver.minimize(oX);
  }

  /** \brief Solve the linear systems: \f$ iA \cdot oX = iBx \f$, \f$ iA \cdot oY = iBy \f$, \f$ iA \cdot oZ = iBz \f$
   */
  static bool
  Solve(const MatrixType & iA,
        const VectorType & iBx,
        const VectorType & iBy,
        const VectorType & iBz,
        VectorType &       oX,
        VectorType &       oY,
        VectorType &       oZ)
  {
    bool result1 = Solve(iA, iBx, 100000, oX);
    bool result2 = Solve(iA, iBy, 100000, oY);
    bool result3 = Solve(iA, iBz, 100000, oZ);

    return (result1 && result2 && result3);
  }

  /** \brief Solve the linear systems: \f$ iA \cdot oX = iBx \f$, \f$ iA \cdot oY = iBy \f$ */
  static bool
  Solve(const MatrixType & iA, const VectorType & iBx, const VectorType & iBy, VectorType & oX, VectorType & oY)
  {
    bool result1 = Solve(iA, iBx, oX);
    bool result2 = Solve(iA, iBy, oY);

    return (result1 && result2);
  }

  /** \brief Solve the linear systems: \f$ iA \cdot oX = iBx \f$ in N iterations */
  static bool
  Solve(const MatrixType & iA, const VectorType & iB, const long & iNbIter, VectorType & oX)
  {
    using SparseLinearSystemType = vnl_sparse_matrix_linear_system<ValueType>;
    SparseLinearSystemType system(iA, iB);

    SolverType solver(system);
    solver.set_max_iterations(iNbIter);
    return solver.minimize(oX);
  }
};

#endif
