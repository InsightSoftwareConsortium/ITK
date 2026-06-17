/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef EigenSparseLUSolverTraits_h
#define EigenSparseLUSolverTraits_h

#include "itk_eigen.h"
#include ITK_EIGEN(Sparse)
#include ITK_EIGEN(SparseLU)

/** \class EigenSparseLUSolverTraits
 * \brief Generic interface for sparse LU solver backed by Eigen::SparseLU.
 *
 * This generic interface (common to several sparse solvers), allow to
 * interchange solver solutions when dealing with sparse linear systems. See
 * itk::ParameterizationQuadEdgeMeshFilter for reference.
 *
 * \ingroup ITKCommon
 *
 * \sa VNLSparseLUSolverTraits
 * \sa VNLIterativeSparseSolverTraits
 */
template <typename T = double>
class EigenSparseLUSolverTraits
{
public:
  using ValueType = T;
  using MatrixType = Eigen::SparseMatrix<ValueType>;
  using VectorType = Eigen::Matrix<ValueType, Eigen::Dynamic, 1>;
  using SolverType = Eigen::SparseLU<MatrixType>;

  static bool
  IsDirectSolver()
  {
    return true;
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
    iA.coeffRef(iR, iC) = iV;
  }

  /** \brief iA[iR][iC] += iV */
  static void
  AddToMatrix(MatrixType & iA, const unsigned int & iR, const unsigned int & iC, const ValueType & iV)
  {
    iA.coeffRef(iR, iC) += iV;
  }

  /** \brief oX = iA * iB */
  static void
  MatVecMult(const MatrixType & iA, const VectorType & iB, VectorType & oX)
  {
    oX = iA * iB;
  }

  /** \brief Solve the linear system \f$ iA \cdot oX = iB \f$ */
  static bool
  Solve(const MatrixType & iA, const VectorType & iB, VectorType & oX)
  {
    MatrixType A(iA);
    A.makeCompressed();
    SolverType solver(A);
    oX = solver.solve(iB);
    return solver.info() == Eigen::Success;
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
    MatrixType A(iA);
    A.makeCompressed();
    SolverType solver(A);
    Solve(solver, iBx, iBy, iBz, oX, oY, oZ);
    return solver.info() == Eigen::Success;
  }

  /** \brief Solve the linear systems: \f$ iA \cdot oX = iBx \f$, \f$ iA \cdot oY = iBy \f$ */
  static bool
  Solve(const MatrixType & iA, const VectorType & iBx, const VectorType & iBy, VectorType & oX, VectorType & oY)
  {
    MatrixType A(iA);
    A.makeCompressed();
    SolverType solver(A);
    Solve(solver, iBx, iBy, oX, oY);
    return solver.info() == Eigen::Success;
  }

  /** \brief Solve the linear system \f$ iA \cdot oX = iB \f$ reusing the factored matrix */
  static void
  Solve(SolverType & solver, const VectorType & iB, VectorType & oX)
  {
    oX = solver.solve(iB);
  }

  /** \brief Solve the linear systems: \f$ iA \cdot oX = iBx \f$, \f$ iA \cdot oY = iBy \f$, \f$ iA \cdot oZ = iBz \f$
   * reusing the factored matrix */
  static void
  Solve(SolverType &       solver,
        const VectorType & iBx,
        const VectorType & iBy,
        const VectorType & iBz,
        VectorType &       oX,
        VectorType &       oY,
        VectorType &       oZ)
  {
    oX = solver.solve(iBx);
    oY = solver.solve(iBy);
    oZ = solver.solve(iBz);
  }

  /** \brief Solve the linear systems: \f$ iA \cdot oX = iBx \f$, \f$ iA \cdot oY = iBy \f$ reusing the factored
   * matrix */
  static void
  Solve(SolverType & solver, const VectorType & iBx, const VectorType & iBy, VectorType & oX, VectorType & oY)
  {
    oX = solver.solve(iBx);
    oY = solver.solve(iBy);
  }
};

#endif
