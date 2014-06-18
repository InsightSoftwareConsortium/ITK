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
#ifndef __VNLSparseLUSolverTraits_h
#define __VNLSparseLUSolverTraits_h

#include "vnl/vnl_vector.h"
#include "vnl/vnl_sparse_matrix.h"
#include "vnl/algo/vnl_sparse_lu.h"

/** \class VNLSparseLUSolverTraits
 * \brief Generic interface for sparse LU solver.
 *
 * This generic interface (common to several sparse solvers), allow to
 * interchange solver solutions when dealing with sparse linear systems. See
 * itk::ParameterizationQuadEdgeMeshFilter for reference.
 *
 * It internally uses the VNL library to represent and deal with vectors
 * (vnl_vector) and sparse matrices (vnl_sparse_matrix). The solver by itself
 * is made of a sparse LU decomposition followed by solving upper triangular
 * system.  see vnl_sparse_lu for more details on the method used.
 *
 * \ingroup ITKCommon
 *
 * \sa VNLIterativeSparseSolverTraits:w
 */
template< typename T = double >
class VNLSparseLUSolverTraits
{
public:
  typedef T                               ValueType;
  typedef vnl_sparse_matrix< ValueType >  MatrixType;
  typedef vnl_vector< ValueType >         VectorType;
  typedef vnl_sparse_lu                   SolverType;

  /** \return false (it is not a direct solver, it is an iterative solver) */
  static bool IsDirectSolver()
  {
    return true;
  }

  /** \brief initialize a square sparse matrix of size iN x iN */
  static MatrixType InitializeSparseMatrix(const unsigned int & iN)
  {
    return MatrixType(iN, iN);
  }

  /** \brief initialize a sparse matrix of size iRow x iCol */
  static MatrixType InitializeSparseMatrix(const unsigned int & iRow, const unsigned int& iCol)
  {
    return MatrixType(iRow, iCol);
  }

  /** \brief initialize a vector of size iN */
  static VectorType InitializeVector(const unsigned int & iN)
  {
    return VectorType(iN);
  }

  /** \brief iA[iR][iC] = iV */
  static void FillMatrix(MatrixType & iA, const unsigned int & iR, const unsigned int & iC, const ValueType & iV)
  {
    iA(iR, iC) = iV;
  }

  /** \brief iA[iR][iC] += iV */
  static void AddToMatrix(MatrixType & iA, const unsigned int & iR, const unsigned int & iC, const ValueType & iV)
  {
    iA(iR, iC) += iV;
  }

  /** \brief Solve the linear system \f$ iA \cdot oX = iB \f$ */
  static bool Solve(const MatrixType & iA, const VectorType & iB, VectorType & oX)
  {
    SolverType solver( iA );
    Solve( solver, iB, oX );

    return true;
  }

  /** \brief Solve the linear systems: \f$ iA \cdot oX = iBx \f$, \f$ iA \cdot oY = iBy \f$, \f$ iA \cdot oZ = iBz \f$ */
  static bool Solve(const MatrixType & iA,
             const VectorType & iBx, const VectorType & iBy, const VectorType & iBz,
             VectorType & oX, VectorType & oY, VectorType & oZ )
  {
    SolverType solver( iA );
    Solve( solver, iBx, iBy, iBz, oX, oY, oZ );

    return true;
  }

  /** \brief Solve the linear systems: \f$ iA \cdot oX = iBx \f$, \f$ iA \cdot oY = iBy \f$ */
  static bool Solve(const MatrixType & iA,
             const VectorType & iBx, const VectorType & iBy,
             VectorType & oX, VectorType & oY)
  {
    SolverType solver( iA );
    Solve( solver, iBx, iBy, oX, oY );

    return true;
  }

  /** \brief Solve the linear system \f$ iA \cdot oX = iB \f$ factoring the internal matrix if needed */
  static void Solve( SolverType & solver, const VectorType & iB, VectorType & oX )
  {
    oX = solver.solve( iB );
  }

  /** \brief Solve the linear systems: \f$ iA \cdot oX = iBx \f$, \f$ iA \cdot oY = iBy \f$, \f$ iA \cdot oZ = iBz \f$ factoring the internal matrix if needed */
  static void Solve( SolverType & solver,
             const VectorType & iBx, const VectorType & iBy, const VectorType & iBz,
             VectorType & oX, VectorType & oY, VectorType & oZ )
  {
    oX = solver.solve( iBx );
    oY = solver.solve( iBy );
    oZ = solver.solve( iBz );
  }

  /** \brief Solve the linear systems: \f$ iA \cdot oX = iBx \f$, \f$ iA \cdot oY = iBy \f$ factoring the internal matrix if needed */
  static void Solve( SolverType & solver,
             const VectorType & iBx, const VectorType & iBy,
             VectorType & oX, VectorType & oY )
  {
    oX = solver.solve( iBx );
    oY = solver.solve( iBy );
  }
};

#endif
