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

#ifndef itkFEMItpackSparseMatrix_h
#define itkFEMItpackSparseMatrix_h

#include "itkFEMException.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class ItpackSparseMatrix
 * \brief a compressed row sparse matrix representation that makes
 *        use of itpack to dynamically assemble the matrix
 * \sa ItpackLinearSystemWrapper
 * \ingroup ITKFEM
 */

// Forward declaration of friend class
class ItpackLinearSystemWrapper;

class ITKFEM_EXPORT ItpackSparseMatrix
{
public:

  /** typedefs from f2c.h  */
  typedef long   integer;
  typedef double doublereal;

  /** Constructor */
  ItpackSparseMatrix();

  /**
   * Constructor with single parameter
   * \param order the order of the matrix to be created
   */
  ItpackSparseMatrix(integer order);

  /**
   * Constructor with two parameters
   * \param order the order of the matrix to be created
   * \param maxNonZeroValues the maximum number of non-zero values that may
   *        appear in the matrix
   */
  ItpackSparseMatrix(integer order, integer maxNonZeroValues);

  /**
   * Destructor
   */
  ~ItpackSparseMatrix();

  /**
   * Set the order of the matrix
   * \param order the order of the matrix
   * \note the order must be set before any values are entered
   */
  void SetOrder(integer order)
  {
    m_N = order;
  }

  /**
   * Set the maximum number of non-zero values that may appear in the matrix
   * \param maxNonZeroValues maximum number of non-zero values that may appear in matrix
   * \note the maxNonZeroValues must be set before any values are entered
   */
  void SetMaxNonZeroValues(integer maxNonZeroValues)
  {
    m_NZ = maxNonZeroValues;
  }

  /**
   * Insert a value into the matrix
   * \param i row index
   * \param j column index
   * \param value value to be added at (i,j)
   */
  void Set(integer i, integer j, doublereal value);

  /**
   * Add to existing entry of matrix
   * \param i row index
   * \param j column index
   * \param value value to add to current value at (i,j)
   */
  void Add(integer i, integer j, doublereal value);

  /** Get a value from the matrix
   * \param i row index
   * \param j column index
   */
  doublereal Get(integer i, integer j);

  /**
   * Get the order of the matrix (via "itpack-like" naming scheme)
   */
  integer *    GetN()
  {
    return &m_N;
  }

  /**
   * Get the row indices of the matrix (via "itpack-like" naming scheme)
   */
  integer *     GetIA();

  /**
   * Pass pointers to compressed row format arrays
   * \param ia row indices
   * \param ja column indices
   * \param a matrix values
   */
  void  SetCompressedRow(integer *ia, integer *ja, doublereal *a);

  /**
   * Get the column indices of the matrix (via "itpack-like" naming scheme)
   */
  integer *     GetJA();

  /**
   * Get the values of the matrix (via "itpack-like" naming scheme)
   */
  doublereal *  GetA();

  /**
   * Get the values of the matrix
   */
  doublereal * GetValueArray()
  {
    return GetA();
  }

  /**
   * Get the column indices
   */
  integer *    GetColumnArray()
  {
    return GetJA();
  }

  /**
   * Get the row indices
   */
  integer *    GetRowArray()
  {
    return GetIA();
  }

  /**
   * Get the order of the matrix
   */
  integer     GetOrder()       const
  {
    return m_N;
  }

  /**
   * Get the maximum number of non-zero values allowed in the matrix
   */
  integer     GetMaxNonZeroValues() const
  {
    return m_NZ;
  }

  /**
   * Clear the memory
   */
  void Clear();

  /**
   * Multiply the matrix by a vector
   */
  void mult(doublereal *vector, doublereal *result);

  /**
   * Multiply the matrix by another ItpackSparseMatrix
   */
  void mult(ItpackSparseMatrix *rightMatrix, ItpackSparseMatrix *resultMatrix);

  /** output compressed row vectors: IA, JA, A */
  void PrintCompressedRow();

private:

  /** friend class */
  friend class LinearSystemWrapperItpack;

  /** initialize matrix */
  void Initialize();

  /** unfinalize matrix */
  void UnFinalize();

  /** finalize matrix form */
  void Finalize();

  /** flag indicating whether the matrix representation has been finalized */
  integer m_MatrixFinalized;

  /** flag indicating variables have been initialized */
  integer m_MatrixInitialized;

  /** Order of system */
  integer m_N;

  /** Maximum number of non-zero elements in master stiffness matrix */
  integer m_NZ;

  /** row pointegerers used in compressed row storage format */
  integer *m_IA;

  /** column indices used in compressed row storage format */
  integer *m_JA;

  /** nonzero entries in compressed row storage format */
  doublereal *m_A;

  /** integer workspace used in matrix building */
  integer *m_IWORK;

  /**
   * flag indicating mode of matrix building for repeat entries
   * m_MODE < 0 - current entry left as is
   *        = 0 - current entry reset
   *        > 0 - value is added to current entry
   */
  integer m_MODE;

  /** unit number that error messages are written to during matrix building */
  integer m_NOUT;

  /**
   * flag indicating desired level of error reporting during matrix building
   * m_LEVEL < 0 - no printing
   *         = 0 - fatal error messages written to m_NOUT
   *         > 0 - messages printed when repeat entries are encountered
   * \note must be set to less than 0
   */
  integer m_LEVEL;

  /**
   * error flag for matrix building
   * m_IER = 0   - no error
   *       = 700 - entry already set - reset according to m_MODE
   *       = 701 - Improper value for i or j
   *       = 701 - m_NZ is too small - no room for new entry
   */
  // integer m_IER;
};

/**
 * \class FEMExceptionItpackSparseMatrixSbagn
 * \brief handles errors that occur when unfinalizing the matrix
 * \sa ItpackSparseMatrix
 * \sa FEMException
 * \ingroup ITKFEM
 */
class ITK_ABI_EXPORT FEMExceptionItpackSparseMatrixSbagn : public FEMException
{
public:

  /** typedefs from f2c.h  */
  typedef long   integer;
  typedef double doublereal;

  /**
   * Constructor. In order to construct this exception object, five parameters
   * must be provided: file, lineNumber, location and a detailed description
   * of the exception, and the invalid index
   */
  FEMExceptionItpackSparseMatrixSbagn(const char *file, unsigned int lineNumber, std::string location,
                                      integer errorCode);

  /** Virtual destructor needed for subclasses. Has to have empty throw(). */
  virtual ~FEMExceptionItpackSparseMatrixSbagn()
  ITK_NOEXCEPT ITK_OVERRIDE;

  /** Type related information. */
  itkTypeMacro(FEMExceptionItpackSparseMatrixSbagn, FEMException);
};

/**
 * \class FEMExceptionItpackSparseMatrixSbsij
 * \brief handles errors that occur when building the matrix
 * \sa ItpackSparseMatrix
 * \sa FEMException
 * \ingroup ITKFEM
 */
class ITK_ABI_EXPORT FEMExceptionItpackSparseMatrixSbsij : public FEMException
{
public:
  /** typedefs from f2c.h  */
  typedef long   integer;
  typedef double doublereal;

  /**
   * Constructor. In order to construct this exception object, five parameters
   * must be provided: file, lineNumber, location and a detailed description
   * of the exception, and the invalid index
   */
  FEMExceptionItpackSparseMatrixSbsij(const char *file, unsigned int lineNumber, std::string location,
                                      integer errorCode);

  /** Virtual destructor needed for subclasses. Has to have empty throw(). */
  virtual ~FEMExceptionItpackSparseMatrixSbsij()
  ITK_NOEXCEPT ITK_OVERRIDE;

  /** Type related information. */
  itkTypeMacro(FEMExceptionItpackSparseMatrixSbsij, FEMException);
};
}
}  // end namespace itk::fem

#endif
