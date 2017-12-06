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

#ifndef itkFEMLinearSystemWrapper_h
#define itkFEMLinearSystemWrapper_h

#include "itkMacro.h"
#include "itkFEMSolution.h"
#include "itkFEMException.h"
#include "ITKFEMExport.h"

#include <vector>
#include <typeinfo>
#include <string>

namespace itk
{
namespace fem
{
/**
 * \class LinearSystemWrapper
 * \brief Defines all functions required by Solver class to allocate,
 *        assemble and solve a linear system of equation.
 *
 * Linear system is defined as A*x=B, where A is a square matrix and F
 * is a vector. Member functions are provided to access a specific element
 * within A and B. Objects of derived classes should make appropriate calls
 * to the numeric library in implementation of virtual functions to assemble
 * and solve the linear system.
 *
 * See comments for each virtual member for more information about how to
 * derive a new LinearSystemWrapper class. An example derived class
 * LinearSystemWrapperVNL is defined to use VNL sparse matrix representation
 * and solver.
 *
 * \sa Solver::SetLinearSystemWrapper
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT LinearSystemWrapper : public Solution
{
public:
  typedef LinearSystemWrapper Self;
  typedef Solution            Superclass;
  typedef Self *              Pointer;
  typedef const Self *        ConstPointer;

  typedef std::vector<unsigned int> ColumnArray;

  /**
   * Constructor for linear system, should perform any initialization that
   * is required by derived class.
   */
  LinearSystemWrapper() :
    m_Order(0), m_NumberOfMatrices(1), m_NumberOfVectors(1), m_NumberOfSolutions(1)
  {
  }
  /* , m_PrimaryMatrixSetupFunction(0), m_PrimaryVectorSetupFunction(0),
    m_PrimarySolutionSetupFunction(0) {} */

  /**
   * Virtual destructor should properly destroy the object and clean up any
   * memory allocated for matrix and vector storage.
   */
  virtual ~LinearSystemWrapper();

  /**
   * Clear all the data (matrices) inside the system, so that the system
   * is ready to solve another problem from scratch.
   */
  virtual void Clean();

  /**
   * Set the order of the system.  All matrices will be of size NxN and
   * all vectors will be of size N
   * \param N order of the linear system
   */
  void SetSystemOrder(unsigned int N)
  {
    m_Order = N;
  }

  /**
   * Get the order of the system
   */
  unsigned int GetSystemOrder() const
  {
    return m_Order;
  }

  /**
   * Set Index of matrices used by the system
   * \param nMatrices Index of matrices used by system
   */
  void SetNumberOfMatrices(unsigned int nMatrices)
  {
    m_NumberOfMatrices = nMatrices;
  }

  /*
   * Set the maximum number of entries permitted in a matrix
   * \param matrixIndex index of matrix to set value for
   * \param maxNonZeros maximum number of entries allowed in matrix
   * \note in general this function does nothing, however it may
   *       redefined by the derived wrapper if necessary
   */
  // virtual void SetMaximumNonZeroValuesInMatrix(unsigned int maxNonZeroValues)
  // = 0;

  /**
   * Get Index of matrices used by system
   */
  unsigned int GetNumberOfMatrices() const
  {
    return m_NumberOfMatrices;
  }

  /**
   * Set Index of vectors used by the system
   * \param nVectors Index of vectors used by system
   */
  void SetNumberOfVectors(unsigned int nVectors)
  {
    m_NumberOfVectors = nVectors;
  }

  /**
   * Get Index of vectors used by system
   */
  unsigned int GetNumberOfVectors() const
  {
    return m_NumberOfVectors;
  }

  /**
   * Set Index of solutions used by the system
   * \param nSolutions Index of solutions used by system
   */
  void SetNumberOfSolutions(unsigned int nSolutions)
  {
    m_NumberOfSolutions = nSolutions;
  }

  /**
   * Get Index of solutions used by system
   */
  unsigned int GetNumberOfSolutions() const
  {
    return m_NumberOfSolutions;
  }

  /**
   * Initialization of the A matrix. First any existing data for matrix A
   * must be be destroyed, and then a new matrix is created in the memory. All
   * elements in A must be set to zero.
   *
   * \param matrixIndex index of matrix to initialize
   */
  virtual void InitializeMatrix(unsigned int matrixIndex = 0) = 0;

  /**
   * Check to see if matrix is initialized
   * \param matrixIndex index of matrix to examine
   */
  virtual bool IsMatrixInitialized(unsigned int matrixIndex = 0) = 0;

  /**
   * Free the memory from a matrix
   * \param matrixIndex index of matrix to destroy
   */
  virtual void DestroyMatrix(unsigned int matrixIndex = 0) = 0;

  /**
   * Initialization of the a vector. First any existing data for vector B
   * must be destroyed, then new vector is created in the memory. All
   * elements in B must be set to zero.
   *
   */
  virtual void InitializeVector(unsigned int vectorIndex = 0) = 0;

  /**
   * Check to see if vector is initialized
   * \param vectorIndex vector of index to examine
   */
  virtual bool IsVectorInitialized(unsigned int vectorIndex = 0) = 0;

  /**
   * Free the memory from a vector
   * \param vectorIndex index of vector to destroy
   */
  virtual void DestroyVector(unsigned int vectorIndex = 0) = 0;

  /**
   * Initialization of a solution vector.  Existing memory must be destroyed
   * and the new solution vector is created in memory.  All values should
   * be set to zero.
   * \param solutionIndex index of solution vector to initialize
   */
  virtual void InitializeSolution(unsigned int solutionIndex = 0) = 0;

  /**
   * Check to see if solution vector is initialized
   * \param solutionIndex index of solution vector to examine
   */
  virtual bool IsSolutionInitialized(unsigned int solutionIndex = 0) = 0;

  /** Free the memory from a solution vector
   * \param solutionIndex index of solution vector to destroy
   */
  virtual void DestroySolution(unsigned int solutionIndex = 0) = 0;

  /**
   * Virtual function to get a value of a specific element of a matrix.
   * \param i row of the element
   * \param j column of the element
   * \param matrixIndex index of matrix to get value from
   */
  virtual Float GetMatrixValue(unsigned int i, unsigned int j, unsigned int matrixIndex = 0) const = 0;

  /**
   * Virtual function to set a value of a specific element of the A matrix.
   * \param i row of the element
   * \param j column of the element
   * \param value new value of the element
   * \param matrixIndex index of matrix to set value in
   */
  virtual void SetMatrixValue(unsigned int i, unsigned int j, Float value, unsigned int matrixIndex = 0) = 0;

  /**
   * Virtual function to add a value to a specific element of the A matrix.
   * \param i row of the element
   * \param j column of the element
   * \param value value to add to the existing element
   * \param matrixIndex index of matrix to add value to
   */
  virtual void AddMatrixValue(unsigned int i, unsigned int j, Float value, unsigned int matrixIndex = 0) = 0;

  /**
   * Returns the column index (zero based) of the i-th non zero
   * (non allocated)element in a given row of A matrix. This function
   * is useful for optimizations when sparse matrices are used. Note
   * that the value of an element with returned column index may actually
   * be equal zero.
   * \param row Row number
   * \param cols Which element in that row. Can range from 0 to number of
   *          elements allocated in a row. If this is out of range, the
   *          function returns -1.
   * \param matrixIndex Index of matrix (defaults to 0)
   */
  virtual void GetColumnsOfNonZeroMatrixElementsInRow(unsigned int row, ColumnArray & cols,
                                                      unsigned int matrixIndex = 0);

  /**
   * Virtual function to get a value of a specific element of the B vector.
   * \param i row of the element
   * \param vectorIndex index of vector to get value from
   */
  virtual Float GetVectorValue(unsigned int i, unsigned int vectorIndex = 0) const = 0;

  /**
   * Virtual function to set a value of a specific element of the B vector.
   * \param i row of the element
   * \param value new value of the element
   * \param vectorIndex index of vector to set value in
   */
  virtual void SetVectorValue(unsigned int i, Float value, unsigned int vectorIndex = 0) = 0;

  /**
   * Virtual function to add a value to a specific element of the B vector.
   * \param i row of the element
   * \param value value to add to the existing element
   * \param vectorIndex index of vector to add value to
   */
  virtual void AddVectorValue(unsigned int i, Float value, unsigned int vectorIndex = 0) = 0;

  /**
   * Virtual function to set a value of specific element of the solution
   * vector.
   * \param i element Index in solution vector
   * \param value new value of the element
   * \param solutionIndex index of solution vector to set value in
   */
  virtual void SetSolutionValue(unsigned int i, Float value, unsigned int solutionIndex = 0) = 0;

  /**
   * Virtual function to add a value of specific element of the solution
   * vector.
   * \param i element Index in solution vector
   * \param value new value of the element
   * \param solutionIndex index of solution vector to add value to
   */
  virtual void AddSolutionValue(unsigned int i, Float value, unsigned int solutionIndex = 0) = 0;

  /**
   * Solves the linear system and creates the solution vector, which can later
   * be accessed via GetSolutionValue(i,SolutionIndex) member function. Here all the major processing is
   * done with calls to external numeric library.
   * \note This function can only be called after the linear system was
   *       properly assembled.
   */
  virtual void Solve(void) = 0;

  /**
   * Swaps access indices of any 2 matrices in the linear system
   * \param matrixIndex1 index of a matrix to swap
   * \param matrixIndex2 index of matrix to swap with
   */
  virtual void SwapMatrices(unsigned int matrixIndex1, unsigned int matrixIndex2) = 0;

  /**
   * Copies the content of source matrix to destination matrix. Any existing
   * data in destination matrix is overwritten.
   *
   * \param matrixIndex1 index of a matrix that will be copied
   * \param matrixIndex2 index of matrix to copy to
   */
  virtual void CopyMatrix(unsigned int matrixIndex1, unsigned int matrixIndex2);

  /**
   * Swaps access indices of any 2 vectors in the linear system
   * \param vectorIndex1 index of a vector to swap
   * \param vectorIndex2 index of vector to swap with
   */
  virtual void SwapVectors(unsigned int vectorIndex1, unsigned int vectorIndex2) = 0;

  /**
   * Swaps access indices of any 2 solution vectors in the linear system
   * \param solutionIndex1 index of a solution vector to swap
   * \param solutionIndex2 index of solution vector to swap with
   */
  virtual void SwapSolutions(unsigned int solutionIndex1, unsigned int solutionIndex2) = 0;

  /**
   * Multiplies all elements of a matrix by a scalar
   * \param scale scalar to multiply all matrix values by
   * \param matrixIndex index of matrix to modify
   */
  virtual void ScaleMatrix(Float scale, unsigned int matrixIndex = 0);

  /**
   * Multiplies all elements of a vector by a scalar
   * \param scale scalar to multiply all vector values by
   * \param vectorIndex index of vector to modify
   */
  void ScaleVector(Float scale, unsigned int vectorIndex = 0);

  /**
   * Multiplies all elements of a solution by a scalar
   * \param scale scalar to multiply all solution values by
   * \param solutionIndex index of solution to modify
   */
  void ScaleSolution(Float scale, unsigned int solutionIndex = 0);

  /**
   * Perform a matrix*matrix operation and store the result in the linear system
   * \param leftMatrixIndex index of left matrix
   * \param rightMatrixIndex index of right matrix
   * \param resultMatrixIndex index of matrix where solution is stored
   */
  virtual void MultiplyMatrixMatrix(unsigned int resultMatrixIndex, unsigned int leftMatrixIndex,
                                    unsigned int rightMatrixIndex) = 0;

  /**
   * Adds two matrices storing the result in the first matrix.
   *
   * \param matrixIndex1 index of a matrix to add the other matrix to
   * \param matrixIndex2 index of matrix to add
   */
  virtual void AddMatrixMatrix(unsigned int matrixIndex1, unsigned int matrixIndex2);

  /**
   * Adds two vectors storing the result in the first vector.
   *
   * \param vectorIndex1 index of a vector to add the other vector to
   * \param vectorIndex2 index of vector to add
   */
  virtual void AddVectorVector(unsigned int vectorIndex1, unsigned int vectorIndex2);

  /**
   * Perform a matrix*vector operation and store the result in the linear system
   * \param matrixIndex index of matrix to multiply
   * \param vectorIndex index of vector to multiply
   * \param resultVectorIndex index of vector where result is store
   */
  virtual void MultiplyMatrixVector(unsigned int resultVectorIndex, unsigned int matrixIndex, unsigned int vectorIndex);

  /**
   * Perform a matrix*solution operation and store the result in the linear system
   * \param matrixIndex index of matrix to multiply
   * \param solutionIndex index of solution to multiply
   * \param resultVectorIndex index of vector where result is store
   */
  virtual void MultiplyMatrixSolution(unsigned int resultVectorIndex, unsigned int matrixIndex, unsigned int solutionIndex);

  /**
   * Copy a solution vector to a vector
   * \param solutionIndex index of solution vector to copy
   * \param vectorIndex index of vector to copy solution to
   */
  virtual void CopySolution2Vector(unsigned int solutionIndex, unsigned int vectorIndex) = 0;

  /**
   * Copy a vector to a solution vector
   * \param vectorIndex index of a vector to copy
   * \param solutionIndex index of a solution to copy the solution to
   */
  virtual void CopyVector2Solution(unsigned int vectorIndex, unsigned int solutionIndex) = 0;

  /**
   * Copy a vector
   * \param vectorSource index of a vector to copy
   * \param vectorDestination index to copy the vector to
   */
  virtual void CopyVector(unsigned int vectorSource, unsigned int vectorDestination);

  /**
   * Remove all zeros from a matrix
   * \param matrixIndex index of matrix to remove zeros from
   * \param tempMatrixIndex index of matrix to use for temp storage space
   * \note an extra matrix must be allocated by the solver in order to use this method
   */
  virtual void OptimizeMatrixStorage(unsigned int matrixIndex, unsigned int tempMatrixIndex);

  /**
   * Reorder the Degrees of Freedom in order to reduce bandwidth of matrix
   * \param matrixIndex index of matrix to examine
   * \param newNumbering vector of new degree of freedom ordering
   */
  virtual void ReverseCuthillMckeeOrdering(ColumnArray & newNumbering, unsigned int matrixIndex = 0);

protected:

  /** Order of linear system */
  unsigned int m_Order;

  /**
   * Number of matrices used by system
   */
  unsigned int m_NumberOfMatrices;

  /**
   * Number of vectors used by system
   */
  unsigned int m_NumberOfVectors;

  /**
   * Number of solutions used by system
   */
  unsigned int m_NumberOfSolutions;

  /*
   * Function used to prepare primary matrix for numerical solving
   */
  // void (*m_PrimaryMatrixSetupFunction)(LinearSystemWrapper *lsw);

  /*
   * Function used to prepare primary vector for numerical solving
   */
  /* void (*m_PrimaryVectorSetupFunction)(LinearSystemWrapper *lsw);*/

  /*
   * Function used to prepare primary matrix for numerical solving
   */
  /* void (*m_PrimarySolutionSetupFunction)(LinearSystemWrapper *lsw); */

private:

  /**
   * matrix reordering utility
   */
  void CuthillMckeeOrdering(ColumnArray & newNumbering, int startingRow, unsigned int matrixIndex = 0);

  void FollowConnectionsCuthillMckeeOrdering(unsigned int rowNumber, ColumnArray & rowDegree,
                                             ColumnArray & newNumbering, unsigned int nextRowNumber,
                                             unsigned int matrixIndex = 0);

  /** Copy constructor is not allowed. */
  LinearSystemWrapper(const LinearSystemWrapper &);

  /** Asignment operator is not allowed. */
  const LinearSystemWrapper & operator=(const LinearSystemWrapper &);

};

class ITK_ABI_EXPORT FEMExceptionLinearSystem : public FEMException
{
public:
  /**
   * Constructor. In order to construct this exception object, four parameters
   * must be provided: file, lineNumber, location and a detailed description
   * of the exception.
   */
  FEMExceptionLinearSystem(const char *file, unsigned int lineNumber, std::string location, std::string moreDescription);

  /** Virtual destructor needed for subclasses. Has to have empty throw(). */
  virtual ~FEMExceptionLinearSystem()
  ITK_NOEXCEPT ITK_OVERRIDE;

  /** Type related information. */
  itkTypeMacro(FEMExceptionLinearSystem, FEMException);
};

class ITK_ABI_EXPORT FEMExceptionLinearSystemBounds : public FEMException
{
public:
  /**
   * Constructor. In order to construct this exception object, five parameters
   * must be provided: file, lineNumber, location and a detailed description
   * of the exception, and the invalid index
   */
  FEMExceptionLinearSystemBounds(const char *file, unsigned int lineNumber, std::string location,
                                 std::string moreDescription,
                                 unsigned int index1);

  /**
   * Constructor. In order to construct this exception object, six parameters
   * must be provided: file, lineNumber, location and a detailed description
   * of the exception, the first index, and the second index   */
  FEMExceptionLinearSystemBounds(const char *file, unsigned int lineNumber, std::string location,
                                 std::string moreDescription, unsigned int index1,
                                 unsigned int index2);

  /** Virtual destructor needed for subclasses. Has to have empty throw(). */
  virtual ~FEMExceptionLinearSystemBounds()
  ITK_NOEXCEPT ITK_OVERRIDE;

  /** Type related information. */
  itkTypeMacro(FEMExceptionLinearSystem, FEMException);
};
}
}  // end namespace itk::fem

#endif // #ifndef itkFEMLinearSystemWrapper_h
