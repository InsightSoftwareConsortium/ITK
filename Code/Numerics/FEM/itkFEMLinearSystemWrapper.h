/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLinearSystemWrapper.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMLinearSystemWrapper_h
#define __itkFEMLinearSystemWrapper_h 

#include "itkFEMSolution.h"
#include <vector>

namespace itk {
namespace fem {




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
 */
class LinearSystemWrapper : public Solution
{
public:
  /** Standard "Self" typedef.*/
  typedef LinearSystemWrapper Self;
  /** Standard "Superclass" typedef. */
  typedef Solution Superclass;
  /**  Pointer to an object. */
  typedef Self* Pointer;
  /**  Const pointer to an object. */
  typedef const Self* ConstPointer;

  typedef std::vector<unsigned int> ColumnArray;

  /**
   * Constructor for linear system, should perform any initialization that
   * is required by derived class.
   */
  LinearSystemWrapper() 
    : m_Order(0), m_NumberOfMatrices(1), m_NumberOfVectors(1), m_NumberOfSolutions(1) {}
      /* , m_PrimaryMatrixSetupFunction(0), m_PrimaryVectorSetupFunction(0), m_PrimarySolutionSetupFunction(0) {} */

  /**
   * Virtual destructor should properly destroy the object and clean up any
   * memory allocated for matrix and vector storage.
   */
  virtual ~LinearSystemWrapper() {};

  /**
   * Set the order of the system.  All matrices will be of size NxN and 
   * all vectors will be of size N
   * \param N order of the linear system
   */
  void SetSystemOrder(unsigned int N) { m_Order = N; }

  /**
   * Get the order of the system
   */
  unsigned int GetSystemOrder() const { return m_Order; }

  /**
   * Set Index of matrices used by the system
   * \param nMatrices Index of matrices used by system
   */
  void SetNumberOfMatrices(unsigned int nMatrices) { m_NumberOfMatrices = nMatrices; }

  /*
   * Set the maximum number of entries permitted in a matrix
   * \param matrixIndex index of matrix to set value for
   * \param maxNonZeros maximum number of entries allowed in matrix
   * \note in general this function does nothing, however it may 
   *       redefined by the derived wrapper if necessary
   */
  //virtual void SetMaximumNonZeroValuesInMatrix(unsigned int maxNonZeroValues) = 0;

  /**
   * Get Index of matrices used by system
   */
  unsigned int GetNumberOfMatrices() { return m_NumberOfMatrices; }

  /**
   * Set Index of vectors used by the system
   * \param nVectors Index of vectors used by system
   */
  void SetNumberOfVectors(unsigned int nVectors) { m_NumberOfVectors = nVectors; }

  /**
   * Get Index of vectors used by system
   */
  unsigned int GetNumberOfVectors() { return m_NumberOfVectors; }

  /**
   * Set Index of solutions used by the system
   * \param nSolutions Index of solutions used by system
   */
  void SetNumberOfSolutions(unsigned int nSolutions) { m_NumberOfSolutions = nSolutions; }

  /**
   * Get Index of solutions used by system
   */
  unsigned int GetNumberOfSolutions() { return m_NumberOfSolutions; }

  /**
   * Initialization of the A matrix. First any existing data for matrix A 
   * must be be destroyed, and then a new matrix is created in the memory. All
   * elements in A must be set to zero. 
   *
   * \param MatrixIndex index of matrix to initialize
   */
  virtual void InitializeMatrix(unsigned int matrixIndex = 0) = 0;


  /**
   * Check to see if matrix is initialized
   * \param matrixIndex index of matrix to examine
   */
  virtual bool IsMatrixInitialized(unsigned int matrixIndex = 0) = 0;

  /**
   * Free the memory from a matrix
   * \param MatrixIndex index of matrix to destroy
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
   * \param VectorIndex index of vector to destroy
   */
  virtual void DestroyVector(unsigned int vectorIndex = 0) = 0;

  /**
   * Initialization of a solution vector.  Existing memory must be destroyed
   * and the new solution vector is created in memory.  All values should
   * be set to zero.
   * \param SolutionIndex index of solution vector to initialize
   */
  virtual void InitializeSolution(unsigned int solutionIndex = 0) = 0;

  /**
   * Check to see if solution vector is initialized
   * \param solutionIndex index of solution vector to examine
   */
  virtual bool IsSolutionInitialized(unsigned int solutionIndex = 0) = 0;

  /** Free teh mememory from a solution vector
   * \param SolutionIndex index of solution vector to destroy
   */
  virtual void DestroySolution(unsigned int solutionIndex = 0) = 0;

  /**
   * Virtual function to get a value of a specific element of a matrix.
   * \param i row of the element
   * \param j column of the element
   * \param MatrixIndex index of matrix to get value from
   */
  virtual Float GetMatrixValue(unsigned int i, unsigned int j, unsigned int matrixIndex = 0) const = 0;

  /**
   * Virtual function to set a value of a specific element of the A matrix.
   * \param i row of the element
   * \param j column of the element
   * \param value new value of the element
   * \param MatrixValue index of matrix to set value in
   */
  virtual void SetMatrixValue(unsigned int i, unsigned int j, Float value, unsigned int matrixIndex = 0) = 0;

  /**
   * Virtual function to add a value to a specific element of the A matrix.
   * \param i row of the element
   * \param j column of the element
   * \param value value to add to the existing element
   * \param MatrixIndex index of matrix to add value to
   */
  virtual void AddMatrixValue(unsigned int i, unsigned int j, Float value, unsigned int matrixIndex = 0) = 0;

  /**
   * Returns the column index (zero based) of the i-th non zero
   * (non allocated)element in a given row of A matrix. This function
   * is usefull for optimizations when sparse matrices are used. Note
   * that the value of an element with returned column index may actually
   * be equal zero.
   * \param row Row number
   * \param i Which element in that row. Can range from 0 to number of
   *          elements allocated in a row. If this is out of range, the
   *          function returns -1.
   * \param MatrixIndex Index of matrix (defaults to 0)
   */
  virtual void GetColumnsOfNonZeroMatrixElementsInRow( unsigned int row, ColumnArray& cols, unsigned int matrixIndex = 0 );

  /**
   * Virtual function to get a value of a specific element of the B vector.
   * \param i row of the element
   * \param VectorIndex index of vector to get value from
   */
  virtual Float GetVectorValue(unsigned int i, unsigned int vectorIndex = 0) const = 0;

  /**
   * Virtual function to set a value of a specific element of the B vector.
   * \param i row of the element
   * \param value new value of the element
   * \param VectorIndex index of vector to set value in
   */
  virtual void SetVectorValue(unsigned int i, Float value, unsigned int vectorIndex = 0) = 0;

  /**
   * Virtual function to add a value to a specific element of the B vector.
   * \param i row of the element
   * \param value value to add to the existing element
   * \param VectorIndex index of vector to add value to
   */
  virtual void AddVectorValue(unsigned int i, Float value, unsigned int vectorIndex = 0) = 0;

  /**
   * Virtual function to set a value of specific element of the solution
   * vector.
   * \param i element Index in solution vector
   * \param value new value of the element
   * \param SolutionIndex index of solution vector to set value in
   */
  virtual void SetSolutionValue(unsigned int i, Float value, unsigned int solutionIndex = 0) = 0;

  /**
   * Virtual function to add a value of specific element of the solution
   * vector.
   * \param i element Index in solution vector
   * \param value new value of the element
   * \param SolutionIndex index of solution vector to add value to
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
   * \param MatrixIndex1 index of a matrix to swap
   * \param MatrixIndex2 index of matrix to swap with
   */
  virtual void SwapMatrices(unsigned int matrixIndex1, unsigned int matrixIndex2) = 0;

  /** 
   * Swaps access indices of any 2 vectors in the linear system
   * \param VectorIndex1 index of a vector to swap
   * \param VectorIndex2 index of vector to swap with
   */
  virtual void SwapVectors(unsigned int vectorIndex1, unsigned int vectorIndex2) = 0;

  /** 
   * Swaps access indices of any 2 solution vectors in the linear system
   * \param SolutionIndex1 index of a solution vector to swap
   * \param SolutionIndex2 index of solution vector to swap with
   */
  virtual void SwapSolutions(unsigned int solutionIndex1, unsigned int solutionIndex2) = 0;


  /**
   * Multiplies all elements of a matrix by a scalar
   * \param scale scalar to multiply all matrix values by
   * \param matrixIndex index of matrix to modify
   */
  void ScaleMatrix(Float scale, unsigned int matrixIndex = 0);


  /**
   * Multiplies all elements of a vector by a scalar
   * \param scale scalar to multiply all vector values by
   * \param vectorIndex index of vector to modify
   */
  void ScaleVector(Float scale, unsigned int vectorIndex = 0);


  /**
   * Multiplies all elements of a solution by a scalar
   * \param scale scalar to multiply all solution values by
   * \param matrixIndex index of solution to modify
   */
  void ScaleSolution(Float scale, unsigned int solutionIndex = 0);

  /**
   * Perform a matrix*matrix operation and store the result in the linear system
   * \param LeftMatrixIndex index of left matrix
   * \param RightMatrixIndex index of right matrix
   * \param ResultMatrixIndex index of matrix where solution is stored
   */
  virtual void MultiplyMatrixMatrix(unsigned int resultMatrixIndex, unsigned int leftMatrixIndex, unsigned int rightMatrixIndex) = 0;

  /**
   * Perform a matrix*vector operation and store the result in the linear system
   * \param MatrixIndex index of matrix to multiply
   * \param VectorIndex index of vector to multiply
   * \param ResultVectorIndex index of vector where result is store
   */
  void MultiplyMatrixVector(unsigned int resultVectorIndex, unsigned int matrixIndex, unsigned int vectorIndex);

  /**
   * Copy a solution vector to a vector
   * \param SolutionIndex index of solution vector to copy
   * \param VectorIndex index of vector to copy solution to
   */
  virtual void CopySolution2Vector(unsigned int solutionIndex, unsigned int vectorIndex) = 0;

  /*
   * Sets the function used to prepare the primary system matrix for numerical solving
   * \param SetupFunction pointer to function that stores the matrix to 
   * solve in the 0 matrix of the linear system
   */
  /*
  void SetPrimaryMatrixSetupFunction(void (*SetupFunction)(LinearSystemWrapper *lsw))
  { 
    m_PrimaryMatrixSetupFunction = SetupFunction; 
  }
  */


  /*
   * Sets the function used to prepare the primary system vector for numerical solving
   * \param SetupFunction pointer to function that stores the vector to
   * solve in the 0 vector of the linear system
   */
  /*
  void SetPrimaryVectorSetupFunction(void (*SetupFunction)(LinearSystemWrapper *lsw))
  { 
    m_PrimaryVectorSetupFunction = SetupFunction; 
  }
  */

  /*
   * Sets the function used to prepare the primary system solution for numerical solving
   * \param SetupFunction pointer to function that stores the solution 
   * in the 0 solution vector of the linear system
   */
  /*
  void SetPrimarySolutionSetupFunction(void (*SetupFunction)(LinearSystemWrapper *lsw))
  { 
    m_PrimarySolutionSetupFunction = SetupFunction; 
  }
  */

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
  //void (*m_PrimaryMatrixSetupFunction)(LinearSystemWrapper *lsw);

  /*
   * Function used to prepare primary vector for numerical solving 
   */
  /* void (*m_PrimaryVectorSetupFunction)(LinearSystemWrapper *lsw);*/

  /*
   * Function used to prepare primary matrix for numerical solving 
   */
  /* void (*m_PrimarySolutionSetupFunction)(LinearSystemWrapper *lsw); */

private:

  /** Copy constructor is not allowed. */
  LinearSystemWrapper(const LinearSystemWrapper&);

  /** Asignment operator is not allowed. */
  const LinearSystemWrapper& operator= (const LinearSystemWrapper&);

};

}} // end namespace itk::fem

#endif // #ifndef __itkFEMLinearSystemWrapper_h
