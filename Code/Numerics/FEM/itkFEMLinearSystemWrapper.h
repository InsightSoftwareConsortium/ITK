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
class LinearSystemWrapper
{
public:
  /** Floating point storage type used within a class */
  typedef double Float;

  /**
   * Constructor for linear system, should perform any initialization that
   * is required by derived class.
   */
  LinearSystemWrapper() {}

  /**
   * Virtual destructor should properly destroy the object and clean up any
   * memory allocated for matrix and vector storage.
   */
  virtual ~LinearSystemWrapper() {};



  /**
   * Initialization of the A matrix. First any existing data for matrix A and
   * vector B must be destroyed, then new matrix is created in the memory. All
   * elements in A must be set to zero. Vector B remains undefined after the
   * call to InitA. Where and how the matrix A is stored entirely depentd on
   * how the derived class is implemented.
   *
   * \param N Number of equations (size of matrix A and vector B)
   */
  virtual void InitA(int N) = 0;

  /**
   * Initialization of the B vector. First any existing data for vector B
   * must be destroyed, then new vector is created in the memory. All
   * elements in B must be set to zero.
   *
   * \note Number of equations should be taken from the A matrix, and this
   *       function should fail if the matrix A is uninitialized.
   */
  virtual void InitB(void) = 0;

  /**
   * Virtual function to get a value of a specific element of the A matrix.
   * \param i row of the element
   * \param j column of the element
   */
  virtual Float GetA(int i, int j) = 0;

  /**
   * Virtual function to set a value of a specific element of the A matrix.
   * \param i row of the element
   * \param j column of the element
   * \param value new value of the element
   */
  virtual void SetA(int i, int j, Float value) = 0;

  /**
   * Virtual function to add a value to a specific element of the A matrix.
   * \param i row of the element
   * \param j column of the element
   * \param value value to add to the existing element
   */
  virtual void AddA(int i, int j, Float value) = 0;

  /**
   * Virtual function to access a specific element of the B vector. Function
   * must return a valid reference to element that can be modified.
   * \param i element number in vector B
   */

  /**
   * Virtual function to get a value of a specific element of the B vector.
   * \param i row of the element
   */
  virtual Float GetB(int i) = 0;

  /**
   * Virtual function to set a value of a specific element of the B vector.
   * \param i row of the element
   * \param value new value of the element
   */
  virtual void SetB(int i, Float value) = 0;

  /**
   * Virtual function to add a value to a specific element of the B vector.
   * \param i row of the element
   * \param value value to add to the existing element
   */
  virtual void AddB(int i, Float value) = 0;

  /**
   * Virtual function to get a value of specific element of the solution
   * vector.
   * \param i element number in solution vector
   * \note This function can only be called after the linear system was
   *       succesfully solved by calling the Solve() function.
   */
  virtual Float GetX(int i) = 0;

  /**
   * Virtual function to set a value of specific element of the solution
   * vector.
   * \param i element number in solution vector
   * \param value new value of the element
   */
  virtual void SetX(int i, Float value) = 0;

  /**
   * Solves the linear system and creates the solution vector, which can later
   * be accessed via x(i) member function. Here all the major processing is
   * done with calls to external numeric library.
   * \note This function can only be called after the linear system was
   *       properly assembled.
   */
  virtual void Solve(void) = 0;


private:
  /** Copy constructor is not allowed. */
  LinearSystemWrapper(const LinearSystemWrapper&);
  /** Asignment operator is not allowed. */
  const LinearSystemWrapper& operator= (const LinearSystemWrapper&);

};




}} // end namespace itk::fem

#endif // #ifndef __itkFEMLinearSystemWrapper_h
