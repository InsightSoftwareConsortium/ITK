/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMItpackLinearSystemWrapper.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMItpackLinearSystemWrapper_h
#define __itkFEMItpackLinearSystemWrapper_h

#include "itpack_f2c.h"
#include "itpack.h"
#include "itkFEMSolution.h"
#include "itkFEMLinearSystemWrapper.h"
#include "itkFEMItpackSparseMatrix.h"
#include <vector>

namespace itk {
namespace fem {

/**
 * \class ItpackLinearSystemWrapper
 * \brief LinearSystemWrapper class that uses Itpack numeric library functions
 *        to define and solve a sparse linear system of equations
 * \sa LinearSystemWrapper
 */
class ItpackLinearSystemWrapper : public LinearSystemWrapper
{
public:
 
  /** Standard "Self" typedef. */
  typedef ItpackLinearSystemWrapper Self;

  /** Standard "Superclass" typedef. */
  typedef LinearSystemWrapper Superclass;

  /** itpack integer typedef */
  typedef itpack::integer integer;
  
  /** itpack double typedef */
  typedef itpack::doublereal doublereal;

  /** matrix representatin typedef */
  typedef ItpackSparseMatrix MatrixRepresentation;

  /** vector of matrices typedef */
  typedef std::vector<MatrixRepresentation> MatrixHolder;

  /* auto pointer to vector of matrices typedef */
  /* typedef std::auto_ptr<MatrixHolder> MatrixArrayPtr; */

  /** vector representation typedef */
  /* typedef std::auto_ptr<doublereal> VectorRepresentation; */
  typedef doublereal* VectorRepresentation;

  /** vector of vector typedef */
  typedef std::vector<VectorRepresentation> VectorHolder;

  /* auto pointer to vector of vectors typedef */
  /* typedef std::auto_ptr<VectorHolder> VectorArrayPtr; */

  /* pointer to array of unsigned int typedef */
  /* typedef std::auto_ptr<unsigned int> UnsignedIntegerArrayPtr; */

  /* -----------------------------------------------------------------
   * 
   * Routines for setting/reporting itpack parameters 
   *
   * ----------------------------------------------------------------- 
   */

  /**
   * Set the maximum number of iterations
   * \param i maximum number of iterations that may be performed
   */
  void SetMaximumNumberIterations(integer i) {   m_IPARM[0] = i; }

  /**
   * Get the maximum number iterations that may be performed
   */
  integer  GetMaximumNumberIterations() { return m_IPARM[0]; }

  //void SetErrorReportingLevel(integer i) {   m_IPARM[1] = i; }

  /**
   * Get a flag indicating the type of error reporting 
   */
  integer  GetErrorReportingLevel() { return m_IPARM[1]; }

  /**
   * Set the communication switch - meaningless in this implementation
   * \param i flag value
   */
  void SetCommunicationSwitch(integer i) {   m_IPARM[2] = i; }

  /**
   * Get the communication flag - meaningless in this implementation
   */
  integer  GetCommunicationSwitch() { return m_IPARM[2]; }

  //void SetOutputNumber(integer i) {   m_IPARM[3] = i; }

  /**
   * Get the output number - meaningless in this implementation
   */
  integer  GetOutputNumber() { return m_IPARM[3]; }

  /**
   * Set flag indicating symmetric matrix is being used
   * \param i 1=symmetric, 0=non-symmetric
   */
  void SetSymmetricMatrixFlag(integer i) {   m_IPARM[4] = i; }

  /** 
   * Get flag indicating use of symmetric matrix (1=symmetric, 0=non-symmetric)
   */
  integer  GetSymmetricMatrixFlag() { return m_IPARM[4]; }

  /**
   * Set flag for ???
   * \param i ??
   */
  void SetAdaptiveSwitch(integer i) {   m_IPARM[5] = i; }

  /**
   * Get flag indicating ??
   */
  integer  GetAdaptiveSwitch() { return m_IPARM[5]; }

  /**
   * Set flag for ??
   * \param i ??
   */
  void SetAdaptiveCaseSwitch(integer i) {   m_IPARM[6] = i; }

  /**
   * Get flag indicating ??
   */
  integer  GetAdaptiveCaseSwitch() { return m_IPARM[6]; }

  /**
   * Set size of workspace used by solver
   * \param i size of the workspace vector
   * \note this value is set by default
   */
  void SetWorkspaceUsed(integer i) {   m_IPARM[7] = i; }

  /**
   * Get the size of the workspace used by solver
   * \note after solver is called this is the amount of workspace actually used
   */
  integer  GetWorkspaceUsed() { return m_IPARM[7]; }

  /**
   * Set flag indicating use of red black ordering
   * \param i 1=red black ordering used, 0=not
   */
  void SetRedBlackOrderingSwitch(integer i) {   m_IPARM[8] = i; }

  /**
   * Get the flag indicating use of red black ordering
   */
  integer  GetRedBlackOrderingSwitch() { return m_IPARM[8]; }

  /**
   * Set flag indicating ??
   * \param i ??
   */
  void SetRemoveSwitch(integer i) {   m_IPARM[9] = i; }

  /**
   * Get flag indicating ??
   */
  integer  GetRemoveSwitch() { return m_IPARM[9]; }

  /**
   * Set the flag indicating use of timer routines - meaningless in this implementation
   * \param i flag
   */
  void SetTimingSwitch(integer i) {   m_IPARM[10] = i; }

  /**
   * Get the flag indicating use of the timer routines - meaningless in this implementation
   */
  integer  GetTimingSwitch() { return m_IPARM[10]; }

  /**
   * Set the flag for level of error reporting - meaningless in this implementation
   * \param i flag for level of error analysis
   */
  void SetErrorAnalysisSwitch(integer i) {   m_IPARM[11] = i; }

  /**
   * Get the flag for level of error reporting - meaningless in this implementation
   */
  integer  GetErrorAnalysisSwitch() { return m_IPARM[11]; }

  /**
   * Set the level of accuracy for an acceptable solution
   * \param i accuracy desired
   */
  void   SetAccuracy(doublereal i) {   m_RPARM[0] = i; }

  /**
   * Get the level of accuracy
   */
  doublereal GetAccuracy()    { return m_RPARM[0]; }

  /**
   * Set ??
   * \param i larges jacobian eigenvalue estimate
   */
  void   SetLargestJacobiEigenvalueEstimate(doublereal i) {   m_RPARM[1] = i; }

  /**
   * Get ??
   */
  doublereal GetLargestJacobiEigenvalueEstimate()    { return m_RPARM[1]; }

  /**
   * Set ??
   * \param i smalles jacobian eigenvalue estimate
   */
  void   SetSmallestJacobiEigenvalueEstimate(doublereal i) {   m_RPARM[2] = i; }

  /**
   * Get ??
   */
  doublereal GetSmallestJacobiEigenvalueEstimate()    { return m_RPARM[2]; }

  /**
   * Set the damping factor used by ??
   * \param i damping factor
   */
  void   SetDampingFactor(doublereal i) {   m_RPARM[3] = i; }

  /**
   * Get the damping factor used by ??
   */
  doublereal GetDampingFactor()    { return m_RPARM[3]; }

  /**
   * Set the over-relaxation parameter ??
   * \param i parameter
   */
  void   SetOverrelaxationParameter(doublereal i) {   m_RPARM[4] = i; }

  /**
   * Get the over-relaxation parameter ??
   */
  doublereal GetOverrelaxationParameter()    { return m_RPARM[4]; }

  /**
   * Set the ??
   * \param i ??
   */
  void   SetEstimatedSpectralRadiusSSOR(doublereal i) {   m_RPARM[5] = i; }

  /**
   * Get the ??
   */
  doublereal GetEstimatedSpectralRadiusSSOR()    { return m_RPARM[5]; }

  /**
   * Set the ??
   * \param i ??
   */
  void   SetEstimatedSpectralRadiusLU(doublereal i) {   m_RPARM[6] = i; }

  /**
   * Get the ??
   */
  doublereal GetEstimatedSpectralRadiusLU()    { return m_RPARM[6]; }

  /**
   * Set the tolerance level
   * \param i tolerance
   */
  void   SetTolerance(doublereal i) {   m_RPARM[7] = i; }

  /**
   * Get the tolerance level
   */
  doublereal GetTolerance()    { return m_RPARM[7]; }

  /**
   * Set the time to convergence
   * \param i ??
   */
  void   SetTimeToConvergence(doublereal i) {   m_RPARM[8] = i; }

  /**
   * Get the time to convergence
   */
  doublereal GetTimeToConvergence()    { return m_RPARM[8]; }

  /**
   * Set the time for call
   * \param i ??
   */
  void   SetTimeForCall(doublereal i) {   m_RPARM[9] = i; }

  /**
   * Get the time for call
   */
  doublereal GetTimeForCall()    { return m_RPARM[9]; }

  /**
   * Set digits in error
   * \param i number of digits in error
   */
  void   SetDigitsInError(doublereal i) {   m_RPARM[10] = i; }

  /**
   * Get the number of digits in the error
   */
  doublereal GetDigitsInError()    { return m_RPARM[10]; }

  /**
   * Set the number of digits in the residual
   * \param i number of digits in the residual
   */
  void   SetDigitsInResidual(doublereal i) {   m_RPARM[11] = i; }

  /**
   * Get the number of digits in the residual
   */
  doublereal GetDigitsInResidual()    { return m_RPARM[11]; }

  /** 
   * Set numerical solving method to jacobian conjugate gradient 
   */
  void JacobianConjugateGradient() { m_Method = 0; }

  /** 
   * Set numerical solving method to jacobian semi iterative
   */
  void JacobianSemiIterative() { m_Method = 1; }

  /** 
   * Set numerical solving method to successive over-relaxation
   */
  void SuccessiveOverrelaxation() { m_Method = 2; }

  /** 
   * Set numerical solving method to symmetric successive over-relaxation
   * conjugate gradient 
   */
  void SymmetricSuccessiveOverrelaxationConjugateGradient() { m_Method = 3; }

  /** 
   * Set numerical solving method to symmetric successive over-relaxation
   * successive over-relaxation
   */
  void SymmetricSuccessiveOverrelaxationSuccessiveOverrelaxation() { m_Method = 4; }

  /** 
   * Set numerical solving method to reduced system conjugate gradient 
   */
  void ReducedSystemConjugateGradient() { m_Method = 5; }

  /** 
   * Set numerical solving method to reduced system semi-iteration */
  void ReducedSystemSemiIteration() { m_Method = 6; }


  /* -----------------------------------------------------------------
   * 
   * Redefine methods defined in LinearSystemWrapper 
   *
   * ----------------------------------------------------------------- 
   */

  /**
   * set maximum number of entires in a matrix
   * \param matrixIndex index of matrix
   * \param maxNonZeroValues maximum number of entries allowed in matrix
   * \note this must be called before any matrices are initialized
   */  
  virtual void SetMaximumNonZeroValuesInMatrix(unsigned int matrixIndex, unsigned int maxNonZeroValues);


  void ScaleMatrix(Float scale, unsigned int matrixIndex);

  /* -----------------------------------------------------------------
   * 
   * Functions required by LinearSystemWrapper 
   *
   * ----------------------------------------------------------------- 
   */

  /**
   * constructor 
   */
  ItpackLinearSystemWrapper();
  
  /**
   * destructor
   */
  ~ItpackLinearSystemWrapper();


  /* memory management routines */
  virtual void  InitializeMatrix(unsigned int MatrixIndex);  

  virtual void  DestroyMatrix(unsigned int MatrixIndex);

  virtual void  InitializeVector(unsigned int VectorIndex);

  virtual void  DestroyVector(unsigned int MatrixIndex);

  virtual void  InitializeSolution(unsigned int SolutionIndex);

  virtual void  DestroySolution(unsigned int SolutionIndex);

  /* assembly & solving routines */
  virtual Float GetMatrixValue(unsigned int i, unsigned int j, unsigned int MatrixIndex) const;

  virtual void  SetMatrixValue(unsigned int i, unsigned int j, Float value, unsigned int MatrixIndex);

  virtual void  AddMatrixValue(unsigned int i, unsigned int j, Float value, unsigned int MatrixIndex);

  virtual Float GetVectorValue(unsigned int i, unsigned int VectorIndex) const;

  virtual void  SetVectorValue(unsigned int i, Float value, unsigned int VectorIndex);

  virtual void  AddVectorValue(unsigned int i, Float value, unsigned int VectorIndex);

  virtual Float GetSolutionValue(unsigned int i, unsigned int SolutionIndex) const;

  virtual void  SetSolutionValue(unsigned int i, Float value, unsigned int SolutionIndex);

  virtual void  AddSolutionValue(unsigned int i, Float value, unsigned int SolutionIndex);

  virtual void  Solve(void);


  /* matrix & vector manipulation routines */
  virtual void  SwapMatrices(unsigned int MatrixIndex1, unsigned int MatrixIndex2);

  virtual void  SwapVectors(unsigned int VectorIndex1, unsigned int VectorIndex2);

  virtual void  SwapSolutions(unsigned int SolutionIndex1, unsigned int SolutionIndex2);

  virtual void  CopySolution2Vector(unsigned SolutionIndex, unsigned int VectorIndex);

  virtual void  MultiplyMatrixMatrix(unsigned int ResultMatrixIndex, unsigned int LeftMatrixIndex, unsigned int RightMatrixIndex);

  virtual void  MultiplyMatrixVector(unsigned int ResultVectorIndex, unsigned int MatrixIndex, unsigned int VectorIndex);

private:

  /** pointer to vector of matrices */
  MatrixHolder *m_Matrices;

  /** pointer to vector of force arrays */
  VectorHolder *m_Vectors;

  /** pointer to vector of solution arrays */
  VectorHolder *m_Solutions;

  /** pointer to array of unsigned int's indicating max number of entries in each matrix */
  //UnsignedIntegerArrayPtr m_MaximumNonZeroValues;
  unsigned int *m_MaximumNonZeroValues;

  /** Array of pointers to available solver functions */
  int (*m_Methods[7])(integer *, integer *, integer *, doublereal *, doublereal *, doublereal *, integer *, integer *, doublereal *, 
       integer *, doublereal *, integer *);

  /** flag indicating which solver function should be used */
  integer m_Method;

  /** vector of length 12 used to initialize various parameters on input */
  integer m_IPARM[12];

  /** vector of length 12 used to initialize various parameters on input */
  doublereal m_RPARM[12];

};

}} // end namespace itk::fem

#endif // #ifndef __itkFEMItpackLinearSystemWrapper_h


