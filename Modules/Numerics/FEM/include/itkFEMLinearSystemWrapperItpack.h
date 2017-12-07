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

#ifndef itkFEMLinearSystemWrapperItpack_h
#define itkFEMLinearSystemWrapperItpack_h

#include "itkFEMSolution.h"
#include "itkFEMLinearSystemWrapper.h"
#include "itkFEMItpackSparseMatrix.h"
#include "ITKFEMExport.h"
#include <vector>

/** Array of pointers to available solver functions */
/** typedefs from f2c.h  */
typedef long   integer;
typedef double doublereal;

extern "C" {
typedef
int ( *ItkItpackSolverFunction )(integer *, integer *, integer *, doublereal *, doublereal *, doublereal *, integer *,
                                 integer *, doublereal *,
                                 integer *, doublereal *, integer *);
}

namespace itk
{
namespace fem
{
/**
 * \class LinearSystemWrapperItpack
 * \brief LinearSystemWrapper class that uses Itpack numeric library functions
 *        to define and solve a sparse linear system of equations
 * \sa LinearSystemWrapper
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT LinearSystemWrapperItpack : public LinearSystemWrapper
{
public:

  /** Standard "Self" typedef. */
  typedef LinearSystemWrapperItpack Self;

  /** Standard "Superclass" typedef. */
  typedef LinearSystemWrapper Superclass;

  /** matrix representatin typedef */
  typedef ItpackSparseMatrix MatrixRepresentation;

  /** vector of matrices typedef */
  typedef std::vector<MatrixRepresentation> MatrixHolder;

  /* auto pointer to vector of matrices typedef */
  /* typedef std::auto_ptr<MatrixHolder> MatrixArrayPtr; */

  /** vector representation typedef */
  /* typedef std::auto_ptr<double> VectorRepresentation; */
  typedef double *VectorRepresentation;

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
  void SetMaximumNumberIterations(int i)
  {
    m_IPARM[0] = i;
  }

  /**
   * Get the maximum number iterations that may be performed
   */
  int  GetMaximumNumberIterations() const
  {
    return m_IPARM[0];
  }

  // void SetErrorReportingLevel(int i) {   m_IPARM[1] = i; }

  /**
   * Get a flag indicating the type of error reporting
   */
  int  GetErrorReportingLevel() const
  {
    return m_IPARM[1];
  }

  /**
   * Set the communication switch - meaningless in this implementation
   * \param i flag value
   */
  void SetCommunicationSwitch(int i)
  {
    m_IPARM[2] = i;
  }

  /**
   * Get the communication flag - meaningless in this implementation
   */
  int  GetCommunicationSwitch() const
  {
    return m_IPARM[2];
  }

  // void SetOutputNumber(int i) {   m_IPARM[3] = i; }

  /**
   * Get the output number - meaningless in this implementation
   */
  int  GetOutputNumber() const
  {
    return m_IPARM[3];
  }

  /**
   * Set flag indicating symmetric matrix is being used
   * \param i 1=symmetric, 0=non-symmetric
   */
  void SetSymmetricMatrixFlag(int i)
  {
    m_IPARM[4] = i;
  }

  /**
   * Get flag indicating use of symmetric matrix (1=symmetric, 0=non-symmetric)
   */
  int  GetSymmetricMatrixFlag()
  {
    return m_IPARM[4];
  }

  /**
   * Set flag for ???
   * \param i ??
   */
  void SetAdaptiveSwitch(int i)
  {
    m_IPARM[5] = i;
  }

  /**
   * Get flag indicating ??
   */
  int  GetAdaptiveSwitch() const
  {
    return m_IPARM[5];
  }

  /**
   * Set flag for ??
   * \param i ??
   */
  void SetAdaptiveCaseSwitch(int i)
  {
    m_IPARM[6] = i;
  }

  /**
   * Get flag indicating ??
   */
  int  GetAdaptiveCaseSwitch() const
  {
    return m_IPARM[6];
  }

  /**
   * Set size of workspace used by solver
   * \param i size of the workspace vector
   * \note this value is set by default
   */
  void SetWorkspaceUsed(int i)
  {
    m_IPARM[7] = i;
  }

  /**
   * Get the size of the workspace used by solver
   * \note after solver is called this is the amount of workspace actually used
   */
  int  GetWorkspaceUsed()
  {
    return m_IPARM[7];
  }

  /**
   * Set flag indicating use of red black ordering
   * \param i 1=red black ordering used, 0=not
   */
  void SetRedBlackOrderingSwitch(int i)
  {
    m_IPARM[8] = i;
  }

  /**
   * Get the flag indicating use of red black ordering
   */
  int  GetRedBlackOrderingSwitch()
  {
    return m_IPARM[8];
  }

  /**
   * Set flag indicating ??
   * \param i ??
   */
  void SetRemoveSwitch(int i)
  {
    m_IPARM[9] = i;
  }

  /**
   * Get flag indicating ??
   */
  int  GetRemoveSwitch()
  {
    return m_IPARM[9];
  }

  /**
   * Set the flag indicating use of timer routines - meaningless in this implementation
   * \param i flag
   */
  void SetTimingSwitch(int i)
  {
    m_IPARM[10] = i;
  }

  /**
   * Get the flag indicating use of the timer routines - meaningless in this implementation
   */
  int  GetTimingSwitch()
  {
    return m_IPARM[10];
  }

  /**
   * Set the flag for level of error reporting - meaningless in this implementation
   * \param i flag for level of error analysis
   */
  void SetErrorAnalysisSwitch(int i)
  {
    m_IPARM[11] = i;
  }

  /**
   * Get the flag for level of error reporting - meaningless in this implementation
   */
  int  GetErrorAnalysisSwitch() const
  {
    return m_IPARM[11];
  }

  /**
   * Set the level of accuracy for an acceptable solution
   * \param i accuracy desired
   */
  void   SetAccuracy(double i)
  {
    m_RPARM[0] = i;
  }

  /**
   * Get the level of accuracy
   */
  double GetAccuracy() const
  {
    return m_RPARM[0];
  }

  /**
   * Set ??
   * \param i larges jacobian eigenvalue estimate
   */
  void   SetLargestJacobiEigenvalueEstimate(double i)
  {
    m_RPARM[1] = i;
  }

  /**
   * Get ??
   */
  double GetLargestJacobiEigenvalueEstimate() const
  {
    return m_RPARM[1];
  }

  /**
   * Set ??
   * \param i smalles jacobian eigenvalue estimate
   */
  void   SetSmallestJacobiEigenvalueEstimate(double i)
  {
    m_RPARM[2] = i;
  }

  /**
   * Get ??
   */
  double GetSmallestJacobiEigenvalueEstimate()
  {
    return m_RPARM[2];
  }

  /**
   * Set the damping factor used by ??
   * \param i damping factor
   */
  void   SetDampingFactor(double i)
  {
    m_RPARM[3] = i;
  }

  /**
   * Get the damping factor used by ??
   */
  double GetDampingFactor() const
  {
    return m_RPARM[3];
  }

  /**
   * Set the over-relaxation parameter ??
   * \param i parameter
   */
  void   SetOverrelaxationParameter(double i)
  {
    m_RPARM[4] = i;
  }

  /**
   * Get the over-relaxation parameter ??
   */
  double GetOverrelaxationParameter()
  {
    return m_RPARM[4];
  }

  /**
   * Set the ??
   * \param i ??
   */
  void   SetEstimatedSpectralRadiusSSOR(double i)
  {
    m_RPARM[5] = i;
  }

  /**
   * Get the ??
   */
  double GetEstimatedSpectralRadiusSSOR() const
  {
    return m_RPARM[5];
  }

  /**
   * Set the ??
   * \param i ??
   */
  void   SetEstimatedSpectralRadiusLU(double i)
  {
    m_RPARM[6] = i;
  }

  /**
   * Get the ??
   */
  double GetEstimatedSpectralRadiusLU() const
  {
    return m_RPARM[6];
  }

  /**
   * Set the tolerance level
   * \param i tolerance
   */
  void   SetTolerance(double i)
  {
    m_RPARM[7] = i;
  }

  /**
   * Get the tolerance level
   */
  double GetTolerance()
  {
    return m_RPARM[7];
  }

  /**
   * Set the time to convergence
   * \param i ??
   */
  void   SetTimeToConvergence(double i)
  {
    m_RPARM[8] = i;
  }

  /**
   * Get the time to convergence
   */
  double GetTimeToConvergence()
  {
    return m_RPARM[8];
  }

  /**
   * Set the time for call
   * \param i ??
   */
  void   SetTimeForCall(double i)
  {
    m_RPARM[9] = i;
  }

  /**
   * Get the time for call
   */
  double GetTimeForCall()
  {
    return m_RPARM[9];
  }

  /**
   * Set digits in error
   * \param i number of digits in error
   */
  void   SetDigitsInError(double i)
  {
    m_RPARM[10] = i;
  }

  /**
   * Get the number of digits in the error
   */
  double GetDigitsInError() const
  {
    return m_RPARM[10];
  }

  /**
   * Set the number of digits in the residual
   * \param i number of digits in the residual
   */
  void   SetDigitsInResidual(double i)
  {
    m_RPARM[11] = i;
  }

  /**
   * Get the number of digits in the residual
   */
  double GetDigitsInResidual() const
  {
    return m_RPARM[11];
  }

  /**
   * Set numerical solving method to jacobian conjugate gradient
   */
  void JacobianConjugateGradient()
  {
    m_Method = 0;
  }

  /**
   * Set numerical solving method to jacobian semi iterative
   */
  void JacobianSemiIterative()
  {
    m_Method = 1;
  }

  /**
   * Set numerical solving method to successive over-relaxation
   */
  void SuccessiveOverrelaxation()
  {
    m_Method = 2;
  }

  /**
   * Set numerical solving method to symmetric successive over-relaxation
   * conjugate gradient
   */
  void SymmetricSuccessiveOverrelaxationConjugateGradient()
  {
    m_Method = 3;
  }

  /**
   * Set numerical solving method to symmetric successive over-relaxation
   * successive over-relaxation
   */
  void SymmetricSuccessiveOverrelaxationSuccessiveOverrelaxation()
  {
    m_Method = 4;
  }

  /**
   * Set numerical solving method to reduced system conjugate gradient
   */
  void ReducedSystemConjugateGradient()
  {
    m_Method = 5;
  }

  /**
   * Set numerical solving method to reduced system semi-iteration */
  void ReducedSystemSemiIteration()
  {
    m_Method = 6;
  }

  /** -----------------------------------------------------------------
   *
   * Redefine methods defined in LinearSystemWrapper
   *
   * -----------------------------------------------------------------
   */

  /**
   * set maximum number of entires in a matrix
   * \param maxNonZeroValues maximum number of entries allowed in matrix
   * \note this must be called before any matrices are initialized
   */
  virtual void SetMaximumNonZeroValuesInMatrix(unsigned int maxNonZeroValues)
  {
    m_MaximumNonZeroValues =
      maxNonZeroValues;
  }

  virtual void ScaleMatrix(Float scale, unsigned int matrixIndex) ITK_OVERRIDE;

  /** -----------------------------------------------------------------
   *
   * Functions required by LinearSystemWrapper
   *
   * -----------------------------------------------------------------
   */

  /**
   * constructor
   */
  LinearSystemWrapperItpack();

  /**
   * destructor
   */
  ~LinearSystemWrapperItpack() ITK_OVERRIDE;

  /* memory management routines */
  virtual void  InitializeMatrix(unsigned int matrixIndex) ITK_OVERRIDE;

  virtual bool  IsMatrixInitialized(unsigned int matrixIndex) ITK_OVERRIDE;

  virtual void  DestroyMatrix(unsigned int matrixIndex) ITK_OVERRIDE;

  virtual void  InitializeVector(unsigned int vectorIndex) ITK_OVERRIDE;

  virtual bool  IsVectorInitialized(unsigned int vectorIndex) ITK_OVERRIDE;

  virtual void  DestroyVector(unsigned int vectorIndex) ITK_OVERRIDE;

  virtual void  InitializeSolution(unsigned int solutionIndex) ITK_OVERRIDE;

  virtual bool  IsSolutionInitialized(unsigned int solutionIndex) ITK_OVERRIDE;

  virtual void  DestroySolution(unsigned int solutionIndex) ITK_OVERRIDE;

  /* assembly & solving routines */
  virtual Float GetMatrixValue(unsigned int i, unsigned int j, unsigned int matrixIndex) const ITK_OVERRIDE;

  virtual void  SetMatrixValue(unsigned int i, unsigned int j, Float value, unsigned int matrixIndex) ITK_OVERRIDE;

  virtual void  AddMatrixValue(unsigned int i, unsigned int j, Float value, unsigned int matrixIndex) ITK_OVERRIDE;

  virtual void GetColumnsOfNonZeroMatrixElementsInRow(unsigned int row, ColumnArray & cols, unsigned int matrixIndex) ITK_OVERRIDE;

  virtual Float GetVectorValue(unsigned int i, unsigned int vectorIndex) const ITK_OVERRIDE;

  virtual void  SetVectorValue(unsigned int i, Float value, unsigned int vectorIndex) ITK_OVERRIDE;

  virtual void  AddVectorValue(unsigned int i, Float value, unsigned int vectorIndex) ITK_OVERRIDE;

  virtual Float GetSolutionValue(unsigned int i, unsigned int solutionIndex) const ITK_OVERRIDE;

  virtual void  SetSolutionValue(unsigned int i, Float value, unsigned int solutionIndex) ITK_OVERRIDE;

  virtual void  AddSolutionValue(unsigned int i, Float value, unsigned int solutionIndex) ITK_OVERRIDE;

  virtual void  Solve(void) ITK_OVERRIDE;

  /* matrix & vector manipulation routines */
  virtual void  SwapMatrices(unsigned int matrixIndex1, unsigned int matrixIndex2) ITK_OVERRIDE;

  virtual void  SwapVectors(unsigned int vectorIndex1, unsigned int vectorIndex2) ITK_OVERRIDE;

  virtual void  SwapSolutions(unsigned int solutionIndex1, unsigned int solutionIndex2) ITK_OVERRIDE;

  virtual void  CopySolution2Vector(unsigned solutionIndex, unsigned int vectorIndex) ITK_OVERRIDE;

  virtual void  CopyVector2Solution(unsigned int vectorIndex, unsigned int solutionIndex) ITK_OVERRIDE;

  virtual void  MultiplyMatrixMatrix(unsigned int resultMatrixIndex, unsigned int leftMatrixIndex,
                                     unsigned int rightMatrixIndex) ITK_OVERRIDE;

  virtual void  MultiplyMatrixVector(unsigned int resultVectorIndex, unsigned int matrixIndex, unsigned int vectorIndex) ITK_OVERRIDE;

  /**
   * Perform a matrix*solution operation and store the result in the linear system
   * \param matrixIndex index of matrix to multiply
   * \param solutionIndex index of solution to multiply
   * \param resultVectorIndex index of vector where result is store
   */
  virtual void MultiplyMatrixSolution(unsigned int resultVectorIndex, unsigned int matrixIndex, unsigned int solutionIndex) ITK_OVERRIDE;

private:

  /** pointer to vector of matrices */
  MatrixHolder *m_Matrices;

  /** pointer to vector of force arrays */
  VectorHolder *m_Vectors;

  /** pointer to vector of solution arrays */
  VectorHolder *m_Solutions;

  /** pointer to array of unsigned int's indicating max number of entries in
    each matrix */
  // UnsignedIntegerArrayPtr m_MaximumNonZeroValues;
  unsigned int m_MaximumNonZeroValues;

  /** Array of pointers to available solver functions */
  ItkItpackSolverFunction m_Methods[7];

  /** flag indicating which solver function should be used */
  integer m_Method;

  /** vector of length 12 used to initialize various parameters on input */
  integer m_IPARM[12];

  /** vector of length 12 used to initialize various parameters on input */
  doublereal m_RPARM[12];
};

/**
 * \class FEMExceptionItpackSolver
 * \brief handles errors that occur in itpack solving routines
 * \sa LinearSystemWrapperItpack
 * \sa FEMException
 * \ingroup ITKFEM
 */
class ITK_ABI_EXPORT FEMExceptionItpackSolver : public FEMException
{
public:
  /** typedefs from f2c.h  */
  typedef long integer;

  /**
   * Constructor. In order to construct this exception object, four parameters
   * must be provided: file, lineNumber, location and a detailed description
   * of the exception.
   */
  FEMExceptionItpackSolver(const char *file, unsigned int lineNumber, std::string location, integer errorCode);

  /** Virtual destructor needed for subclasses. Has to have empty throw(). */
  virtual ~FEMExceptionItpackSolver() ITK_NOEXCEPT ITK_OVERRIDE {}

  /** Type related information. */
  itkTypeMacro(FEMExceptionItpackSolver, FEMException);
};
}
}  // end namespace itk::fem

#endif // #ifndef itkFEMLinearSystemWrapperItpack_h
