/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLinearSystemWrapperVNL.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMLinearSystemWrapperVNL_h
#define __itkFEMLinearSystemWrapperVNL_h
#include "itkFEMLinearSystemWrapper.h"
#include "vnl/vnl_sparse_matrix.h"
#include "vnl/vnl_vector.h"
#include <vnl/vnl_sparse_matrix_linear_system.h>
#include <vnl/algo/vnl_lsqr.h>
#include <vector>


namespace itk {
namespace fem {


/**
 * \class LinearSystemWrapperVNL
 * \brief LinearSystemWrapper class that uses VNL numeric library functions
 *        to define a sparse linear system of equations.
 * \sa LinearSystemWrapper
 */
class LinearSystemWrapperVNL : public LinearSystemWrapper
{
public:

  /* values stored in matrices & vectors */
  typedef LinearSystemWrapper::Float Float;

  /* superclass */
  typedef LinearSystemWrapper SuperClass;

  /* matrix typedef */
  typedef vnl_sparse_matrix<Float>                 MatrixRepresentation;

  /* matrix holder typedef */
  typedef std::vector< MatrixRepresentation* >     MatrixHolder;

  /* constructor & destructor */
  LinearSystemWrapperVNL() : LinearSystemWrapper(), m_Matrices(0), m_Vectors(0), m_Solutions(0) {}
  virtual ~LinearSystemWrapperVNL();

  /* memory management routines */
  virtual void  InitializeMatrix(unsigned int matrixIndex);
  virtual bool  IsMatrixInitialized(unsigned int matrixIndex);
  virtual void  DestroyMatrix(unsigned int matrixIndex);
  virtual void  InitializeVector(unsigned int vectorIndex);
  virtual bool  IsVectorInitialized(unsigned int vectorIndex);
  virtual void  DestroyVector(unsigned int vectorIndex);
  virtual void  InitializeSolution(unsigned int solutionIndex);
  virtual bool  IsSolutionInitialized(unsigned int solutionIndex);
  virtual void  DestroySolution(unsigned int solutionIndex);
  virtual void  SetMaximumNonZeroValuesInMatrix(unsigned int, unsigned int) {}


  /* assembly & solving routines */
  virtual Float GetMatrixValue(unsigned int i, unsigned int j, unsigned int matrixIndex) const { return (*((*m_Matrices)[matrixIndex]))(i,j); }
  virtual void  SetMatrixValue(unsigned int i, unsigned int j, Float value, unsigned int matrixIndex) { (*((*m_Matrices)[matrixIndex]))(i,j) =  value; }
  virtual void  AddMatrixValue(unsigned int i, unsigned int j, Float value, unsigned int matrixIndex) { (*((*m_Matrices)[matrixIndex]))(i,j) += value; }
  virtual Float GetVectorValue(unsigned int i, unsigned int vectorIndex) const { return (* ( (*m_Vectors)[vectorIndex] ) )[i]; }
  virtual void  SetVectorValue(unsigned int i, Float value, unsigned int vectorIndex) { (*((*m_Vectors)[vectorIndex]))(i) =  value; }
  virtual void  AddVectorValue(unsigned int i, Float value, unsigned int vectorIndex) { (*((*m_Vectors)[vectorIndex]))(i) += value; }
  virtual Float GetSolutionValue(unsigned int i, unsigned int solutionIndex) const;
  virtual void  SetSolutionValue(unsigned int i, Float value, unsigned int solutionIndex) { (*((*m_Solutions)[solutionIndex]))(i) =  value; }
  virtual void  AddSolutionValue(unsigned int i, Float value, unsigned int solutionIndex) { (*((*m_Solutions)[solutionIndex]))(i) += value; }
  virtual void  Solve(void);

  /* matrix & vector manipulation routines */
  virtual void  ScaleMatrix(Float scale, unsigned int matrixIndex);
  virtual void  SwapMatrices(unsigned int matrixIndex1, unsigned int matrixIndex2);
  virtual void  SwapVectors(unsigned int vectorIndex1, unsigned int vectorIndex2);
  virtual void  SwapSolutions(unsigned int solutionIndex1, unsigned int solutionIndex2);
  virtual void  CopySolution2Vector(unsigned solutionIndex, unsigned int vectorIndex);
  virtual void  CopyVector2Solution(unsigned int vectorIndex, unsigned int solutionIndex);
  virtual void  MultiplyMatrixMatrix(unsigned int resultMatrixIndex, unsigned int leftMatrixIndex, unsigned int rightMatrixIndex);
  virtual void  MultiplyMatrixVector(unsigned int resultVectorIndex, unsigned int matrixIndex, unsigned int vectorIndex);

private:

  /** vector of pointers to VNL sparse matrices */
  //std::vector< vnl_sparse_matrix<Float>* > *m_Matrices;
  MatrixHolder *m_Matrices;

  /** vector of pointers to VNL vectors  */
  std::vector< vnl_vector<Float>* > *m_Vectors;

  /** vector of pointers to VNL vectors */
  std::vector< vnl_vector<Float>* > *m_Solutions;

};

}} // end namespace itk::fem

#endif // #ifndef __itkFEMLinearSystemWrapperVNL_h


