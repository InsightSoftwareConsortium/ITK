/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLinearSystemWrapperDenseVNL.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMLinearSystemWrapperDenseVNL_h
#define __itkFEMLinearSystemWrapperDenseVNL_h
#include "itkFEMLinearSystemWrapper.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"
#include "vxl/vnl/algo/vnl_svd.h"
#include <vxl/vnl/algo/vnl_lsqr.h>
#include <vector>


namespace itk {
namespace fem {


/**
 * \class LinearSystemWrapperDenseVNL
 * \brief LinearSystemWrapper class that uses VNL numeric library functions
 *        to define a sparse linear system of equations.
 * \sa LinearSystemWrapper
 */
class LinearSystemWrapperDenseVNL : public LinearSystemWrapper
{
public:

  /* values stored in matrices & vectors */
  typedef LinearSystemWrapper::Float Float;

  /* superclass */
  typedef LinearSystemWrapper SuperClass;

  /* matrix typedef */
  typedef vnl_matrix<Float>                 MatrixRepresentation;

  /* matrix holder typedef */
  typedef std::vector< MatrixRepresentation* >     MatrixHolder;

  /* constructor & destructor */
  LinearSystemWrapperDenseVNL() : LinearSystemWrapper(), m_Matrices(0), m_Vectors(0), m_Solutions(0) {}
  virtual ~LinearSystemWrapperDenseVNL();

  /* memory management routines */
  virtual void  InitializeMatrix(unsigned int MatrixIndex);
  virtual bool  IsMatrixInitialized(unsigned int matrixIndex);
  virtual void  DestroyMatrix(unsigned int MatrixIndex);
  virtual void  InitializeVector(unsigned int VectorIndex);
  virtual bool  IsVectorInitialized(unsigned int vectorIndex);
  virtual void  DestroyVector(unsigned int MatrixIndex);
  virtual void  InitializeSolution(unsigned int SolutionIndex);
  virtual bool  IsSolutionInitialized(unsigned int solutionIndex);
  virtual void  DestroySolution(unsigned int SolutionIndex);
  virtual void  SetMaximumNonZeroValuesInMatrix(unsigned int maxNonZeros, unsigned int matrixIndex) {}


  /* assembly & solving routines */
  virtual Float GetMatrixValue(unsigned int i, unsigned int j, unsigned int MatrixIndex) const { return (*((*m_Matrices)[MatrixIndex]))(i,j); }
  virtual void  SetMatrixValue(unsigned int i, unsigned int j, Float value, unsigned int MatrixIndex) { (*((*m_Matrices)[MatrixIndex]))(i,j) =  value; }
  virtual void  AddMatrixValue(unsigned int i, unsigned int j, Float value, unsigned int MatrixIndex) { (*((*m_Matrices)[MatrixIndex]))(i,j) += value; }
  virtual Float GetVectorValue(unsigned int i, unsigned int VectorIndex) const { return (* ( (*m_Vectors)[VectorIndex] ) )[i]; }
  virtual void  SetVectorValue(unsigned int i, Float value, unsigned int VectorIndex) { (*((*m_Vectors)[VectorIndex]))(i) =  value; }
  virtual void  AddVectorValue(unsigned int i, Float value, unsigned int VectorIndex) { (*((*m_Vectors)[VectorIndex]))(i) += value; }
  virtual Float GetSolutionValue(unsigned int i, unsigned int SolutionIndex) const;
  virtual void  SetSolutionValue(unsigned int i, Float value, unsigned int SolutionIndex) { (*((*m_Solutions)[SolutionIndex]))(i) =  value; }
  virtual void  AddSolutionValue(unsigned int i, Float value, unsigned int SolutionIndex) { (*((*m_Solutions)[SolutionIndex]))(i) += value; }
  virtual void  Solve(void);

  /* matrix & vector manipulation routines */
  virtual void  ScaleMatrix(Float scale, unsigned int matrixIndex);
  virtual void  SwapMatrices(unsigned int MatrixIndex1, unsigned int MatrixIndex2);
  virtual void  SwapVectors(unsigned int VectorIndex1, unsigned int VectorIndex2);
  virtual void  SwapSolutions(unsigned int SolutionIndex1, unsigned int SolutionIndex2);
  virtual void  CopySolution2Vector(unsigned SolutionIndex, unsigned int VectorIndex);
  virtual void  MultiplyMatrixMatrix(unsigned int ResultMatrixIndex, unsigned int LeftMatrixIndex, unsigned int RightMatrixIndex);
  virtual void  MultiplyMatrixVector(unsigned int ResultVectorIndex, unsigned int MatrixIndex, unsigned int VectorIndex);

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

#endif // #ifndef __itkFEMLinearSystemWrapperDenseVNL_h


