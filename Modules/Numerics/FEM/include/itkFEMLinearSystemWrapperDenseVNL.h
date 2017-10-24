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

#ifndef itkFEMLinearSystemWrapperDenseVNL_h
#define itkFEMLinearSystemWrapperDenseVNL_h
#include "itkFEMLinearSystemWrapper.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"
#include "vnl/algo/vnl_svd.h"
#include "ITKFEMExport.h"
#include <vector>

namespace itk
{
namespace fem
{
/**
 * \class LinearSystemWrapperDenseVNL
 * \brief LinearSystemWrapper class that uses VNL numeric library functions
 *        to define a sparse linear system of equations.
 * \sa LinearSystemWrapper
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT LinearSystemWrapperDenseVNL : public LinearSystemWrapper
{
public:

  /* values stored in matrices & vectors */
  typedef LinearSystemWrapper::Float Float;

  /* superclass */
  typedef LinearSystemWrapper Superclass;

  /* matrix typedef */
  typedef vnl_matrix<Float> MatrixRepresentation;

  /* matrix holder typedef */
  typedef std::vector<MatrixRepresentation *> MatrixHolder;

  /* constructor & destructor */
  LinearSystemWrapperDenseVNL() : LinearSystemWrapper(), m_Matrices(ITK_NULLPTR), m_Vectors(ITK_NULLPTR), m_Solutions(ITK_NULLPTR)
  {
  }
  virtual ~LinearSystemWrapperDenseVNL() ITK_OVERRIDE;

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

  virtual void  SetMaximumNonZeroValuesInMatrix(unsigned int, unsigned int)
  {
  }

  /* assembly & solving routines */
  virtual Float GetMatrixValue(unsigned int i, unsigned int j,
                               unsigned int matrixIndex) const ITK_OVERRIDE
  {
    return ( *( ( *m_Matrices )[matrixIndex] ) )(i, j);
  }
  virtual void  SetMatrixValue(unsigned int i, unsigned int j, Float value,
                               unsigned int matrixIndex) ITK_OVERRIDE
  {
    ( *( ( *m_Matrices )[matrixIndex] ) )(i, j) =  value;
  }
  virtual void  AddMatrixValue(unsigned int i, unsigned int j, Float value,
                               unsigned int matrixIndex) ITK_OVERRIDE
  {
    ( *( ( *m_Matrices )[matrixIndex] ) )(i, j) += value;
  }
  virtual Float GetVectorValue(unsigned int i,
                               unsigned int vectorIndex) const ITK_OVERRIDE
  {
    return ( *( ( *m_Vectors )[vectorIndex] ) )[i];
  }
  virtual void  SetVectorValue(unsigned int i, Float value,
                               unsigned int vectorIndex) ITK_OVERRIDE
  {
    ( *( ( *m_Vectors )[vectorIndex] ) )(i) =  value;
  }
  virtual void  AddVectorValue(unsigned int i, Float value,
                               unsigned int vectorIndex) ITK_OVERRIDE
  {
    ( *( ( *m_Vectors )[vectorIndex] ) )(i) += value;
  }
  virtual Float GetSolutionValue(unsigned int i, unsigned int solutionIndex) const ITK_OVERRIDE;

  virtual void  SetSolutionValue(unsigned int i, Float value,
                                 unsigned int solutionIndex) ITK_OVERRIDE
  {
    ( *( ( *m_Solutions )[solutionIndex] ) )(i) =  value;
  }
  virtual void  AddSolutionValue(unsigned int i, Float value,
                                 unsigned int solutionIndex) ITK_OVERRIDE
  {
    ( *( ( *m_Solutions )[solutionIndex] ) )(i) += value;
  }
  virtual void  Solve(void) ITK_OVERRIDE;

  /* matrix & vector manipulation routines */
  virtual void  ScaleMatrix(Float scale, unsigned int matrixIndex) ITK_OVERRIDE;

  virtual void  ScaleVector(Float scale, unsigned int vectorIndex);

  virtual void  ScaleSolution(Float scale, unsigned int solutionIndex);

  virtual void  SwapMatrices(unsigned int matrixIndex1, unsigned int matrixIndex2) ITK_OVERRIDE;

  virtual void  SwapVectors(unsigned int vectorIndex1, unsigned int vectorIndex2) ITK_OVERRIDE;

  virtual void  SwapSolutions(unsigned int solutionIndex1, unsigned int solutionIndex2) ITK_OVERRIDE;

  virtual void  CopySolution2Vector(unsigned solutionIndex, unsigned int vectorIndex) ITK_OVERRIDE;

  virtual void  CopyVector2Solution(unsigned int vectorIndex, unsigned int solutionIndex) ITK_OVERRIDE;

  virtual void  MultiplyMatrixMatrix(unsigned int resultMatrixIndex, unsigned int leftMatrixIndex,
                                     unsigned int rightMatrixIndex) ITK_OVERRIDE;

  virtual void  MultiplyMatrixVector(unsigned int resultVectorIndex, unsigned int matrixIndex, unsigned int vectorIndex) ITK_OVERRIDE;

private:

  /** vector of pointers to VNL sparse matrices */
  // std::vector< vnl_sparse_matrix<Float>* > *m_Matrices;
  MatrixHolder *m_Matrices;

  /** vector of pointers to VNL vectors  */
  std::vector<vnl_vector<Float> *> *m_Vectors;

  /** vector of pointers to VNL vectors */
  std::vector<vnl_vector<Float> *> *m_Solutions;
};
}
}  // end namespace itk::fem

#endif
