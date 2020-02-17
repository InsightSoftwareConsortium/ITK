/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#ifndef itkFEMLinearSystemWrapperVNL_h
#define itkFEMLinearSystemWrapperVNL_h

#include "itkFEMLinearSystemWrapper.h"
#include "vnl/vnl_sparse_matrix.h"
#include "vnl/vnl_vector.h"
#include <vector>
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class LinearSystemWrapperVNL
 * \brief LinearSystemWrapper class that uses VNL numeric library functions
 *        to define a sparse linear system of equations.
 * \sa LinearSystemWrapper
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT LinearSystemWrapperVNL : public LinearSystemWrapper
{
public:
  /* values stored in matrices & vectors */
  using Float = LinearSystemWrapper::Float;

  /* superclass */
  using Superclass = LinearSystemWrapper;

  /* matrix type alias */
  using MatrixRepresentation = vnl_sparse_matrix<Float>;

  /* matrix holder type alias */
  using MatrixHolder = std::vector<MatrixRepresentation *>;

  /* constructor & destructor */
  LinearSystemWrapperVNL()
    : LinearSystemWrapper()
  {}

  ~LinearSystemWrapperVNL() override;

  /* memory management routines */
  void
  InitializeMatrix(unsigned int matrixIndex) override;

  bool
  IsMatrixInitialized(unsigned int matrixIndex) override;

  void
  DestroyMatrix(unsigned int matrixIndex) override;

  void
  InitializeVector(unsigned int vectorIndex) override;

  bool
  IsVectorInitialized(unsigned int vectorIndex) override;

  void
  DestroyVector(unsigned int vectorIndex) override;

  void
  InitializeSolution(unsigned int solutionIndex) override;

  bool
  IsSolutionInitialized(unsigned int solutionIndex) override;

  void
  DestroySolution(unsigned int solutionIndex) override;

  virtual void
  SetMaximumNonZeroValuesInMatrix(unsigned int, unsigned int)
  {}

  /* assembly & solving routines */
  Float
  GetMatrixValue(unsigned int i, unsigned int j, unsigned int matrixIndex) const override
  {
    return (*((*m_Matrices)[matrixIndex]))(i, j);
  }
  void
  SetMatrixValue(unsigned int i, unsigned int j, Float value, unsigned int matrixIndex) override
  {
    (*((*m_Matrices)[matrixIndex]))(i, j) = value;
  }
  void
  AddMatrixValue(unsigned int i, unsigned int j, Float value, unsigned int matrixIndex) override
  {
    (*((*m_Matrices)[matrixIndex]))(i, j) += value;
  }
  Float
  GetVectorValue(unsigned int i, unsigned int vectorIndex) const override
  {
    return (*((*m_Vectors)[vectorIndex]))[i];
  }
  void
  SetVectorValue(unsigned int i, Float value, unsigned int vectorIndex) override
  {
    (*((*m_Vectors)[vectorIndex]))(i) = value;
  }
  void
  AddVectorValue(unsigned int i, Float value, unsigned int vectorIndex) override
  {
    (*((*m_Vectors)[vectorIndex]))(i) += value;
  }
  Float
  GetSolutionValue(unsigned int i, unsigned int solutionIndex) const override;

  void
  SetSolutionValue(unsigned int i, Float value, unsigned int solutionIndex) override
  {
    (*((*m_Solutions)[solutionIndex]))(i) = value;
  }
  void
  AddSolutionValue(unsigned int i, Float value, unsigned int solutionIndex) override
  {
    (*((*m_Solutions)[solutionIndex]))(i) += value;
  }
  void
  Solve() override;

  /* matrix & vector manipulation routines */
  void
  ScaleMatrix(Float scale, unsigned int matrixIndex) override;

  void
  SwapMatrices(unsigned int matrixIndex1, unsigned int matrixIndex2) override;

  void
  SwapVectors(unsigned int vectorIndex1, unsigned int vectorIndex2) override;

  void
  SwapSolutions(unsigned int solutionIndex1, unsigned int solutionIndex2) override;

  void
  CopySolution2Vector(unsigned solutionIndex, unsigned int vectorIndex) override;

  void
  CopyVector2Solution(unsigned int vectorIndex, unsigned int solutionIndex) override;

  void
  MultiplyMatrixMatrix(unsigned int resultMatrixIndex,
                       unsigned int leftMatrixIndex,
                       unsigned int rightMatrixIndex) override;

  void
  MultiplyMatrixVector(unsigned int resultVectorIndex, unsigned int matrixIndex, unsigned int vectorIndex) override;

private:
  /** vector of pointers to VNL sparse matrices */
  // std::vector< vnl_sparse_matrix<Float>* > *m_Matrices;
  MatrixHolder * m_Matrices{ nullptr };

  /** vector of pointers to VNL vectors  */
  std::vector<vnl_vector<Float> *> * m_Vectors{ nullptr };

  /** vector of pointers to VNL vectors */
  std::vector<vnl_vector<Float> *> * m_Solutions{ nullptr };
};
} // end namespace fem
} // end namespace itk

#endif // itkFEMLinearSystemWrapperVNL_h
