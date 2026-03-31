/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkFEMLinearSystemWrapperDenseVNL.h"

namespace itk::fem
{
// Destructor is defined out-of-line so that the vtable and typeinfo are
// emitted as exported symbols in shared library builds.  Moving it to
// inline = default in the header would make them hidden under
// -fvisibility-inlines-hidden, breaking dynamic_cast across DSO boundaries.
// See: https://github.com/InsightSoftwareConsortium/ITK/issues/6000
LinearSystemWrapperDenseVNL::~LinearSystemWrapperDenseVNL() = default;

void
LinearSystemWrapperDenseVNL::InitializeMatrix(unsigned int matrixIndex)
{
  // allocate if necessary
  if (m_Matrices == nullptr)
  {
    m_Matrices = std::make_unique<MatrixHolder>(m_NumberOfMatrices);
  }

  (*m_Matrices)[matrixIndex] = std::make_unique<MatrixRepresentation>(this->GetSystemOrder(), this->GetSystemOrder());
  (*m_Matrices)[matrixIndex]->fill(0.0);
}

bool
LinearSystemWrapperDenseVNL::IsMatrixInitialized(unsigned int matrixIndex)
{
  if (!m_Matrices)
  {
    return false;
  }
  if (!((*m_Matrices)[matrixIndex]))
  {
    return false;
  }

  return true;
}

void
LinearSystemWrapperDenseVNL::DestroyMatrix(unsigned int matrixIndex)
{
  if (m_Matrices)
  {
    (*m_Matrices)[matrixIndex].reset();
  }
}

void
LinearSystemWrapperDenseVNL::InitializeVector(unsigned int vectorIndex)
{
  // allocate if necessary
  if (m_Vectors == nullptr)
  {
    m_Vectors = std::make_unique<std::vector<std::unique_ptr<vnl_vector<Float>>>>(m_NumberOfVectors);
  }

  (*m_Vectors)[vectorIndex] = std::make_unique<vnl_vector<Float>>(this->GetSystemOrder());
  (*m_Vectors)[vectorIndex]->fill(0.0);
}

bool
LinearSystemWrapperDenseVNL::IsVectorInitialized(unsigned int vectorIndex)
{
  if (!m_Vectors)
  {
    return false;
  }
  if (!(*m_Vectors)[vectorIndex])
  {
    return false;
  }

  return true;
}

void
LinearSystemWrapperDenseVNL::DestroyVector(unsigned int vectorIndex)
{
  if (m_Vectors)
  {
    (*m_Vectors)[vectorIndex].reset();
  }
}

void
LinearSystemWrapperDenseVNL::InitializeSolution(unsigned int solutionIndex)
{
  // allocate if necessary
  if (m_Solutions == nullptr)
  {
    m_Solutions = std::make_unique<std::vector<std::unique_ptr<vnl_vector<Float>>>>(m_NumberOfSolutions);
  }

  (*m_Solutions)[solutionIndex] = std::make_unique<vnl_vector<Float>>(this->GetSystemOrder());
  (*m_Solutions)[solutionIndex]->fill(0.0);
}

bool
LinearSystemWrapperDenseVNL::IsSolutionInitialized(unsigned int solutionIndex)
{
  if (!m_Solutions)
  {
    return false;
  }
  if (!(*m_Solutions)[solutionIndex])
  {
    return false;
  }

  return true;
}

void
LinearSystemWrapperDenseVNL::DestroySolution(unsigned int solutionIndex)
{
  if (m_Solutions)
  {
    (*m_Solutions)[solutionIndex].reset();
  }
}

LinearSystemWrapperDenseVNL::Float
LinearSystemWrapperDenseVNL::GetSolutionValue(unsigned int i, unsigned int solutionIndex) const
{
  if (m_Solutions == nullptr)
  {
    return 0.0;
  }
  if (((*m_Solutions)[solutionIndex])->size() <= i)
  {
    return 0.0;
  }
  else
  {
    return (*((*m_Solutions)[solutionIndex]))(i);
  }
}

void
LinearSystemWrapperDenseVNL::Solve()
{
  if ((m_Matrices->empty()) || (m_Vectors->empty()) || (m_Solutions->empty()))
  {
    throw FEMException(__FILE__, __LINE__, "FEM error!");
  }

  /* use functions to make sure that zero based matrix, vector, & index store
    final system to solve */
  /*
  if (m_PrimaryMatrixSetupFunction != nullptr) (*m_PrimaryMatrixSetupFunction)(static_cast<Superclass*>(this));
  if (m_PrimaryVectorSetupFunction != nullptr) (*m_PrimaryVectorSetupFunction)(static_cast<Superclass*>(this));
  if (m_PrimarySolutionSetupFunction != nullptr) (*m_PrimarySolutionSetupFunction)(static_cast<Superclass*>(this));
  */

  /**
   * Solve the system of linear equation and store the result in
   * m_Solutions(0).
   * Here we use the SVD method.
   */

  vnl_svd<Float> svd((*((*m_Matrices)[0])));
  (*((*m_Solutions)[0])) = svd.solve((*((*m_Vectors)[0])));
}

void
LinearSystemWrapperDenseVNL::SwapMatrices(unsigned int MatrixIndex1, unsigned int MatrixIndex2)
{
  std::swap((*m_Matrices)[MatrixIndex1], (*m_Matrices)[MatrixIndex2]);
}

void
LinearSystemWrapperDenseVNL::SwapVectors(unsigned int VectorIndex1, unsigned int VectorIndex2)
{
  std::swap((*m_Vectors)[VectorIndex1], (*m_Vectors)[VectorIndex2]);
}

void
LinearSystemWrapperDenseVNL::SwapSolutions(unsigned int SolutionIndex1, unsigned int SolutionIndex2)
{
  std::swap((*m_Solutions)[SolutionIndex1], (*m_Solutions)[SolutionIndex2]);
}

void
LinearSystemWrapperDenseVNL::CopySolution2Vector(unsigned int SolutionIndex, unsigned int VectorIndex)
{
  (*m_Vectors)[VectorIndex] = std::make_unique<vnl_vector<Float>>(*((*m_Solutions)[SolutionIndex]));
}

void
LinearSystemWrapperDenseVNL::CopyVector2Solution(unsigned int VectorIndex, unsigned int SolutionIndex)
{
  (*m_Solutions)[SolutionIndex] = std::make_unique<vnl_vector<Float>>(*((*m_Vectors)[VectorIndex]));
}

void
LinearSystemWrapperDenseVNL::MultiplyMatrixMatrix(unsigned int ResultMatrixIndex,
                                                  unsigned int LeftMatrixIndex,
                                                  unsigned int RightMatrixIndex)
{
  // delete (*m_Matrices)[ResultMatrixIndex];
  // (*m_Matrices)[ResultMatrixIndex] = new vnl_matrix<Float>(
  // this->GetSystemOrder(), this->GetSystemOrder() );

  (*((*m_Matrices)[ResultMatrixIndex])) = (*((*m_Matrices)[LeftMatrixIndex])) * (*((*m_Matrices)[RightMatrixIndex]));
}

void
LinearSystemWrapperDenseVNL::MultiplyMatrixVector(unsigned int resultVectorIndex,
                                                  unsigned int matrixIndex,
                                                  unsigned int vectorIndex)
{
  // delete (*m_Matrices)[ResultMatrixIndex];
  // (*m_Matrices)[ResultMatrixIndex] = new vnl_matrix<Float>(
  // this->GetSystemOrder(), this->GetSystemOrder() );
  (*(*m_Vectors)[resultVectorIndex]) = (*(*m_Vectors)[vectorIndex]);
  (*m_Vectors)[resultVectorIndex]->pre_multiply(*((*m_Matrices)[matrixIndex]));
}

void
LinearSystemWrapperDenseVNL::ScaleMatrix(Float scale, unsigned int matrixIndex)
{
  (*((*m_Matrices)[matrixIndex])) = (*((*m_Matrices)[matrixIndex])) * scale;
}

void
LinearSystemWrapperDenseVNL::ScaleVector(Float scale, unsigned int vectorIndex)
{
  (*(*m_Vectors)[vectorIndex]) = (*(*m_Vectors)[vectorIndex]) * scale;
}

void
LinearSystemWrapperDenseVNL::ScaleSolution(Float scale, unsigned int solutionIndex)
{
  (*(*m_Solutions)[solutionIndex]) = (*(*m_Solutions)[solutionIndex]) * scale;
}

} // namespace itk::fem
