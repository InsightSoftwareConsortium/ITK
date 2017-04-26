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
#ifndef itkRieszRotationMatrix_hxx
#define itkRieszRotationMatrix_hxx

#include "itkRieszRotationMatrix.h"
#include "itkNumericTraits.h"
#include "itkRieszUtilities.h"

namespace itk
{
template <typename T, unsigned int VImageDimension>
RieszRotationMatrix<T, VImageDimension>::RieszRotationMatrix()
  : Superclass()
  , m_SpatialRotationMatrix()
  , m_Order(0)
  , m_Components(0)
{}

template <typename T, unsigned int VImageDimension>
RieszRotationMatrix<T, VImageDimension>::RieszRotationMatrix(const Self & rieszMatrix)
  : Superclass(rieszMatrix)
{
  this->m_SpatialRotationMatrix = rieszMatrix.GetRotationMatrix;
  this->m_Order = rieszMatrix.GetOrder();
  this->m_Components = rieszMatrix.GetComponents();
}

template <typename T, unsigned int VImageDimension>
RieszRotationMatrix<T, VImageDimension>::RieszRotationMatrix(const SpatialRotationMatrixType & spatialRotationMatrix,
                                                             const unsigned int &              order)
  : Superclass()
  , m_SpatialRotationMatrix(spatialRotationMatrix)
  , m_Order(order)
{
  this->m_Components = itk::utils::ComputeNumberOfComponents(this->m_Order, VImageDimension);
  this->SetSize(m_Components, m_Components);
  this->ComputeSteerableMatrix();
}

/**
 *  Product by a std::vector
 */
template <typename T, unsigned int VImageDimension>
std::vector<T>
RieszRotationMatrix<T, VImageDimension>::operator*(const std::vector<T> & vect) const
{
  unsigned int rows = this->Rows();
  unsigned int cols = this->Cols();

  if (vect.size() != cols)
  {
    itkGenericExceptionMacro(<< "Matrix with " << this->Cols() << " columns cannot be "
                             << "multiplied with vector of length: " << vect.size());
  }

  std::vector<T> result(rows);
  for (unsigned int r = 0; r < rows; r++)
  {
    T sum = NumericTraits<T>::ZeroValue();
    for (unsigned int c = 0; c < cols; c++)
    {
      sum += this->m_Matrix(r, c) * vect[c];
    }
    result[r] = sum;
  }
  return result;
}

template <typename T, unsigned int VImageDimension>
void
RieszRotationMatrix<T, VImageDimension>::ComputeSteerableMatrix()
{}

} // end namespace itk

#endif
