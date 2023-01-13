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
#ifndef itkArray2D_hxx
#define itkArray2D_hxx


namespace itk
{

template <typename TValue>
Array2D<TValue>::Array2D(unsigned int numberOfRows, unsigned int numberOfCols)
  : vnl_matrix<TValue>(numberOfRows, numberOfCols)
{}

template <typename TValue>
Array2D<TValue>::Array2D(const VnlMatrixType & matrix)
  : vnl_matrix<TValue>(matrix)
{}

template <typename TValue>
Array2D<TValue>::Array2D(const Self & array)
  : vnl_matrix<TValue>(array)
{}

template <typename TValue>
Array2D<TValue> &
Array2D<TValue>::operator=(const Self & array)
{
  this->VnlMatrixType::operator=(array);
  return *this;
}

template <typename TValue>
Array2D<TValue> &
Array2D<TValue>::operator=(const VnlMatrixType & matrix)
{
  this->VnlMatrixType::operator=(matrix);
  return *this;
}

template <typename TValue>
void
Array2D<TValue>::SetSize(unsigned int m, unsigned int n)
{
  this->set_size(m, n);
}
} // namespace itk

#endif
