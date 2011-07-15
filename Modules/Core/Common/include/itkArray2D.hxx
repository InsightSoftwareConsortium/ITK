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
#ifndef __itkArray2D_hxx
#define __itkArray2D_hxx

#include "itkArray2D.h"

namespace itk
{
/** Default constructor  */
template< typename TValueType >
Array2D< TValueType >
::Array2D():vnl_matrix< TValueType >()
{}

/** Constructor with number of rows and columns as arguments */
template< typename TValueType >
Array2D< TValueType >
::Array2D(unsigned int numberOfRows, unsigned int numberOfCols):
  vnl_matrix< TValueType >(numberOfRows, numberOfCols)
{}

/** Constructor from a vnl_matrix */
template< typename TValueType >
Array2D< TValueType >
::Array2D(const VnlMatrixType & matrix):vnl_matrix< TValueType >(matrix)
{}

/** Copy Constructor  */
template< typename TValueType >
Array2D< TValueType >
::Array2D(const Self & array):vnl_matrix< TValueType >(array)
{}

/** Assignment Operator from Array */
template< typename TValueType >
const Array2D< TValueType > &
Array2D< TValueType >
::operator=(const Self & array)
{
  this->VnlMatrixType::operator=(array);
  return *this;
}

/** Assignment Operator from vnl_matrix */
template< typename TValueType >
const Array2D< TValueType > &
Array2D< TValueType >
::operator=(const VnlMatrixType & matrix)
{
  this->VnlMatrixType::operator=(matrix);
  return *this;
}

/** Set the size of the array */
template< typename TValueType >
void Array2D< TValueType >
::SetSize(unsigned int m, unsigned int n)
{
  this->set_size(m, n);
}
} // namespace itk

#endif
