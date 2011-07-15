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
#ifndef __itkTransform_hxx
#define __itkTransform_hxx

#include "itkTransform.h"

namespace itk
{
/**
 * Constructor
 */
template< class TScalarType,
          unsigned int NInputDimensions,
          unsigned int NOutputDimensions >
Transform< TScalarType, NInputDimensions, NOutputDimensions >
::Transform():
  m_Parameters(1),
  m_FixedParameters(1),
  m_Jacobian(NOutputDimensions, 1)
{
  itkWarningMacro(
    << "Using default transform constructor.  Should specify NOutputDims and NParameters as args to constructor.");
}

/**
 * Constructor
 */
template< class TScalarType,
          unsigned int NInputDimensions,
          unsigned int NOutputDimensions >
Transform< TScalarType, NInputDimensions, NOutputDimensions >
::Transform(unsigned int dimension, unsigned int numberOfParameters):
  m_Parameters(numberOfParameters),
  m_FixedParameters(numberOfParameters),
  m_Jacobian(dimension, numberOfParameters)
{}

/**
 * GenerateName
 */
template< class TScalarType,
          unsigned int NInputDimensions,
          unsigned int NOutputDimensions >
std::string Transform< TScalarType, NInputDimensions, NOutputDimensions >
::GetTransformTypeAsString() const
{
  std::ostringstream n;

  n << GetNameOfClass();
  n << "_";
  n << this->GetTransformTypeAsString(static_cast<TScalarType *>(0));
  n << "_" << this->GetInputSpaceDimension() << "_" << this->GetOutputSpaceDimension();
  return n.str();
}
} // end namespace itk

#endif
