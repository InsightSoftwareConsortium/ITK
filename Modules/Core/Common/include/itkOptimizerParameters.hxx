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

#ifndef itkOptimizerParameters_hxx
#define itkOptimizerParameters_hxx

#include "itkOptimizerParameters.h"

namespace itk
{
/** Default constructor */
template <typename TParametersValueType>
OptimizerParameters<TParametersValueType>::OptimizerParameters() = default;

/** Copy constructor */
template <typename TParametersValueType>
OptimizerParameters<TParametersValueType>::OptimizerParameters(const OptimizerParameters & rhs)
  : Array<TParametersValueType>(rhs)
{
  // Note: don't copy the OptimizerParametersHelper.
  // The Array copy constructor will allocate new memory
  // and copy the data to it. So we end up here with a generic
  // OptimizerParameters data object even if 'rhs' points to
  // something different.
}

/** Constructor with size */
template <typename TParametersValueType>
OptimizerParameters<TParametersValueType>::OptimizerParameters(SizeValueType dimension)
  : Array<TParametersValueType>(dimension)
{}

/** Constructor with Array */
template <typename TParametersValueType>
OptimizerParameters<TParametersValueType>::OptimizerParameters(const ArrayType & array)
  : Array<TParametersValueType>(array)
{}

template <typename TParametersValueType>
void
OptimizerParameters<TParametersValueType>::Initialize()
{
  // Set the default OptimizerParametersHelper
  this->m_Helper.reset(new OptimizerParametersHelperType);
}

/** Destructor */
template <typename TParametersValueType>
OptimizerParameters<TParametersValueType>::~OptimizerParameters() = default;

template <typename TParametersValueType>
void
OptimizerParameters<TParametersValueType>::SetHelper(OptimizerParametersHelperType * helper)
{
  this->m_Helper.reset(helper);
}

/** Copy operator for self */
template <typename TParametersValueType>
const typename OptimizerParameters<TParametersValueType>::Self &
OptimizerParameters<TParametersValueType>::operator=(const Self & rhs)
{
  // Note: there's no need to copy the OptimizerParametersHelper.
  // Call the superclass implementation.
  this->ArrayType::operator=(rhs);
  return *this;
}

template <typename TParametersValueType>
const typename OptimizerParameters<TParametersValueType>::Self &
OptimizerParameters<TParametersValueType>::operator=(const ArrayType & rhs)
{
  // Call the superclass implementation
  this->ArrayType::operator=(rhs);
  return *this;
}

template <typename TParametersValueType>
const typename OptimizerParameters<TParametersValueType>::Self &
OptimizerParameters<TParametersValueType>::operator=(const VnlVectorType & rhs)
{
  // Call the superclass implementation
  this->ArrayType::operator=(rhs);
  return *this;
}

template <typename TParametersValueType>
void
OptimizerParameters<TParametersValueType>::MoveDataPointer(TParametersValueType * pointer)
{
  if (m_Helper == nullptr)
  {
    itkGenericExceptionMacro("OptimizerParameters::MoveDataPointer: "
                             "m_Helper must be set.");
  }
  this->m_Helper->MoveDataPointer(this, pointer);
}

template <typename TParametersValueType>
void
OptimizerParameters<TParametersValueType>::SetParametersObject(LightObject * object)
{
  if (m_Helper == nullptr)
  {
    itkGenericExceptionMacro("OptimizerParameters::SetParameterObject: "
                             "m_Helper must be set.");
  }
  this->m_Helper->SetParametersObject(this, object);
}

} // namespace itk
#endif
