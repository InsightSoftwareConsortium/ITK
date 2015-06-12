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
#ifndef itkScaleLogarithmicTransform_hxx
#define itkScaleLogarithmicTransform_hxx

#include "itkScaleLogarithmicTransform.h"

namespace itk
{
// Constructor with default arguments
template<typename TParametersValueType, unsigned int NDimensions>
ScaleLogarithmicTransform<TParametersValueType, NDimensions>
::ScaleLogarithmicTransform()
{
}

// Destructor
template<typename TParametersValueType, unsigned int NDimensions>
ScaleLogarithmicTransform<TParametersValueType, NDimensions>::
~ScaleLogarithmicTransform()
{
}

// Set the parameters
template<typename TParametersValueType, unsigned int NDimensions>
void
ScaleLogarithmicTransform<TParametersValueType, NDimensions>
::SetParameters(const ParametersType & parameters)
{
  ScaleType scales;

  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    scales[i] = std::exp(parameters[i]);
    }
  // Save parameters. Needed for proper operation of TransformUpdateParameters.
  if( &parameters != &(this->m_Parameters) )
    {
    this->m_Parameters = parameters;
    }
  this->SetScale(scales);

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();
}

// Get Parameters
template<typename TParametersValueType, unsigned int NDimensions>
const typename ScaleLogarithmicTransform<TParametersValueType, NDimensions>::ParametersType
& ScaleLogarithmicTransform<TParametersValueType, NDimensions>
::GetParameters(void) const
  {
  itkDebugMacro(<< "Getting parameters ");

  const ScaleType & scales = this->GetScale();
  // Transfer the translation part
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    this->m_Parameters[i] = std::log(scales[i]);
    }

  itkDebugMacro(<< "After getting parameters " << this->m_Parameters);

  return this->m_Parameters;
  }

// Print self
template<typename TParametersValueType, unsigned int NDimensions>
void
ScaleLogarithmicTransform<TParametersValueType, NDimensions>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template<typename TParametersValueType, unsigned int NDimensions>
void
ScaleLogarithmicTransform<TParametersValueType, NDimensions>
::ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const
{
  const ScaleType & scales = this->GetScale();

  jacobian.SetSize( SpaceDimension, this->GetNumberOfLocalParameters() );
  jacobian.Fill(0);
  for( unsigned int dim = 0; dim < SpaceDimension; dim++ )
    {
    // the derivative with respect to Log(scale) = scale * derivative with
    // respect to scale.
    jacobian(dim, dim) = scales[dim] * p[dim];
    }
}

} // namespace

#endif
