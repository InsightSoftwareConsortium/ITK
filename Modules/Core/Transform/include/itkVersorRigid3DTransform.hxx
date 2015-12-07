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
#ifndef itkVersorRigid3DTransform_hxx
#define itkVersorRigid3DTransform_hxx

#include "itkVersorRigid3DTransform.h"

namespace itk
{
// Constructor with default arguments
template<typename TParametersValueType>
VersorRigid3DTransform<TParametersValueType>
::VersorRigid3DTransform() :
  Superclass(ParametersDimension)
{
}

// Constructor with arguments
template<typename TParametersValueType>
VersorRigid3DTransform<TParametersValueType>::VersorRigid3DTransform(unsigned int paramDim) :
  Superclass(paramDim)
{
}

// Constructor with arguments
template<typename TParametersValueType>
VersorRigid3DTransform<TParametersValueType>::VersorRigid3DTransform(const MatrixType & matrix,
                                                            const OutputVectorType & offset) :
  Superclass(matrix, offset)
{
}

// Set Parameters
template<typename TParametersValueType>
void
VersorRigid3DTransform<TParametersValueType>
::SetParameters(const ParametersType & parameters)
{
  itkDebugMacro(<< "Setting parameters " << parameters);

  // Save parameters. Needed for proper operation of TransformUpdateParameters.
  if( &parameters != &(this->m_Parameters) )
    {
    this->m_Parameters = parameters;
    }

  // Transfer the versor part

  AxisType axis;

  double norm = parameters[0] * parameters[0];
  axis[0] = parameters[0];
  norm += parameters[1] * parameters[1];
  axis[1] = parameters[1];
  norm += parameters[2] * parameters[2];
  axis[2] = parameters[2];
  if( norm > 0 )
    {
    norm = std::sqrt(norm);
    }

  double epsilon = 1e-10;
  if( norm >= 1.0 - epsilon )
    {
    axis = axis / ( norm + epsilon * norm );
    }
  VersorType newVersor;
  newVersor.Set(axis);
  this->SetVarVersor(newVersor);
  this->ComputeMatrix();

  itkDebugMacro( << "Versor is now " << this->GetVersor() );

  // Transfer the translation part
  TranslationType newTranslation;
  newTranslation[0] = parameters[3];
  newTranslation[1] = parameters[4];
  newTranslation[2] = parameters[5];
  this->SetVarTranslation(newTranslation);
  this->ComputeOffset();

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();

  itkDebugMacro(<< "After setting parameters ");
}

//
// Get Parameters
//
// Parameters are ordered as:
//
// p[0:2] = right part of the versor (axis times std::sin(t/2))
// p[3:5} = translation components
//

template<typename TParametersValueType>
const typename VersorRigid3DTransform<TParametersValueType>::ParametersType
& VersorRigid3DTransform<TParametersValueType>
::GetParameters(void) const
  {
  itkDebugMacro(<< "Getting parameters ");

  this->m_Parameters[0] = this->GetVersor().GetX();
  this->m_Parameters[1] = this->GetVersor().GetY();
  this->m_Parameters[2] = this->GetVersor().GetZ();

  // Transfer the translation
  this->m_Parameters[3] = this->GetTranslation()[0];
  this->m_Parameters[4] = this->GetTranslation()[1];
  this->m_Parameters[5] = this->GetTranslation()[2];

  itkDebugMacro(<< "After getting parameters " << this->m_Parameters);

  return this->m_Parameters;
  }

template<typename TParametersValueType>
void
VersorRigid3DTransform<TParametersValueType>
::UpdateTransformParameters( const DerivativeType & update, TParametersValueType factor )
{
  SizeValueType numberOfParameters = this->GetNumberOfParameters();

  if( update.Size() != numberOfParameters )
    {
    itkExceptionMacro("Parameter update size, " << update.Size() << ", must "
                      " be same as transform parameter size, "
                      << numberOfParameters << std::endl);
    }

  /* Make sure m_Parameters is updated to reflect the current values in
   * the transform's other parameter-related variables. This is effective for
   * managing the parallel variables used for storing parameter data,
   * but inefficient. However for small global transforms, shouldn't be
   * too bad. Dense-field transform will want to make sure m_Parameters
   * is always updated whenever the transform is changed, so GetParameters
   * can be skipped in their implementations of UpdateTransformParameters.
   */
  this->GetParameters();

  VectorType rightPart;

  for ( unsigned int i = 0; i < 3; i++ )
    {
    rightPart[i] = this->m_Parameters[i];
    }

  VersorType currentRotation;
  currentRotation.Set(rightPart);

  // The gradient indicate the contribution of each one
  // of the axis to the direction of highest change in
  // the function
  VectorType axis;
  axis[0] = update[0];
  axis[1] = update[1];
  axis[2] = update[2];

  // gradientRotation is a rotation along the
  // versor direction which maximize the
  // variation of the cost function in question.
  // An additional Exponentiation produce a jump
  // of a particular length along the versor gradient
  // direction.

  VersorType gradientRotation;
  const TParametersValueType norm = axis.GetNorm();
  if (Math::FloatAlmostEqual<TParametersValueType>(norm, 0.0))
    {
    axis[2] = 1;
    gradientRotation.Set(axis, 0.0);
    }
  else
    {
    gradientRotation.Set(axis, factor * norm);
    }

  //
  // Composing the currentRotation with the gradientRotation
  // produces the new Rotation versor
  //
  VersorType newRotation = currentRotation * gradientRotation;

  ParametersType newParameters( numberOfParameters );

  newParameters[0] = newRotation.GetX();
  newParameters[1] = newRotation.GetY();
  newParameters[2] = newRotation.GetZ();

  // Optimize the non-versor parameters as the
  // RegularStepGradientDescentOptimizer
  for ( unsigned int k = 3; k < numberOfParameters; k++ )
    {
    newParameters[k] = this->m_Parameters[k] + update[k] * factor;
    }

  /* Call SetParameters with the updated parameters.
   * SetParameters in most transforms is used to assign the input params
   * to member variables, possibly with some processing. The member variables
   * are then used in TransformPoint.
   * In the case of dense-field transforms that are updated in blocks from
   * a threaded implementation, SetParameters doesn't do this, and is
   * optimized to not copy the input parameters when == m_Parameters.
   */
  this->SetParameters( newParameters );

  /* Call Modified, following behavior of other transform when their
   * parameters change, e.g. MatrixOffsetTransformBase */
  this->Modified();
}

template<typename TParametersValueType>
void
VersorRigid3DTransform<TParametersValueType>
::ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const
{
  typedef typename VersorType::ValueType ValueType;

  // compute derivatives with respect to rotation
  const ValueType vx = this->GetVersor().GetX();
  const ValueType vy = this->GetVersor().GetY();
  const ValueType vz = this->GetVersor().GetZ();
  const ValueType vw = this->GetVersor().GetW();

  jacobian.SetSize( 3, this->GetNumberOfLocalParameters() );
  jacobian.Fill(0.0);

  const double px = p[0] - this->GetCenter()[0];
  const double py = p[1] - this->GetCenter()[1];
  const double pz = p[2] - this->GetCenter()[2];

  const double vxx = vx * vx;
  const double vyy = vy * vy;
  const double vzz = vz * vz;
  const double vww = vw * vw;

  const double vxy = vx * vy;
  const double vxz = vx * vz;
  const double vxw = vx * vw;

  const double vyz = vy * vz;
  const double vyw = vy * vw;

  const double vzw = vz * vw;

  // compute Jacobian with respect to quaternion parameters
  jacobian[0][0] = 2.0 * ( ( vyw + vxz ) * py + ( vzw - vxy ) * pz )
    / vw;
  jacobian[1][0] = 2.0 * ( ( vyw - vxz ) * px   - 2 * vxw   * py + ( vxx - vww ) * pz )
    / vw;
  jacobian[2][0] = 2.0 * ( ( vzw + vxy ) * px + ( vww - vxx ) * py   - 2 * vxw   * pz )
    / vw;

  jacobian[0][1] = 2.0 * ( -2 * vyw  * px + ( vxw + vyz ) * py + ( vww - vyy ) * pz )
    / vw;
  jacobian[1][1] = 2.0 * ( ( vxw - vyz ) * px                + ( vzw + vxy ) * pz )
    / vw;
  jacobian[2][1] = 2.0 * ( ( vyy - vww ) * px + ( vzw - vxy ) * py   - 2 * vyw   * pz )
    / vw;

  jacobian[0][2] = 2.0 * ( -2 * vzw  * px + ( vzz - vww ) * py + ( vxw - vyz ) * pz )
    / vw;
  jacobian[1][2] = 2.0 * ( ( vww - vzz ) * px   - 2 * vzw   * py + ( vyw + vxz ) * pz )
    / vw;
  jacobian[2][2] = 2.0 * ( ( vxw + vyz ) * px + ( vyw - vxz ) * py )
    / vw;

  jacobian[0][3] = 1.0;
  jacobian[1][4] = 1.0;
  jacobian[2][5] = 1.0;
}

// Print self
template<typename TParametersValueType>
void
VersorRigid3DTransform<TParametersValueType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // namespace

#endif
