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
#ifndef itkVersorTransform_hxx
#define itkVersorTransform_hxx

#include "itkVersorTransform.h"
#include "itkMath.h"

namespace itk
{
/** Constructor with default arguments */
template<typename TParametersValueType>
VersorTransform<TParametersValueType>
::VersorTransform() : Superclass(ParametersDimension)
{
  m_Versor.SetIdentity();
}

/** Constructor with default arguments */
template<typename TParametersValueType>
VersorTransform<TParametersValueType>::VersorTransform(unsigned int parametersDimension) :
  Superclass(parametersDimension)
{
  m_Versor.SetIdentity();
}

/** Constructor with default arguments */
template<typename TParametersValueType>
VersorTransform<TParametersValueType>::VersorTransform(const MatrixType & matrix,
                                              const OutputVectorType & offset) : Superclass(matrix, offset)
{
  this->ComputeMatrixParameters();  // called in MatrixOffset baseclass
}

/** Set Parameters */
template<typename TParametersValueType>
void
VersorTransform<TParametersValueType>
::SetParameters(const ParametersType & parameters)
{
  itkDebugMacro(<< "Setting parameters " << parameters);

  // Save parameters. Needed for proper operation of TransformUpdateParameters.
  if( &parameters != &(this->m_Parameters) )
    {
    this->m_Parameters = parameters;
    }

  // Transfer the versor part
  AxisType rightPart;

  rightPart[0] = parameters[0];
  rightPart[1] = parameters[1];
  rightPart[2] = parameters[2];

  // The versor will compute the scalar part.
  m_Versor.Set(rightPart);

  itkDebugMacro(<< "Versor is now " << m_Versor);

  this->ComputeMatrix();

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();

  itkDebugMacro(<< "After setting parameters ");
}

/** Set Parameters */
template<typename TParametersValueType>
const typename VersorTransform<TParametersValueType>::ParametersType
& VersorTransform<TParametersValueType>
::GetParameters(void) const
  {
  this->m_Parameters[0] = this->m_Versor.GetRight()[0];
  this->m_Parameters[1] = this->m_Versor.GetRight()[1];
  this->m_Parameters[2] = this->m_Versor.GetRight()[2];

  return this->m_Parameters;
  }

/** Set Rotational Part */
template<typename TParametersValueType>
void
VersorTransform<TParametersValueType>
::SetRotation(const VersorType & versor)
{
  m_Versor = versor;
  this->ComputeMatrix();
  this->ComputeOffset();
}

/** Set Rotational Part */
template<typename TParametersValueType>
void
VersorTransform<TParametersValueType>
::SetRotation(const AxisType & axis, AngleType angle)
{
  m_Versor.Set(axis, angle);
  this->ComputeMatrix();
  this->ComputeOffset();
}

/** Set Identity */
template<typename TParametersValueType>
void
VersorTransform<TParametersValueType>
::SetIdentity()
{
  Superclass::SetIdentity();

  m_Versor.SetIdentity();

  this->Modified();
}

/** Compute the matrix */
template<typename TParametersValueType>
void
VersorTransform<TParametersValueType>
::ComputeMatrix(void)
{
  this->SetVarMatrix( m_Versor.GetMatrix() );
}

/** Compute the matrix */
template<typename TParametersValueType>
void
VersorTransform<TParametersValueType>
::ComputeMatrixParameters(void)
{
  m_Versor.Set( this->GetMatrix() );
}

template<typename TParametersValueType>
void
VersorTransform<TParametersValueType>
::ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const
{
  typedef typename VersorType::ValueType ValueType;

  // compute derivatives with respect to rotation
  const ValueType vx = m_Versor.GetX();
  const ValueType vy = m_Versor.GetY();
  const ValueType vz = m_Versor.GetZ();
  const ValueType vw = m_Versor.GetW();

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
}

/** Print self */
template<typename TParametersValueType>
void
VersorTransform<TParametersValueType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Versor: " << m_Versor  << std::endl;
}

#ifdef ITKV3_COMPATIBILITY
#if !defined(ITK_LEGACY_REMOVE)
template<typename TParametersValueType>
void
VersorTransform<TParametersValueType>::SetRotationMatrix(const MatrixType & matrix)
{
  this->Superclass::SetMatrix(matrix);
}
#endif
#endif

} // namespace

#endif
