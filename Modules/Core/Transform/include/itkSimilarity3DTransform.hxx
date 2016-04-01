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
#ifndef itkSimilarity3DTransform_hxx
#define itkSimilarity3DTransform_hxx

#include "itkSimilarity3DTransform.h"
#include "itkMath.h"
#include "vnl/vnl_det.h"

namespace itk
{
// Constructor with default arguments
template<typename TParametersValueType>
Similarity3DTransform<TParametersValueType>
::Similarity3DTransform() :
  Superclass(ParametersDimension),
  m_Scale(1.0)
{
}

// Constructor with arguments
template<typename TParametersValueType>
Similarity3DTransform<TParametersValueType>
::Similarity3DTransform(unsigned int paramDim) :
  Superclass(paramDim),
  m_Scale(1.0)
{
}

// Constructor with arguments
template<typename TParametersValueType>
Similarity3DTransform<TParametersValueType>
::Similarity3DTransform(const MatrixType & matrix, const OutputVectorType & offset) :
  Superclass(matrix, offset),
  m_Scale(1.0)
{
}

// / Set the parameters to the IdentityTransform
template<typename TParametersValueType>
void
Similarity3DTransform<TParametersValueType>
::SetIdentity(void)
{
  this->Superclass::SetIdentity();
  this->m_Scale = 1.0;
}

// Set the scale factor
template<typename TParametersValueType>
void
Similarity3DTransform<TParametersValueType>
::SetScale(ScaleType scale)
{
  m_Scale = scale;
  this->ComputeMatrix();
}

// Directly set the matrix
template<typename TParametersValueType>
void
Similarity3DTransform<TParametersValueType>
::SetMatrix(const MatrixType & matrix)
{
  const TParametersValueType tolerance = MatrixOrthogonalityTolerance<TParametersValueType>::GetTolerance();
  this->SetMatrix( matrix, tolerance );
}

template<typename TParametersValueType>
void
Similarity3DTransform<TParametersValueType>
::SetMatrix(const MatrixType & matrix, const TParametersValueType tolerance)
{
  //
  // Since the matrix should be an orthogonal matrix
  // multiplied by the scale factor, then its determinant
  // must be equal to the cube of the scale factor.
  //
  double det = vnl_det( matrix.GetVnlMatrix() );

  if( det == 0.0 )
    {
    itkExceptionMacro(<< "Attempting to set a matrix with a zero determinant");
    }

  //
  // A negative scale is not acceptable
  // It will imply a reflection of the coordinate system.
  //

  double s = itk::Math::cbrt(det);

  //
  // A negative scale is not acceptable
  // It will imply a reflection of the coordinate system.
  //
  if( s <= 0.0 )
    {
    itkExceptionMacro(<< "Attempting to set a matrix with a negative trace");
    }

  MatrixType testForOrthogonal = matrix;
  testForOrthogonal /= s;

  if( !this->MatrixIsOrthogonal(testForOrthogonal, tolerance) )
    {
    itkExceptionMacro(<< "Attempting to set a non-orthogonal matrix (after removing scaling)");
    }

  typedef MatrixOffsetTransformBase<TParametersValueType, 3, 3> Baseclass;
  this->Baseclass::SetMatrix(matrix);
}

// Set Parameters
template<typename TParametersValueType>
void
Similarity3DTransform<TParametersValueType>
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
  m_Scale = parameters[6]; // must be set before calling ComputeMatrix();
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
// p[6:6} = scaling factor (isotropic)
//

template<typename TParametersValueType>
const typename Similarity3DTransform<TParametersValueType>::ParametersType
& Similarity3DTransform<TParametersValueType>
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

  this->m_Parameters[6] = this->GetScale();

  itkDebugMacro(<< "After getting parameters " << this->m_Parameters);

  return this->m_Parameters;
  }

template<typename TParametersValueType>
void
Similarity3DTransform<TParametersValueType>::ComputeJacobianWithRespectToParameters(const InputPointType & p,
                                                                           JacobianType & jacobian) const
{
  typedef typename VersorType::ValueType ValueType;

  // compute derivatives with respect to rotation
  const ValueType vx = this->GetVersor().GetX();
  const ValueType vy = this->GetVersor().GetY();
  const ValueType vz = this->GetVersor().GetZ();
  const ValueType vw = this->GetVersor().GetW();

  jacobian.SetSize( 3, this->GetNumberOfLocalParameters() );
  jacobian.Fill(0.0);

  const InputVectorType pp = p - this->GetCenter();

  const double px = pp[0];
  const double py = pp[1];
  const double pz = pp[2];

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
  jacobian[0][0] = m_Scale * 2.0 * ( ( vyw + vxz ) * py + ( vzw - vxy ) * pz )
    / vw;
  jacobian[1][0] = m_Scale * 2.0 * ( ( vyw - vxz ) * px   - 2 * vxw   * py + ( vxx - vww ) * pz )
    / vw;
  jacobian[2][0] = m_Scale * 2.0 * ( ( vzw + vxy ) * px + ( vww - vxx ) * py   - 2 * vxw   * pz )
    / vw;

  jacobian[0][1] = m_Scale * 2.0 * ( -2 * vyw  * px + ( vxw + vyz ) * py + ( vww - vyy ) * pz )
    / vw;
  jacobian[1][1] = m_Scale * 2.0 * ( ( vxw - vyz ) * px                + ( vzw + vxy ) * pz )
    / vw;
  jacobian[2][1] = m_Scale * 2.0 * ( ( vyy - vww ) * px + ( vzw - vxy ) * py   - 2 * vyw   * pz )
    / vw;

  jacobian[0][2] = m_Scale * 2.0 * ( -2 * vzw  * px + ( vzz - vww ) * py + ( vxw - vyz ) * pz )
    / vw;
  jacobian[1][2] = m_Scale * 2.0 * ( ( vww - vzz ) * px   - 2 * vzw   * py + ( vyw + vxz ) * pz )
    / vw;
  jacobian[2][2] = m_Scale * 2.0 * ( ( vxw + vyz ) * px + ( vyw - vxz ) * py )
    / vw;

  // compute Jacobian with respect to the translation parameters
  jacobian[0][3] = 1.0;
  jacobian[1][4] = 1.0;
  jacobian[2][5] = 1.0;

  // compute Jacobian with respect to the scale parameter
  const MatrixType & matrix = this->GetMatrix();

  const InputVectorType mpp = matrix * pp;

  jacobian[0][6] = mpp[0] / m_Scale;
  jacobian[1][6] = mpp[1] / m_Scale;
  jacobian[2][6] = mpp[2] / m_Scale;
}

// Set the scale factor
template<typename TParametersValueType>
void
Similarity3DTransform<TParametersValueType>
::ComputeMatrix()
{
  this->Superclass::ComputeMatrix();
  MatrixType newMatrix = this->GetMatrix();
  newMatrix *= m_Scale;
  this->SetVarMatrix(newMatrix);
}

/** Compute the matrix */
template<typename TParametersValueType>
void
Similarity3DTransform<TParametersValueType>
::ComputeMatrixParameters(void)
{
  MatrixType matrix = this->GetMatrix();

  m_Scale = itk::Math::cbrt( vnl_det( matrix.GetVnlMatrix() ) );

  matrix /= m_Scale;

  VersorType v;
  v.Set(matrix);
  this->SetVarVersor(v);
}

// Print self
template<typename TParametersValueType>
void
Similarity3DTransform<TParametersValueType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Scale = " << m_Scale << std::endl;
}

} // namespace

#endif
