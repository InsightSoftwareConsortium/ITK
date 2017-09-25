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
#ifndef itkSimilarity2DTransform_hxx
#define itkSimilarity2DTransform_hxx

#include "itkSimilarity2DTransform.h"
#include "itkMath.h"

namespace itk
{

template<typename TParametersValueType>
Similarity2DTransform<TParametersValueType>
::Similarity2DTransform() : Superclass(ParametersDimension)
{
  m_Scale = 1.0f;
}


template<typename TParametersValueType>
Similarity2DTransform<TParametersValueType>
::Similarity2DTransform(unsigned int parametersDimension) :
  Superclass(parametersDimension)
{
  m_Scale = 1.0f;
}


template<typename TParametersValueType>
Similarity2DTransform<TParametersValueType>
::Similarity2DTransform(unsigned int , unsigned int parametersDimension) :
  Superclass(parametersDimension)
{
  m_Scale = 1.0f;
}


template<typename TParametersValueType>
void
Similarity2DTransform<TParametersValueType>
::SetParameters(const ParametersType & parameters)
{
  itkDebugMacro(<< "Setting parameters " << parameters);

  // Save parameters. Needed for proper operation of TransformUpdateParameters.
  if( &parameters != &(this->m_Parameters) )
    {
    this->m_Parameters = parameters;
    }

  // Set scale
  const TParametersValueType scale = parameters[0];
  this->SetVarScale(scale);

  // Set angle
  const TParametersValueType angle = parameters[1];
  this->SetVarAngle(angle);

  // Set translation
  OffsetType translation;
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    translation[i] = parameters[i + 2];
    }
  this->SetVarTranslation(translation);

  this->ComputeMatrix();
  this->ComputeOffset();

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();

  itkDebugMacro(<< "After setting parameters ");
}


template<typename TParametersValueType>
const typename Similarity2DTransform<TParametersValueType>::ParametersType &
Similarity2DTransform<TParametersValueType>
::GetParameters() const
{
  itkDebugMacro(<< "Getting parameters ");

  this->m_Parameters[0] = this->GetScale();
  this->m_Parameters[1] = this->GetAngle();

  // Get the translation
  OffsetType translation = this->GetTranslation();
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    this->m_Parameters[i + 2] = translation[i];
    }

  itkDebugMacro(<< "After getting parameters " << this->m_Parameters);

  return this->m_Parameters;
}


template<typename TParametersValueType>
void
Similarity2DTransform<TParametersValueType>
::SetScale(ScaleType scale)
{
  m_Scale = scale;
  this->ComputeMatrix();
  this->ComputeOffset();
}


template<typename TParametersValueType>
void
Similarity2DTransform<TParametersValueType>
::ComputeMatrix()
{
  const double angle = this->GetAngle();

  const double cc = std::cos(angle);
  const double ss = std::sin(angle);

  const MatrixValueType ca = cc * m_Scale;
  const MatrixValueType sa = ss * m_Scale;

  MatrixType matrix;

  matrix[0][0] = ca; matrix[0][1] = -sa;
  matrix[1][0] = sa; matrix[1][1] = ca;

  this->SetVarMatrix(matrix);
}


template<typename TParametersValueType>
void
Similarity2DTransform<TParametersValueType>
::ComputeMatrixParameters()
{
  m_Scale = std::sqrt( itk::Math::sqr(this->GetMatrix()[0][0])
                      + itk::Math::sqr(this->GetMatrix()[0][1]) );

  this->SetVarAngle( std::acos(this->GetMatrix()[0][0] / m_Scale) );

  if( this->GetMatrix()[1][0] < 0.0 )
    {
    this->SetVarAngle( -this->GetAngle() );
    }

  if( ( this->GetMatrix()[1][0] / m_Scale ) - std::sin( this->GetAngle() ) > 0.000001 )
    {
    itkExceptionMacro(<< "Bad Rotation Matrix");
    }
}


template<typename TParametersValueType>
void
Similarity2DTransform<TParametersValueType>
::ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const
{
  const double angle = this->GetAngle();
  const double ca = std::cos(angle);
  const double sa = std::sin(angle);

  jacobian.SetSize( 2, this->GetNumberOfLocalParameters() );
  jacobian.Fill(0.0);

  const InputPointType center = this->GetCenter();
  const double         cx = center[0];
  const double         cy = center[1];

  // derivatives with respect to the scale
  jacobian[0][0] =    ca * ( p[0] - cx ) - sa * ( p[1] - cy );
  jacobian[1][0] =    sa * ( p[0] - cx ) + ca * ( p[1] - cy );

  // derivatives with respect to the angle
  jacobian[0][1] = ( -sa * ( p[0] - cx ) - ca * ( p[1] - cy ) ) * m_Scale;
  jacobian[1][1] = ( ca * ( p[0] - cx ) - sa * ( p[1] - cy ) ) * m_Scale;

  // compute derivatives with respect to the translation part
  // first with respect to tx
  jacobian[0][2] = 1.0;
  jacobian[1][2] = 0.0;
  // first with respect to ty
  jacobian[0][3] = 0.0;
  jacobian[1][3] = 1.0;
}


template<typename TParametersValueType>
void
Similarity2DTransform<TParametersValueType>
::SetIdentity()
{
  this->Superclass::SetIdentity();
  m_Scale = static_cast<TParametersValueType>( 1.0f );
}


template<typename TParametersValueType>
void
Similarity2DTransform<TParametersValueType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Scale =" << m_Scale  << std::endl;
}


template<typename TParametersValueType>
void
Similarity2DTransform<TParametersValueType>
::CloneInverseTo(Pointer & result) const
{
  result = New();
  this->GetInverse( result.GetPointer() );
}


template<typename TParametersValueType>
bool
Similarity2DTransform<TParametersValueType>
::GetInverse(Self *inverse) const
{
  if( !inverse )
    {
    return false;
    }

  inverse->SetFixedParameters(this->GetFixedParameters());
  this->GetInverseMatrix();
  if( this->GetSingular() )
    {
    return false;
    }
  inverse->SetCenter( this->GetCenter() );  // inverse have the same center
  inverse->SetScale( NumericTraits<double>::OneValue() / this->GetScale() );
  inverse->SetAngle( -this->GetAngle() );
  inverse->SetTranslation( -( this->GetInverseMatrix() * this->GetTranslation() ) );

  return true;
}


template<typename TParametersValueType>
typename Similarity2DTransform<TParametersValueType>::InverseTransformBasePointer
Similarity2DTransform<TParametersValueType>
::GetInverseTransform() const
{
  Pointer inv = New();

  if( this->GetInverse(inv) )
    {
    return inv.GetPointer();
    }
  return ITK_NULLPTR;
}


template<typename TParametersValueType>
void
Similarity2DTransform<TParametersValueType>::CloneTo(Pointer & result) const
{
  result = New();
  result->SetCenter( this->GetCenter() );
  result->SetScale( this->GetScale() );
  result->SetAngle( this->GetAngle() );
  result->SetTranslation( this->GetTranslation() );
}


template<typename TParametersValueType>
void
Similarity2DTransform<TParametersValueType>
::SetMatrix(const MatrixType & matrix)
{
  const TParametersValueType tolerance = MatrixOrthogonalityTolerance<TParametersValueType>::GetTolerance();
  this->SetMatrix( matrix, tolerance );
}


template<typename TParametersValueType>
void
Similarity2DTransform<TParametersValueType>
::SetMatrix(const MatrixType & matrix, const TParametersValueType tolerance)
{
  itkDebugMacro("setting  m_Matrix  to " << matrix);

  typename MatrixType::InternalMatrixType test =
    matrix.GetVnlMatrix() * matrix.GetTranspose();

  test /= test[0][0]; // factor out the scale

  if( !test.is_identity(tolerance) )
    {
    itk::ExceptionObject ex(__FILE__, __LINE__, "Attempt to set a Non-Orthogonal matrix", ITK_LOCATION);
    throw ex;
    }

  this->SetVarMatrix(matrix);
  this->ComputeOffset();
  this->ComputeMatrixParameters();
  this->Modified();
}

} // end namespace itk

#endif
