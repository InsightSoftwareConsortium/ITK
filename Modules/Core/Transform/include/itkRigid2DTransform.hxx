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
#ifndef itkRigid2DTransform_hxx
#define itkRigid2DTransform_hxx

#include "itkRigid2DTransform.h"
#include "vnl/algo/vnl_svd.h"

namespace itk
{

template<typename TParametersValueType>
Rigid2DTransform<TParametersValueType>
::Rigid2DTransform() :
  Superclass(ParametersDimension)
{
  m_Angle = NumericTraits<TParametersValueType>::ZeroValue();
}


template<typename TParametersValueType>
Rigid2DTransform<TParametersValueType>
::Rigid2DTransform(unsigned int parametersDimension) :
  Superclass(parametersDimension)
{
  m_Angle = NumericTraits<TParametersValueType>::ZeroValue();
}


template<typename TParametersValueType>
Rigid2DTransform<TParametersValueType>
::Rigid2DTransform(unsigned int , unsigned int parametersDimension) :
  Superclass(parametersDimension)
{
  m_Angle = NumericTraits<TParametersValueType>::ZeroValue();
}


template<typename TParametersValueType>
Rigid2DTransform<TParametersValueType>::
~Rigid2DTransform()
{
}


template<typename TParametersValueType>
void
Rigid2DTransform<TParametersValueType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Angle       = " << m_Angle        << std::endl;
}


template<typename TParametersValueType>
void
Rigid2DTransform<TParametersValueType>
::SetMatrix(const MatrixType & matrix)
{
  const TParametersValueType tolerance = MatrixOrthogonalityTolerance<TParametersValueType>::GetTolerance();
  this->SetMatrix( matrix, tolerance );
}


template<typename TParametersValueType>
void
Rigid2DTransform<TParametersValueType>
::SetMatrix(const MatrixType & matrix, const TParametersValueType tolerance)
{
  itkDebugMacro("setting  m_Matrix  to " << matrix);
  // The matrix must be orthogonal otherwise it is not
  // representing a valid rotaion in 2D space
  typename MatrixType::InternalMatrixType test =
    matrix.GetVnlMatrix() * matrix.GetTranspose();

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


template<typename TParametersValueType>
void
Rigid2DTransform<TParametersValueType>
::ComputeMatrixParameters()
{
  // Extract the orthogonal part of the matrix
  //
  vnl_matrix<TParametersValueType> p(2, 2);
  p = this->GetMatrix().GetVnlMatrix();
  vnl_svd<TParametersValueType>    svd(p);
  vnl_matrix<TParametersValueType> r(2, 2);
  r = svd.U() * svd.V().transpose();

  m_Angle = std::acos(r[0][0]);

  if( r[1][0] < 0.0 )
    {
    m_Angle = -m_Angle;
    }

  if( r[1][0] - std::sin(m_Angle) > 0.000001 )
    {
    itkWarningMacro( "Bad Rotation Matrix " << this->GetMatrix() );
    }
}


template<typename TParametersValueType>
void
Rigid2DTransform<TParametersValueType>
::Translate(const OffsetType & offset, bool)
{
  OutputVectorType newOffset = this->GetOffset();

  newOffset += offset;
  this->SetOffset(newOffset);
  this->ComputeTranslation();
}


template<typename TParametersValueType>
void
Rigid2DTransform<TParametersValueType>
::CloneInverseTo(Pointer & result) const
{
  result = New();
  this->GetInverse( result.GetPointer() );
}


template<typename TParametersValueType>
bool
Rigid2DTransform<TParametersValueType>
::GetInverse(Self *inverse) const
{
  if( !inverse )
    {
    return false;
    }

  inverse->SetFixedParameters(this->GetFixedParameters());
  inverse->SetCenter( this->GetCenter() );  // inverse have the same center
  inverse->SetAngle( -this->GetAngle() );
  inverse->SetTranslation( -( this->GetInverseMatrix() * this->GetTranslation() ) );

  return true;
}


template<typename TParametersValueType>
typename Rigid2DTransform<TParametersValueType>::InverseTransformBasePointer
Rigid2DTransform<TParametersValueType>
::GetInverseTransform() const
{
  Pointer inv = New();

  return GetInverse(inv) ? inv.GetPointer() : ITK_NULLPTR;
}


template<typename TParametersValueType>
void
Rigid2DTransform<TParametersValueType>
::CloneTo(Pointer & result) const
{
  result = New();
  result->SetCenter( this->GetCenter() );
  result->SetAngle( this->GetAngle() );
  result->SetTranslation( this->GetTranslation() );
}


template<typename TParametersValueType>
void
Rigid2DTransform<TParametersValueType>
::SetIdentity()
{
  this->Superclass::SetIdentity();
  m_Angle = NumericTraits<TParametersValueType>::ZeroValue();
}


template<typename TParametersValueType>
void
Rigid2DTransform<TParametersValueType>
::SetAngle(TParametersValueType angle)
{
  m_Angle = angle;
  this->ComputeMatrix();
  this->ComputeOffset();
  this->Modified();
}


template<typename TParametersValueType>
void
Rigid2DTransform<TParametersValueType>
::SetAngleInDegrees(TParametersValueType angle)
{
  const TParametersValueType angleInRadians = angle * std::atan(1.0) / 45.0;

  this->SetAngle(angleInRadians);
}


template<typename TParametersValueType>
void
Rigid2DTransform<TParametersValueType>
::ComputeMatrix()
{
  const MatrixValueType ca = std::cos(m_Angle);
  const MatrixValueType sa = std::sin(m_Angle);

  MatrixType rotationMatrix;

  rotationMatrix[0][0] = ca; rotationMatrix[0][1] = -sa;
  rotationMatrix[1][0] = sa; rotationMatrix[1][1] = ca;

  this->SetVarMatrix(rotationMatrix);
}


template<typename TParametersValueType>
void
Rigid2DTransform<TParametersValueType>
::SetParameters(const ParametersType & parameters)
{
  itkDebugMacro(<< "Setting parameters " << parameters);

  // Save parameters. Needed for proper operation of TransformUpdateParameters.
  if( &parameters != &(this->m_Parameters) )
    {
    this->m_Parameters = parameters;
    }

  // Set angle
  const TParametersValueType angle = parameters[0];
  this->SetVarAngle(angle);

  // Set translation
  OutputVectorType translation;
  for( unsigned int i = 0; i < OutputSpaceDimension; i++ )
    {
    translation[i] = parameters[i + 1];
    }
  this->SetVarTranslation(translation);

  // Update matrix and offset
  this->ComputeMatrix();
  this->ComputeOffset();

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();

  itkDebugMacro(<< "After setting parameters ");
}


template<typename TParametersValueType>
const typename Rigid2DTransform<TParametersValueType>::ParametersType &
Rigid2DTransform<TParametersValueType>
::GetParameters() const
{
  itkDebugMacro(<< "Getting parameters ");

  // Get the angle
  this->m_Parameters[0] = this->GetAngle();
  // Get the translation
  for( unsigned int i = 0; i < OutputSpaceDimension; i++ )
    {
    this->m_Parameters[i + 1] = this->GetTranslation()[i];
    }

  itkDebugMacro(<< "After getting parameters " << this->m_Parameters);

  return this->m_Parameters;
}


template<typename TParametersValueType>
void
Rigid2DTransform<TParametersValueType>
::ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & j ) const
{
  j.SetSize( OutputSpaceDimension, this->GetNumberOfLocalParameters() );
  j.Fill(0.0);

  const double ca = std::cos( this->GetAngle() );
  const double sa = std::sin( this->GetAngle() );

  const double cx = this->GetCenter()[0];
  const double cy = this->GetCenter()[1];

  // derivatives with respect to the angle
  j[0][0] = -sa * ( p[0] - cx ) - ca * ( p[1] - cy );
  j[1][0] =  ca * ( p[0] - cx ) - sa * ( p[1] - cy );

  // compute derivatives for the translation part
  unsigned int blockOffset = 1;
  for( unsigned int dim = 0; dim < OutputSpaceDimension; dim++ )
    {
    j[dim][blockOffset + dim] = 1.0;
    }
}

#ifdef ITKV3_COMPATIBILITY
#if !defined(ITK_LEGACY_REMOVE)
template<typename TParametersValueType>
void
Rigid2DTransform<TParametersValueType>
::SetRotationMatrix(const MatrixType & matrix)
{
  this->SetMatrix(matrix);
}

template<typename TParametersValueType>
const typename Rigid2DTransform<TParametersValueType>::MatrixType &
Rigid2DTransform<TParametersValueType>
::GetRotationMatrix() const
{
  return this->GetMatrix();
}
#endif
#endif
} // namespace

#endif
