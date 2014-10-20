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
#ifndef __itkRigid2DTransform_hxx
#define __itkRigid2DTransform_hxx

#include "itkRigid2DTransform.h"
#include "vnl/algo/vnl_svd.h"

namespace itk
{
// Constructor with default arguments
template <typename TScalar>
Rigid2DTransform<TScalar>::Rigid2DTransform() :
  Superclass(ParametersDimension)
{
  m_Angle = NumericTraits<TScalar>::Zero;
}

// Constructor with arguments
template <typename TScalar>
Rigid2DTransform<TScalar>::Rigid2DTransform(unsigned int parametersDimension) :
  Superclass(parametersDimension)
{
  m_Angle = NumericTraits<TScalar>::Zero;
}

template <typename TScalar>
Rigid2DTransform<TScalar>::Rigid2DTransform(unsigned int , unsigned int parametersDimension) :
  Superclass(parametersDimension)
{
  m_Angle = NumericTraits<TScalar>::Zero;
}

// Destructor
template <typename TScalar>
Rigid2DTransform<TScalar>::
~Rigid2DTransform()
{
}

// Print self
template <typename TScalar>
void
Rigid2DTransform<TScalar>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Angle       = " << m_Angle        << std::endl;
}

// Set the rotation matrix
template <typename TScalar>
void
Rigid2DTransform<TScalar>::SetMatrix(const MatrixType & matrix)
{
  const double tolerance = 1e-10;
  this->SetMatrix( matrix, tolerance );
}

// Set the rotation matrix with specified tolerance
template <typename TScalar>
void
Rigid2DTransform<TScalar>::SetMatrix(const MatrixType & matrix, double tolerance)
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

/** Compute the Angle from the Rotation Matrix */
template <typename TScalar>
void
Rigid2DTransform<TScalar>
::ComputeMatrixParameters(void)
{
  // Extract the orthogonal part of the matrix
  //
  vnl_matrix<TScalar> p(2, 2);
  p = this->GetMatrix().GetVnlMatrix();
  vnl_svd<TScalar>    svd(p);
  vnl_matrix<TScalar> r(2, 2);
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

// Compose with a translation
template <typename TScalar>
void
Rigid2DTransform<TScalar>::Translate(const OffsetType & offset, bool)
{
  OutputVectorType newOffset = this->GetOffset();

  newOffset += offset;
  this->SetOffset(newOffset);
  this->ComputeTranslation();
}

// Create and return an inverse transformation
template <typename TScalar>
void
Rigid2DTransform<TScalar>::CloneInverseTo(Pointer & result) const
{
  result = New();
  this->GetInverse( result.GetPointer() );
}

// return an inverse transformation
template <typename TScalar>
bool
Rigid2DTransform<TScalar>::GetInverse(Self *inverse) const
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

// Return an inverse of this transform
template <typename TScalar>
typename Rigid2DTransform<TScalar>::InverseTransformBasePointer
Rigid2DTransform<TScalar>
::GetInverseTransform() const
{
  Pointer inv = New();

  return GetInverse(inv) ? inv.GetPointer() : ITK_NULLPTR;
}

// Create and return a clone of the transformation
template <typename TScalar>
void
Rigid2DTransform<TScalar>::CloneTo(Pointer & result) const
{
  result = New();
  result->SetCenter( this->GetCenter() );
  result->SetAngle( this->GetAngle() );
  result->SetTranslation( this->GetTranslation() );
}

// Reset the transform to an identity transform
template <typename TScalar>
void
Rigid2DTransform<TScalar>::SetIdentity(void)
{
  this->Superclass::SetIdentity();
  m_Angle = NumericTraits<TScalar>::Zero;
}

// Set the angle of rotation
template <typename TScalar>
void
Rigid2DTransform<TScalar>
::SetAngle(TScalar angle)
{
  m_Angle = angle;
  this->ComputeMatrix();
  this->ComputeOffset();
  this->Modified();
}

// Set the angle of rotation
template <typename TScalar>
void
Rigid2DTransform<TScalar>
::SetAngleInDegrees(TScalar angle)
{
  const TScalar angleInRadians = angle * std::atan(1.0) / 45.0;

  this->SetAngle(angleInRadians);
}

// Compute the matrix from the angle
template <typename TScalar>
void
Rigid2DTransform<TScalar>
::ComputeMatrix(void)
{
  const MatrixValueType ca = std::cos(m_Angle);
  const MatrixValueType sa = std::sin(m_Angle);

  MatrixType rotationMatrix;

  rotationMatrix[0][0] = ca; rotationMatrix[0][1] = -sa;
  rotationMatrix[1][0] = sa; rotationMatrix[1][1] = ca;

  this->SetVarMatrix(rotationMatrix);
}

// Set Parameters
template <typename TScalar>
void
Rigid2DTransform<TScalar>::SetParameters(const ParametersType & parameters)
{
  itkDebugMacro(<< "Setting parameters " << parameters);

  // Save parameters. Needed for proper operation of TransformUpdateParameters.
  if( &parameters != &(this->m_Parameters) )
    {
    this->m_Parameters = parameters;
    }

  // Set angle
  const TScalar angle = parameters[0];
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

// Get Parameters
template <typename TScalar>
const typename Rigid2DTransform<TScalar>::ParametersType
& Rigid2DTransform<TScalar>::GetParameters(void) const
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

// Compute transformation Jacobian
template <typename TScalar>
void
Rigid2DTransform<TScalar>::ComputeJacobianWithRespectToParameters(const InputPointType & p,
                                                                      JacobianType & j ) const
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
template <typename TScalar>
void
Rigid2DTransform<TScalar>::SetRotationMatrix(const MatrixType & matrix)
{
  this->SetMatrix(matrix);
}

template <typename TScalar>
const typename Rigid2DTransform<TScalar>::MatrixType &
Rigid2DTransform<TScalar>::GetRotationMatrix() const
{
  return this->GetMatrix();
}
#endif
#endif
} // namespace

#endif
