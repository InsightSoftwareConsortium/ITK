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
#ifndef __itkCenteredSimilarity2DTransform_hxx
#define __itkCenteredSimilarity2DTransform_hxx

#include "itkCenteredSimilarity2DTransform.h"

namespace itk
{
// Constructor with default arguments
template <typename TScalar>
CenteredSimilarity2DTransform<TScalar>
::CenteredSimilarity2DTransform() : Superclass(ParametersDimension)
{
}

// Constructor with arguments
template <typename TScalar>
CenteredSimilarity2DTransform<TScalar>::CenteredSimilarity2DTransform(unsigned int spaceDimension,
                                                                          unsigned int parametersDimension) :
  Superclass(spaceDimension, parametersDimension)
{
}

// Set Parameters
template <typename TScalar>
void
CenteredSimilarity2DTransform<TScalar>
::SetParameters(const ParametersType & parameters)
{
  itkDebugMacro(<< "Setting parameters " << parameters);

  // Save parameters. Needed for proper operation of TransformUpdateParameters.
  if( &parameters != &(this->m_Parameters) )
    {
    this->m_Parameters = parameters;
    }

  // Set scale
  const TScalar scale = parameters[0];
  this->SetVarScale(scale);

  // Set angle
  const TScalar angle = parameters[1];
  this->SetVarAngle(angle);

  InputPointType center;
  for( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    center[j] = parameters[j + 2];
    }
  this->SetVarCenter(center);

  // Set translation
  OffsetType translation;
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    translation[i] = parameters[i + 4];
    }

  this->SetVarTranslation(translation);

  this->ComputeMatrix();
  this->ComputeOffset();

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();

  itkDebugMacro(<< "After setting parameters ");
}

// Get Parameters
template <typename TScalar>
const typename CenteredSimilarity2DTransform<TScalar>::ParametersType
& CenteredSimilarity2DTransform<TScalar>
::GetParameters(void) const
  {
  itkDebugMacro(<< "Getting parameters ");

  this->m_Parameters[0] = this->GetScale();
  this->m_Parameters[1] = this->GetAngle();

  InputPointType center = this->GetCenter();
  for( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    this->m_Parameters[j + 2] = center[j];
    }

  OffsetType translation = this->GetTranslation();
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    this->m_Parameters[i + 4] = translation[i];
    }

  itkDebugMacro(<< "After getting parameters " << this->m_Parameters);

  return this->m_Parameters;
  }

template <typename TScalar>
void
CenteredSimilarity2DTransform<TScalar>
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
  jacobian[0][1] = ( -sa * ( p[0] - cx ) - ca * ( p[1] - cy ) )
    * this->GetScale();
  jacobian[1][1] = ( ca * ( p[0] - cx ) - sa * ( p[1] - cy ) )
    * this->GetScale();

  // compute derivatives with respect to the center part
  // first with respect to cx
  jacobian[0][2] = 1.0 - ca * this-> GetScale();
  jacobian[1][2] =     -sa * this->  GetScale();
  // then with respect to cy
  jacobian[0][3] =       sa * this->GetScale();
  jacobian[1][3] = 1.0 - ca * this-> GetScale();

  // compute derivatives with respect to the translation part
  // first with respect to tx
  jacobian[0][4] = 1.0;
  jacobian[1][4] = 0.0;
  // first with respect to ty
  jacobian[0][5] = 0.0;
  jacobian[1][5] = 1.0;
}

template <typename TScalar>
void
CenteredSimilarity2DTransform<TScalar>::SetFixedParameters( const ParametersType & itkNotUsed(parameters) )
{
  // no fixed parameters
}

template <typename TScalar>
const typename CenteredSimilarity2DTransform<TScalar>::ParametersType
& CenteredSimilarity2DTransform<TScalar>::GetFixedParameters(void) const
  {
  // return dummy parameters
  this->m_FixedParameters.SetSize(0);
  return this->m_FixedParameters;
  }

// Print self
template <typename TScalar>
void
CenteredSimilarity2DTransform<TScalar>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

// Create and return an inverse transformation
template <typename TScalar>
void
CenteredSimilarity2DTransform<TScalar>::CloneInverseTo(Pointer & result) const
{
  result = New();
  this->GetInverse( result.GetPointer() );
}

// return an inverse transformation
template <typename TScalar>
bool
CenteredSimilarity2DTransform<TScalar>::GetInverse(Self *inverse) const
{
  if( !inverse )
    {
    return false;
    }

  inverse->SetCenter( this->GetCenter() );  // inverse have the same center
  inverse->SetScale( NumericTraits<double>::One / this->GetScale() );
  inverse->SetAngle( -this->GetAngle() );
  inverse->SetTranslation( -( this->GetInverseMatrix() * this->GetTranslation() ) );
  return true;
}

// Return an inverse of this transform
template <typename TScalar>
typename CenteredSimilarity2DTransform<TScalar>::InverseTransformBasePointer
CenteredSimilarity2DTransform<TScalar>
::GetInverseTransform() const
{
  Pointer inv = New();

  if( this->GetInverse(inv) )
    {
    return inv.GetPointer();
    }
  return ITK_NULLPTR;
}

// Create and return a clone of the transformation
template <typename TScalar>
void
CenteredSimilarity2DTransform<TScalar>::CloneTo(Pointer & result) const
{
  result = New();
  result->SetCenter( this->GetCenter() );
  result->SetScale( this->GetScale() );
  result->SetAngle( this->GetAngle() );
  result->SetTranslation( this->GetTranslation() );
}

} // namespace

#endif
