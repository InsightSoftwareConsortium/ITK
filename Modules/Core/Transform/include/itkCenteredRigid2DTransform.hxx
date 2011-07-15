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
#ifndef __itkCenteredRigid2DTransform_hxx
#define __itkCenteredRigid2DTransform_hxx

#include "itkCenteredRigid2DTransform.h"

namespace itk
{
// Constructor with default arguments
template< class TScalarType >
CenteredRigid2DTransform< TScalarType >
::CenteredRigid2DTransform():
  Superclass(OutputSpaceDimension, ParametersDimension)
{}

// Constructor with arguments
template< class TScalarType >
CenteredRigid2DTransform< TScalarType >::CenteredRigid2DTransform(unsigned int spaceDimension,
                                                                  unsigned int parametersDimension):
  Superclass(spaceDimension, parametersDimension)
{}

//
// Set Parameters
//
// Parameters are ordered as:
//
// p[0]   = angle
// p[1:2} = center of rotation coordinates
// p[3:4} = translation components
//
//
template< class TScalarType >
void
CenteredRigid2DTransform< TScalarType >
::SetParameters(const ParametersType & parameters)
{
  itkDebugMacro(<< "Setting parameters " << parameters);

  this->m_Parameters = parameters;

  // Set the angle
  const TScalarType angle = parameters[0];
  this->SetVarAngle(angle);
  // Set the center
  InputPointType center;

  for ( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    center[i] = parameters[i + 1];
    }
  this->SetVarCenter(center);

  // Set the translation
  OutputVectorType translation;

  for ( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    translation[j] = parameters[j + 1 + SpaceDimension];
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

//
// Get Parameters
//
// Parameters are ordered as:
//
// p[0]   = angle
// p[1:2} = center of rotation coordinates
// p[3:4} = translation components
//
template< class TScalarType >
const typename CenteredRigid2DTransform< TScalarType >::ParametersType &
CenteredRigid2DTransform< TScalarType >
::GetParameters(void) const
{
  itkDebugMacro(<< "Getting parameters ");

  // Get the angle
  this->m_Parameters[0] = this->GetAngle();

  // Get the center
  for ( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    this->m_Parameters[i + 1] = this->GetCenter()[i];
    }

  // Get the translation
  for ( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    this->m_Parameters[j + 1 + SpaceDimension] = this->GetTranslation()[j];
    }

  itkDebugMacro(<< "After getting parameters " << this->m_Parameters);

  return this->m_Parameters;
}

// Compute the transformation Jacobian
template< class TScalarType >
const typename CenteredRigid2DTransform< TScalarType >::JacobianType &
CenteredRigid2DTransform< TScalarType >::GetJacobian(const InputPointType & p) const
{
  const double ca = vcl_cos( this->GetAngle() );
  const double sa = vcl_sin( this->GetAngle() );

  this->m_Jacobian.Fill(0.0);

  const double cx = this->GetCenter()[0];
  const double cy = this->GetCenter()[1];

  // derivatives with respect to the angle
  this->m_Jacobian[0][0] = -sa * ( p[0] - cx ) - ca * ( p[1] - cy );
  this->m_Jacobian[1][0] =  ca * ( p[0] - cx ) - sa * ( p[1] - cy );

  // compute derivatives with respect to the center part
  // first with respect to cx
  this->m_Jacobian[0][1] = 1.0 - ca;
  this->m_Jacobian[1][1] =     -sa;
  // then with respect to cy
  this->m_Jacobian[0][2] =       sa;
  this->m_Jacobian[1][2] = 1.0 - ca;

  // compute derivatives with respect to the translation part
  // first with respect to tx
  this->m_Jacobian[0][3] = 1.0;
  this->m_Jacobian[1][3] = 0.0;
  // first with respect to ty
  this->m_Jacobian[0][4] = 0.0;
  this->m_Jacobian[1][4] = 1.0;

  return this->m_Jacobian;
}

template< class TScalarType >
void
CenteredRigid2DTransform< TScalarType >
::SetFixedParameters( const ParametersType & itkNotUsed(parameters) )
{
  // no fixed parameters
}

template< class TScalarType >
const typename CenteredRigid2DTransform< TScalarType >::ParametersType &
CenteredRigid2DTransform< TScalarType >
::GetFixedParameters(void) const
{
  // return dummy parameters
  this->m_FixedParameters.SetSize(0);
  return this->m_FixedParameters;
}

// Create and return an inverse transformation
template< class TScalarType >
void
CenteredRigid2DTransform< TScalarType >::CloneInverseTo(Pointer & result) const
{
  result = New();
  this->GetInverse( result.GetPointer() );
}

// return an inverse transformation
template< class TScalarType >
bool
CenteredRigid2DTransform< TScalarType >::GetInverse(Self *inverse) const
{
  if ( !inverse )
    {
    return false;
    }

  inverse->SetCenter( this->GetCenter() );  // inverse have the same center
  inverse->SetAngle( -this->GetAngle() );
  inverse->SetTranslation( -( this->GetInverseMatrix()
                              * this->GetTranslation() ) );
  return true;
}

// Return an inverse of this transform
template< class TScalarType >
typename CenteredRigid2DTransform< TScalarType >::InverseTransformBasePointer
CenteredRigid2DTransform< TScalarType >
::GetInverseTransform() const
{
  Pointer inv = New();

  return GetInverse(inv) ? inv.GetPointer() : NULL;
}

// Create and return an clone transformation
template< class TScalarType >
void
CenteredRigid2DTransform< TScalarType >::CloneTo(Pointer & result) const
{
  result = New();
  result->SetCenter( this->GetCenter() );
  result->SetAngle( this->GetAngle() );
  result->SetTranslation( this->GetTranslation() );
}

// Print self
template< class TScalarType >
void
CenteredRigid2DTransform< TScalarType >::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
}
} // namespace

#endif
