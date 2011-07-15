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
#ifndef __itkEuler2DTransform_hxx
#define __itkEuler2DTransform_hxx

#include "itkEuler2DTransform.h"

namespace itk
{
// Constructor with default arguments
template< class TScalarType >
Euler2DTransform< TScalarType >
::Euler2DTransform():
  Superclass(SpaceDimension, ParametersDimension)
{}

// Constructor with arguments
template< class TScalarType >
Euler2DTransform< TScalarType >::Euler2DTransform(unsigned int spaceDimension,
                                                  unsigned int parametersDimension):
  Superclass(spaceDimension, parametersDimension)
{}

// Create and return an inverse transformation
template< class TScalarType >
void
Euler2DTransform< TScalarType >::CloneInverseTo(Pointer & result) const
{
  result = New();
  this->GetInverse( result.GetPointer() );
}

// return an inverse transformation
template< class TScalarType >
bool
Euler2DTransform< TScalarType >::GetInverse(Self *inverse) const
{
  if ( !inverse )
    {
    return false;
    }

  inverse->SetCenter( this->GetCenter() );  // inverse have the same center
  inverse->SetAngle( -this->GetAngle() );
  inverse->SetTranslation( -( this->GetInverseMatrix() * this->GetTranslation() ) );

  return true;
}

// Return an inverse of this transform
template< class TScalarType >
typename Euler2DTransform< TScalarType >::InverseTransformBasePointer
Euler2DTransform< TScalarType >
::GetInverseTransform() const
{
  Pointer inv = New();

  return GetInverse(inv) ? inv.GetPointer() : NULL;
}

// Create and return an inverse transformation
template< class TScalarType >
void
Euler2DTransform< TScalarType >::CloneTo(Pointer & result) const
{
  result = New();
  result->SetCenter( this->GetCenter() );
  result->SetAngle( this->GetAngle() );
  result->SetTranslation( this->GetTranslation() );
}

// Print self
template< class TScalarType >
void
Euler2DTransform< TScalarType >::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
}
} // namespace

#endif
