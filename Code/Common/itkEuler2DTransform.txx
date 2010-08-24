/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEuler2DTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkEuler2DTransform_txx
#define __itkEuler2DTransform_txx

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
