/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCenteredSimilarity2DTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCenteredSimilarity2DTransform_txx
#define __itkCenteredSimilarity2DTransform_txx

#include "itkCenteredSimilarity2DTransform.h"


namespace itk
{

// Constructor with default arguments
template <class TScalarType>
CenteredSimilarity2DTransform<TScalarType>
::CenteredSimilarity2DTransform():Superclass(OutputSpaceDimension,
                                             ParametersDimension)
{
}


// Constructor with arguments
template<class TScalarType>
CenteredSimilarity2DTransform<TScalarType>::
CenteredSimilarity2DTransform( unsigned int spaceDimension, 
                  unsigned int parametersDimension):
  Superclass(spaceDimension,parametersDimension)
{

}


// Set Parameters
template <class TScalarType>
void
CenteredSimilarity2DTransform<TScalarType>
::SetParameters( const ParametersType & parameters )
{
  itkDebugMacro( << "Setting parameters " << parameters );

  // Set scale
  this->SetVarScale(parameters[0]);
 
  // Set angle
  this->SetVarAngle( parameters[1] );

  InputPointType center;
  for(unsigned int j=0; j < SpaceDimension; j++) 
    {
    center[j] = parameters[j+2];
    }
  this->SetVarCenter( center );

  // Set translation
  OffsetType translation;
  for(unsigned int i=0; i < SpaceDimension; i++) 
    {
    translation[i] = parameters[i+4];
    }

  this->SetVarTranslation( translation );

  this->ComputeMatrix();
  this->ComputeOffset();

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();

  itkDebugMacro(<<"After setting parameters ");
}


// Get Parameters
template <class TScalarType>
const typename CenteredSimilarity2DTransform<TScalarType>::ParametersType &
CenteredSimilarity2DTransform<TScalarType>
::GetParameters( void ) const
{
  itkDebugMacro( << "Getting parameters ");

  this->m_Parameters[0] = this->GetScale();
  this->m_Parameters[1] = this->GetAngle();
 
  InputPointType center = this->GetCenter();
  for(unsigned int j=0; j < SpaceDimension; j++) 
    {
    this->m_Parameters[j+2] = center[j];
    }

  OffsetType translation = this->GetTranslation();
  for(unsigned int i=0; i < SpaceDimension; i++) 
    {
    this->m_Parameters[i+4] = translation[i];
    }

  itkDebugMacro(<<"After getting parameters " << this->m_Parameters );

  return this->m_Parameters;
}

// Compute the Jacobian
template<class TScalarType>
const typename CenteredSimilarity2DTransform<TScalarType>::JacobianType &
CenteredSimilarity2DTransform<TScalarType>::
GetJacobian( const InputPointType & p ) const
{
  const double angle = this->GetAngle();
  const double ca = cos( angle );
  const double sa = sin( angle );

  this->m_Jacobian.Fill(0.0);

  const InputPointType center = this->GetCenter();  
  const double cx = center[0];
  const double cy = center[1];

  const OutputVectorType translation = this->GetTranslation();

  // derivatives with respect to the scale
  this->m_Jacobian[0][0] =    ca * ( p[0] - cx ) - sa * ( p[1] - cy );
  this->m_Jacobian[1][0] =    sa * ( p[0] - cx ) + ca * ( p[1] - cy ); 

  // derivatives with respect to the angle
  this->m_Jacobian[0][1] = ( -sa * ( p[0] - cx ) - ca * ( p[1] - cy ) ) 
                                                          * this->GetScale();
  this->m_Jacobian[1][1] = (  ca * ( p[0] - cx ) - sa * ( p[1] - cy ) ) 
                                                          * this->GetScale();

  // compute derivatives with respect to the center part
  // first with respect to cx
  this->m_Jacobian[0][2] = 1.0 - ca * this->GetScale();
  this->m_Jacobian[1][2] =     - sa * this->GetScale();
  // then with respect to cy
  this->m_Jacobian[0][3] =       sa * this->GetScale();
  this->m_Jacobian[1][3] = 1.0 - ca * this->GetScale();

  // compute derivatives with respect to the translation part
  // first with respect to tx
  this->m_Jacobian[0][4] = 1.0;
  this->m_Jacobian[1][4] = 0.0;
  // first with respect to ty
  this->m_Jacobian[0][5] = 0.0;
  this->m_Jacobian[1][5] = 1.0;

  return this->m_Jacobian;

}

template <class TScalarType>
void
CenteredSimilarity2DTransform<TScalarType>::
SetFixedParameters( const ParametersType & itkNotUsed(parameters) )
{
  // no fixed parameters
}

template <class TScalarType>
const typename CenteredSimilarity2DTransform<TScalarType>::ParametersType &
CenteredSimilarity2DTransform<TScalarType>::
GetFixedParameters( void ) const
{
  // return dummy parameters
  this->m_FixedParameters.SetSize(0);
  return this->m_FixedParameters;
}
 
// Print self
template<class TScalarType>
void
CenteredSimilarity2DTransform<TScalarType>::
PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

// Create and return an inverse transformation
template<class TScalarType>
void
CenteredSimilarity2DTransform<TScalarType>::
CloneInverseTo( Pointer & result ) const
{
  result = New();
  result->SetCenter( this->GetCenter() );  // inverse have the same center
  result->SetScale( 1.0 / this->GetScale() );
  result->SetAngle( -this->GetAngle() );
  result->SetTranslation( -( this->GetInverseMatrix() 
                                                  * this->GetTranslation() ) );
}

// Create and return a clone of the transformation
template<class TScalarType>
void
CenteredSimilarity2DTransform<TScalarType>::
CloneTo( Pointer & result ) const
{
  result = New();
  result->SetCenter( this->GetCenter() );
  result->SetScale( this->GetScale() );
  result->SetAngle( this->GetAngle() );
  result->SetTranslation( this->GetTranslation() );
}

} // namespace

#endif
