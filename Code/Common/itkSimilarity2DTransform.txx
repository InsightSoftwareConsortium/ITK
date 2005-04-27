/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimilarity2DTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSimilarity2DTransform_txx
#define _itkSimilarity2DTransform_txx

#include "itkSimilarity2DTransform.h"


namespace itk
{

// Constructor with default arguments
template <class TScalarType>
Similarity2DTransform<TScalarType>
::Similarity2DTransform():Superclass(OutputSpaceDimension, ParametersDimension)
{
  m_Scale = 1.0; 

  // note: this virtual function will only
  // call the one defined in this class because 
  // we are in a constructor
  this->ComputeMatrixAndOffset();
}



// Copy Constructor
template <class TScalarType>
Similarity2DTransform<TScalarType>
::Similarity2DTransform( const Self & other ):Superclass( other )
{
  m_Scale = other.m_Scale;

  // note: this virtual function will only
  // call the one defined in this class because 
  // we are in a constructor
  this->ComputeMatrixAndOffset();
}


// Constructor with arguments
template<class TScalarType>
Similarity2DTransform<TScalarType>::
Similarity2DTransform( unsigned int spaceDimension, 
                  unsigned int parametersDimension):
  Superclass(spaceDimension,parametersDimension)
{
  m_Scale = 1.0; 

}
 

// Set Parameters
template <class TScalarType>
void
Similarity2DTransform<TScalarType>
::SetParameters( const ParametersType & parameters )
{
  itkDebugMacro( << "Setting paramaters " << parameters );

  // Set angles with parameters
  m_Scale = parameters[0];
 
  // Set angles with parameters
  this->SetAngle( parameters[1] );

  // Transfer the translation part
  OffsetType translation;
  for(unsigned int i=0; i < SpaceDimension; i++) 
    {
    translation[i] = parameters[i+2];
    }

  this->SetTranslation( translation );

  this->ComputeMatrixAndOffset();

  itkDebugMacro(<<"After setting paramaters ");
}


// Get Parameters
template <class TScalarType>
const typename Similarity2DTransform<TScalarType>::ParametersType &
Similarity2DTransform<TScalarType>
::GetParameters( void ) const
{
  itkDebugMacro( << "Getting parameters ");

  this->m_Parameters[0] = m_Scale;
  this->m_Parameters[1] = this->GetAngle();
 
  // Transfer the translation part
  OffsetType translation = this->GetTranslation();
  for(unsigned int i=0; i < SpaceDimension; i++) 
    {
    this->m_Parameters[i+2] = translation[i];
    }

  itkDebugMacro(<<"After getting parameters " << this->m_Parameters );

  return this->m_Parameters;
}



// Set Scale Part
template <class TScalarType>
void
Similarity2DTransform<TScalarType>
::SetScale( ScaleType scale )
{
  m_Scale = scale;
  this->ComputeMatrixAndOffset();
}


// Compute the matrix
template <class TScalarType>
void
Similarity2DTransform<TScalarType>
::ComputeMatrixAndOffset( void )
{

  const double angle = this->GetAngle();

  const double cc = cos( angle );
  const double ss = sin( angle );

  const double ca = cc * m_Scale;
  const double sa = ss * m_Scale;

  const InputPointType center = this->GetCenter();  
  const double cx = center[0];
  const double cy = center[1];

  const OutputVectorType translation = this->GetTranslation();
  const double tx = translation[0];
  const double ty = translation[1];

  this->m_RotationMatrix[0][0]= ca; this->m_RotationMatrix[0][1]=-sa;
  this->m_RotationMatrix[1][0]= sa; this->m_RotationMatrix[1][1]= ca;

  OffsetType offset;

  offset[0] = tx + sa * cy + ( 1.0 - ca ) * cx;
  offset[1] = ty - sa * cx + ( 1.0 - ca ) * cy;

  this->SetOffset( offset );

  const double ci = cc / m_Scale;
  const double si = ss / m_Scale;

  this->m_InverseMatrix[0][0]= ci; this->m_InverseMatrix[0][1]= si;
  this->m_InverseMatrix[1][0]=-si; this->m_InverseMatrix[1][1]= ci;

  this->Modified();

}


// Set parameters
template<class TScalarType>
const typename Similarity2DTransform<TScalarType>::JacobianType &
Similarity2DTransform<TScalarType>::
GetJacobian( const InputPointType & p ) const
{

  // need to check if angles are in the right order
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
  this->m_Jacobian[0][1] = ( -sa * ( p[0] - cx ) - ca * ( p[1] - cy ) ) * m_Scale;
  this->m_Jacobian[1][1] = (  ca * ( p[0] - cx ) - sa * ( p[1] - cy ) ) * m_Scale; 

  // compute derivatives with respect to the translation part
  // first with respect to tx
  this->m_Jacobian[0][2] = 1.0;
  this->m_Jacobian[1][2] = 0.0;
  // first with respect to ty
  this->m_Jacobian[0][3] = 0.0;
  this->m_Jacobian[1][3] = 1.0;

  return this->m_Jacobian;

}

 
// Set Identity
template <class TScalarType>
void
Similarity2DTransform<TScalarType>
::SetIdentity(void)
{
  this->Superclass::SetIdentity();
  m_Scale = static_cast< TScalarType >( 1.0f );
  this->ComputeMatrixAndOffset();
}


 
// Print self
template<class TScalarType>
void
Similarity2DTransform<TScalarType>::
PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Scale =" << m_Scale  << std::endl;
}

} // namespace

#endif
