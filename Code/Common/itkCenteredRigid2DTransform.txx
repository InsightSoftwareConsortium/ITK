/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCenteredRigid2DTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkCenteredRigid2DTransform_txx
#define _itkCenteredRigid2DTransform_txx

#include "itkCenteredRigid2DTransform.h"


namespace itk
{

// Constructor with default arguments
template <class TScalarType>
CenteredRigid2DTransform<TScalarType>
::CenteredRigid2DTransform()
{
  m_Angle = NumericTraits< TScalarType >::Zero;
  m_Center.Fill( 0.0 );
  m_Translation.Fill( 0.0 );
}

// Copy Constructor
template <class TScalarType>
CenteredRigid2DTransform<TScalarType>
::CenteredRigid2DTransform( const Self & other )
{
  m_Angle         = other.m_Angle;
  m_Center        = other.m_Center;
  m_Translation   = other.m_Translation;
  this->ComputeMatrixAndOffset();
}

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
template <class TScalarType>
void
CenteredRigid2DTransform<TScalarType>
::SetParameters( const ParametersType & parameters )
{
  itkDebugMacro( << "Setting paramaters " << parameters );

  m_Parameters = parameters;

  // Take the angle
  m_Angle = parameters[0];
 
  // Transfer the center 
  for(unsigned int i=0; i < SpaceDimension; i++) 
  {
    m_Center[i] = parameters[i+1];
  }

  // Transfer the translation
  for(unsigned int j=0; j < SpaceDimension; j++) 
  {
    m_Translation[j] = parameters[j+1+SpaceDimension];
  }

  this->ComputeMatrixAndOffset();

  itkDebugMacro(<<"After setting parameters ");
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

template <class TScalarType>
const typename CenteredRigid2DTransform<TScalarType>::ParametersType &
CenteredRigid2DTransform<TScalarType>
::GetParameters( void ) const
{
  itkDebugMacro( << "Getting parameters ");

  // Set angles with parameters
  m_Parameters[0] = m_Angle;
 
  // Transfer the center of rotation 
  for(unsigned int i=0; i < SpaceDimension; i++) 
  {
    m_Parameters[i+1] = m_Center[i];
  }

  // Transfer the translation
  for(unsigned int j=0; j < SpaceDimension; j++) 
  {
    m_Parameters[j+1+SpaceDimension] = m_Translation[j];
  }

  itkDebugMacro(<<"After getting parameters " << m_Parameters );

  return m_Parameters;
}



// Set Identity
template <class TScalarType>
void
CenteredRigid2DTransform<TScalarType>
::SetIdentity(void)
{
  m_Angle = NumericTraits< TScalarType >::Zero;
  m_Center.Fill( 0.0 );
  m_Translation.Fill( 0.0 );

  this->ComputeMatrixAndOffset();
}



// Set Rotational Part
template <class TScalarType>
void
CenteredRigid2DTransform<TScalarType>
::SetAngle(TScalarType angle)
{
  m_Angle = angle;
  this->ComputeMatrixAndOffset();
}


// Compute the matrix
template <class TScalarType>
void
CenteredRigid2DTransform<TScalarType>
::ComputeMatrixAndOffset( void )
{
  const double cx = cos(m_Angle);
  const double sx = sin(m_Angle);

  m_RotationMatrix[0][0]=cx;m_RotationMatrix[0][1]=sx;
  m_RotationMatrix[1][0]=-sx;m_RotationMatrix[1][1]=cx;

  m_InverseMatrix = m_RotationMatrix.GetTranspose();

  this->Modified();
}


// Set parameters
template<class TScalarType>
const typename CenteredRigid2DTransform<TScalarType>::JacobianType &
CenteredRigid2DTransform<TScalarType>::
GetJacobian( const InputPointType & p ) const
{
  // need to check if angles are in the right order
  const double cx = cos(m_Angle);
  const double sx = sin(m_Angle);

  m_Jacobian.Fill(0.0);

  // derivatives with respect to the angle
  m_Jacobian[0][0] = -sx * p[0] + cx * p[1]; 
  m_Jacobian[1][0] = -cx * p[0] + sx * p[1];

  // compute derivatives for the translation part
  unsigned int blockOffset = 1;  
  for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
    {
    m_Jacobian[ dim ][ blockOffset + dim ] = 1.0;
    }

  return m_Jacobian;

}
  
// Print self
template<class TScalarType>
void
CenteredRigid2DTransform<TScalarType>::
PrintSelf(std::ostream &os, Indent indent) const
{

  Superclass::PrintSelf(os,indent);
  
  os << indent << "Angle       = " << m_Angle        << std::endl;
  os << indent << "Center      = " << m_Center       << std::endl;
  os << indent << "Translation = " << m_Translation  << std::endl;
}

} // namespace

#endif
