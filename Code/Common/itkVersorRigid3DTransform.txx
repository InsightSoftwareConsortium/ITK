/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersorRigid3DTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkVersorRigid3DTransform_txx
#define _itkVersorRigid3DTransform_txx

#include "itkVersorRigid3DTransform.h"


namespace itk
{

// Constructor with default arguments
template <class TScalarType>
VersorRigid3DTransform<TScalarType>
::VersorRigid3DTransform()
{
  m_Versor.SetIdentity();
  this->ComputeMatrix();
}

// Copy Constructor
template <class TScalarType>
VersorRigid3DTransform<TScalarType>
::VersorRigid3DTransform( const Self & other )
{
  // call the superclass copy constructor
  m_Versor = other.m_Versor;
  this->ComputeMatrix();
}

// Set Parameters
template <class TScalarType>
void
VersorRigid3DTransform<TScalarType>
::SetParameters( const ParametersType & parameters )
{


  itkDebugMacro( << "Setting paramaters " << parameters );

  // Transfer the versor part
  
  AxisType axis;

  axis[0] = parameters[0];
  axis[1] = parameters[1];
  axis[2] = parameters[2];

  m_Versor.Set( axis );


  itkDebugMacro( <<"Versor is now " << m_Versor );
  
  
  // Transfer the translation part
  
  OffsetType offset;
  for(unsigned int i=0; i < SpaceDimension; i++) 
    {
    offset[i] = parameters[i+3];
    }

  
  this->SetOffset( offset );

  this->ComputeMatrix();

  itkDebugMacro(<<"After setting paramaters ");
}


// Set Rotational Part
template <class TScalarType>
void
VersorRigid3DTransform<TScalarType>
::SetRotation( const VersorType & versor )
{
    m_Versor = versor;
    this->ComputeMatrix();
}



// Set Rotational Part
template <class TScalarType>
void
VersorRigid3DTransform<TScalarType>
::SetRotation( const AxisType & axis, AngleType  angle )
{
    m_Versor.Set( axis, angle );
    this->ComputeMatrix();
}


// Compute the matrix
template <class TScalarType>
void
VersorRigid3DTransform<TScalarType>
::ComputeMatrix( void )
{

  const TScalarType vx = m_Versor.GetX();
  const TScalarType vy = m_Versor.GetY();
  const TScalarType vz = m_Versor.GetZ();
  const TScalarType vw = m_Versor.GetW();
      
  const TScalarType xx = vx * vx;
  const TScalarType yy = vy * vy;
  const TScalarType zz = vz * vz;
  const TScalarType xy = vx * vy;
  const TScalarType xz = vx * vz;
  const TScalarType xw = vx * vw;
  const TScalarType yz = vy * vz;
  const TScalarType yw = vy * vw;
  const TScalarType zw = vz * vw;

  m_RotationMatrix[0][0] = 1.0 - 2.0 * ( yy + zz );
  m_RotationMatrix[1][1] = 1.0 - 2.0 * ( xx + zz );
  m_RotationMatrix[2][2] = 1.0 - 2.0 * ( xx + yy );
  m_RotationMatrix[0][1] = 2.0 * ( xy - zw );
  m_RotationMatrix[0][2] = 2.0 * ( xz + yw );
  m_RotationMatrix[1][0] = 2.0 * ( xy + zw );
  m_RotationMatrix[2][0] = 2.0 * ( xz - yw );
  m_RotationMatrix[2][1] = 2.0 * ( yz + xw );
  m_RotationMatrix[1][2] = 2.0 * ( yz - xw );
 
  m_InverseMatrix = m_RotationMatrix.GetTranspose();

}


// Set parameters
template<class TScalarType>
const typename VersorRigid3DTransform<TScalarType>::JacobianType &
VersorRigid3DTransform<TScalarType>::
GetJacobian( const InputPointType & p ) const
{
  
  typedef typename VersorType::ValueType  ValueType;

  // compute derivatives with respect to rotation
  const ValueType vx = m_Versor.GetX();
  const ValueType vy = m_Versor.GetY();
  const ValueType vz = m_Versor.GetZ();
  const ValueType vw = m_Versor.GetW();

  m_Jacobian.Fill(0.0);

  // compute Jacobian with respect to quaternion parameters
  m_Jacobian[0][0] = 2.0 * (  vx * p[0] + vy * p[1] + vz * p[2] );
  m_Jacobian[0][1] = 2.0 * (- vy * p[0] + vx * p[1] + vw * p[2] );
  m_Jacobian[0][2] = 2.0 * (- vz * p[0] - vw * p[1] + vx * p[2] );
  m_Jacobian[0][3] = 2.0 * (  vw * p[0] - vz * p[1] + vy * p[2] );

  m_Jacobian[1][0] = - m_Jacobian[0][1];
  m_Jacobian[1][1] =   m_Jacobian[0][0];
  m_Jacobian[1][2] = - m_Jacobian[0][3];
  m_Jacobian[1][3] =   m_Jacobian[0][2];

  m_Jacobian[2][0] = - m_Jacobian[0][2];
  m_Jacobian[2][1] =   m_Jacobian[0][3];
  m_Jacobian[2][2] =   m_Jacobian[0][0];
  m_Jacobian[2][3] = - m_Jacobian[0][1];


  // compute derivatives for the translation part
  unsigned int blockOffset = 4;  
  for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
    {
     m_Jacobian[ dim ][ blockOffset + dim ] = 1.0;
    }

  return m_Jacobian;

}
  
// Print self
template<class TScalarType>
void
VersorRigid3DTransform<TScalarType>::
PrintSelf(std::ostream &os, Indent indent) const
{

  Superclass::PrintSelf(os,indent);
  
  os << indent << "Versor: " << m_Versor  << std::endl;
}

} // namespace

#endif
