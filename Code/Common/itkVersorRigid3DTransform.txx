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
::VersorRigid3DTransform():Superclass(OutputSpaceDimension, ParametersDimension)
{
  m_Versor.SetIdentity();
  m_Translation.Fill( 0.0 );
  m_Center.Fill( 0.0 );
  this->ComputeMatrixAndOffset();
}


// Constructor with arguments
template<class TScalarType>
VersorRigid3DTransform<TScalarType>::
VersorRigid3DTransform( unsigned int spaceDimension, 
                        unsigned int parametersDimension):
Superclass(spaceDimension,parametersDimension)
{
  // note: this virtual function will only
  // call the one defined in this class because 
  // we are in a constructor
  this->ComputeMatrixAndOffset(); 
}
 


// Copy Constructor
template <class TScalarType>
VersorRigid3DTransform<TScalarType>
::VersorRigid3DTransform( const Self & other )
{
  // call the superclass copy constructor
  m_Versor = other.m_Versor;
  this->ComputeMatrixAndOffset();
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
  
  
  // Transfer the center part
  for(unsigned int i=0; i < SpaceDimension; i++) 
    {
    m_Center[i] = parameters[i+SpaceDimension];
    }

   
  // Transfer the translation part
  for(unsigned int j=0; j < SpaceDimension; j++) 
    {
    m_Translation[j] = parameters[j+2*SpaceDimension];
    }

 

  this->ComputeMatrixAndOffset();

  itkDebugMacro(<<"After setting paramaters ");
}



//
// Get Parameters
// 
// Parameters are ordered as:
//
// p[0:2] = right part of the versor (axis times sin(t/2))
// p[3:5} = center of rotation coordinates
// p[6:8} = translation components
//

template <class TScalarType>
const typename VersorRigid3DTransform<TScalarType>::ParametersType &
VersorRigid3DTransform<TScalarType>
::GetParameters( void ) const
{
  itkDebugMacro( << "Getting parameters ");

  m_Parameters[0] = m_Versor.GetX();
  m_Parameters[1] = m_Versor.GetY();
  m_Parameters[2] = m_Versor.GetZ();

  // Transfer the center of rotation 
  for(unsigned int i=0; i < SpaceDimension; i++) 
  {
    m_Parameters[i+SpaceDimension] = m_Center[i];
  }

  // Transfer the translation
  for(unsigned int j=0; j < SpaceDimension; j++) 
  {
    m_Parameters[j+2*SpaceDimension] = m_Translation[j];
  }

  itkDebugMacro(<<"After getting parameters " << m_Parameters );

  return m_Parameters;
}




// Set Rotational Part
template <class TScalarType>
void
VersorRigid3DTransform<TScalarType>
::SetRotation( const VersorType & versor )
{
    m_Versor = versor;
    this->ComputeMatrixAndOffset();
}



// Set Rotational Part
template <class TScalarType>
void
VersorRigid3DTransform<TScalarType>
::SetRotation( const AxisType & axis, AngleType  angle )
{
    m_Versor.Set( axis, angle );
    this->ComputeMatrixAndOffset();
}




template <class TScalarType>
void
VersorRigid3DTransform<TScalarType>
::SetCenter( const InputPointType & center )
{
  m_Center = center;
  this->ComputeMatrixAndOffset();
}




template <class TScalarType>
void
VersorRigid3DTransform<TScalarType>
::SetTranslation( const OutputVectorType & translation )
{
  m_Translation = translation;
  this->ComputeMatrixAndOffset();
}






// Compute the matrix
template <class TScalarType>
void
VersorRigid3DTransform<TScalarType>
::ComputeMatrixAndOffset( void )
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

  OffsetType offset; 
  for(unsigned int i=0; i<3; i++)
    {
    offset[i] = m_Translation[i] + m_Center[i];  
    for(unsigned int j=0; j<3; j++)
      {
      offset[i] -= m_RotationMatrix[i][j] * m_Center[j];
      }
    }

  this->SetOffset( offset );

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


  // compute derivatives for the rotation center 
  unsigned int blockOffset = 4;  
  for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
    {
     m_Jacobian[ dim ][ blockOffset + dim ] = 1.0;
    }

  // compute derivatives for the translation part
  blockOffset += 3;
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
  
  os << indent << "Versor:      " << m_Versor       << std::endl;
  os << indent << "Center:      " << m_Center       << std::endl;
  os << indent << "Translation: " << m_Translation  << std::endl;
}

} // namespace

#endif
