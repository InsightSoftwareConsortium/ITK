/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersorRigid3DTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
::VersorRigid3DTransform():Superclass(Superclass::OutputSpaceDimension, ParametersDimension)
{
  m_Versor.SetIdentity();
  m_Translation.Fill( 0.0 );
  m_Center.Fill( 0.0 );
  this->ComputeMatrixAndOffset();
  this->m_Jacobian = JacobianType( SpaceDimension, ParametersDimension );
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
  m_Versor.SetIdentity();
  m_Translation.Fill( 0.0 );
  m_Center.Fill( 0.0 );
  this->ComputeMatrixAndOffset(); 
}
 


// Copy Constructor
template <class TScalarType>
VersorRigid3DTransform<TScalarType>
::VersorRigid3DTransform( const Self & other )
{
  // call the superclass copy constructor
  m_Versor = other.m_Versor;
  m_Translation = other.m_Translations;
  m_Center = other.m_Center;
  this->ComputeMatrixAndOffset();
}


// Set Identity
template<class TScalarType>
void
VersorRigid3DTransform<TScalarType>
::SetIdentity()
{
  m_Versor.SetIdentity();
  m_Translation.Fill( 0.0 );
  m_Center.Fill( 0.0 );
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

  double norm = parameters[0]*parameters[0];
  axis[0] = parameters[0];
  norm += parameters[1]*parameters[1];
  axis[1] = parameters[1];
  norm += parameters[2]*parameters[2];
  axis[2] = parameters[2];
  if( norm > 0)
    {
    norm = sqrt(norm);
    }

  double epsilon = 1e-10;
  if(norm >= 1.0-epsilon)
    {
    axis = axis / (norm+epsilon*norm);
    }

  m_Versor.Set( axis );

  itkDebugMacro( <<"Versor is now " << m_Versor );
  
   
  // Transfer the translation part
  for(unsigned int j=0; j < SpaceDimension; j++) 
    {
    m_Translation[j] = parameters[j+SpaceDimension];
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
// p[3:5} = translation components
//

template <class TScalarType>
const typename VersorRigid3DTransform<TScalarType>::ParametersType &
VersorRigid3DTransform<TScalarType>
::GetParameters( void ) const
{
  itkDebugMacro( << "Getting parameters ");

  this->m_Parameters[0] = m_Versor.GetX();
  this->m_Parameters[1] = m_Versor.GetY();
  this->m_Parameters[2] = m_Versor.GetZ();

  // Transfer the translation
  for(unsigned int j=0; j < SpaceDimension; j++) 
    {
    this->m_Parameters[j+SpaceDimension] = this->m_Translation[j];
    }

  itkDebugMacro(<<"After getting parameters " << this->m_Parameters );

  return this->m_Parameters;
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

  this->m_RotationMatrix[0][0] = 1.0 - 2.0 * ( yy + zz );
  this->m_RotationMatrix[1][1] = 1.0 - 2.0 * ( xx + zz );
  this->m_RotationMatrix[2][2] = 1.0 - 2.0 * ( xx + yy );
  this->m_RotationMatrix[0][1] = 2.0 * ( xy - zw );
  this->m_RotationMatrix[0][2] = 2.0 * ( xz + yw );
  this->m_RotationMatrix[1][0] = 2.0 * ( xy + zw );
  this->m_RotationMatrix[2][0] = 2.0 * ( xz - yw );
  this->m_RotationMatrix[2][1] = 2.0 * ( yz + xw );
  this->m_RotationMatrix[1][2] = 2.0 * ( yz - xw );
 
  this->m_RotationMatrixMTime.Modified();

  OffsetType offset; 
  for(unsigned int i=0; i<3; i++)
    {
    offset[i] = m_Translation[i] + m_Center[i];  
    for(unsigned int j=0; j<3; j++)
      {
      offset[i] -= this->m_RotationMatrix[i][j] * m_Center[j];
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

  this->m_Jacobian.Fill(0.0);

  const double px = p[0] - m_Center[0];
  const double py = p[1] - m_Center[1];
  const double pz = p[2] - m_Center[2];

  const double vxx = vx * vx;
  const double vyy = vy * vy;
  const double vzz = vz * vz;
  const double vww = vw * vw;

  const double vxy = vx * vy;
  const double vxz = vx * vz;
  const double vxw = vx * vw;

  const double vyz = vy * vz;
  const double vyw = vy * vw;

  const double vzw = vz * vw;


  // compute Jacobian with respect to quaternion parameters
  this->m_Jacobian[0][0] = 2.0 * (                  (vyw+vxz) * py + (vzw-vxy) * pz )
                         / vw;
  this->m_Jacobian[1][0] = 2.0 * ( (vyw-vxz) * px   -2*vxw    * py + (vxx-vww) * pz ) 
                         / vw;
  this->m_Jacobian[2][0] = 2.0 * ( (vzw+vxy) * px + (vww-vxx) * py   -2*vxw    * pz ) 
                         / vw;

  this->m_Jacobian[0][1] = 2.0 * (  -2*vyw   * px + (vxw+vyz) * py + (vww-vyy) * pz ) 
                         / vw;
  this->m_Jacobian[1][1] = 2.0 * ( (vxw-vyz) * px                  + (vzw+vxy) * pz ) 
                         / vw;
  this->m_Jacobian[2][1] = 2.0 * ( (vyy-vww) * px + (vzw-vxy) * py   -2*vyw    * pz ) 
                         / vw;

  this->m_Jacobian[0][2] = 2.0 * (  -2*vzw   * px + (vzz-vww) * py + (vxw-vyz) * pz ) 
                         / vw;
  this->m_Jacobian[1][2] = 2.0 * ( (vww-vzz) * px   -2*vzw    * py + (vyw+vxz) * pz ) 
                         / vw;
  this->m_Jacobian[2][2] = 2.0 * ( (vxw+vyz) * px + (vyw-vxz) * py                  ) 
                         / vw;

  // compute derivatives for the translation part
  unsigned int blockOffset = 3;  

  for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
    {
    this->m_Jacobian[ dim ][ blockOffset + dim ] = 1.0;
    }

  return this->m_Jacobian;

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
