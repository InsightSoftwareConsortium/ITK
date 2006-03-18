/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersorTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVersorTransform_txx
#define __itkVersorTransform_txx

#include "itkVersorTransform.h"


namespace itk
{

/** Constructor with default arguments */
template <class TScalarType>
VersorTransform<TScalarType>
::VersorTransform() : Superclass(OutputSpaceDimension, ParametersDimension)
{
  m_Versor.SetIdentity();
}

/** Constructor with default arguments */
template<class TScalarType>
VersorTransform<TScalarType>::
VersorTransform(unsigned int spaceDimension, 
                unsigned int parametersDimension) :
  Superclass(spaceDimension,parametersDimension)
{
  m_Versor.SetIdentity();
}

/** Constructor with default arguments */
template<class TScalarType>
VersorTransform<TScalarType>::
VersorTransform(const MatrixType & matrix,
                const OutputVectorType & offset) : Superclass(matrix, offset)
{
  this->ComputeMatrixParameters();  // called in MatrixOffset baseclass
}

/** Set Parameters */
template <class TScalarType>
void
VersorTransform<TScalarType>
::SetParameters( const ParametersType & parameters )
{
  itkDebugMacro( << "Setting parameters " << parameters );

  // Transfer the versor part
  AxisType rightPart;

  rightPart[0] = parameters[0];
  rightPart[1] = parameters[1];
  rightPart[2] = parameters[2];

  // The versor will compute the scalar part.
  m_Versor.Set( rightPart ); 

  itkDebugMacro( <<"Versor is now " << m_Versor );
  
  this->ComputeMatrix();

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();

  itkDebugMacro(<<"After setting parameters ");
}

/** Set Parameters */
template <class TScalarType>
const typename VersorTransform<TScalarType>::ParametersType & 
VersorTransform<TScalarType>
::GetParameters( void ) const
{
  this->m_Parameters[0] = this->m_Versor.GetRight()[0];
  this->m_Parameters[1] = this->m_Versor.GetRight()[1];
  this->m_Parameters[2] = this->m_Versor.GetRight()[2];

  return this->m_Parameters;
}

/** Set Rotational Part */
template <class TScalarType>
void
VersorTransform<TScalarType>
::SetRotation( const VersorType & versor )
{
  m_Versor = versor;
  this->ComputeMatrix();
  this->ComputeOffset();
}


/** Set Rotational Part */
template <class TScalarType>
void
VersorTransform<TScalarType>
::SetRotation( const AxisType & axis, AngleType  angle )
{
  m_Versor.Set( axis, angle );
  this->ComputeMatrix();
  this->ComputeOffset();
}

/** Set Identity */
template <class TScalarType>
void
VersorTransform<TScalarType>
::SetIdentity( )
{
  Superclass::SetIdentity();

  m_Versor.SetIdentity();
  
  this->Modified();
}


/** Compute the matrix */
template <class TScalarType>
void
VersorTransform<TScalarType>
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

  MatrixType newMatrix;
  newMatrix[0][0] = 1.0 - 2.0 * ( yy + zz );
  newMatrix[1][1] = 1.0 - 2.0 * ( xx + zz );
  newMatrix[2][2] = 1.0 - 2.0 * ( xx + yy );
  newMatrix[0][1] = 2.0 * ( xy - zw );
  newMatrix[0][2] = 2.0 * ( xz + yw );
  newMatrix[1][0] = 2.0 * ( xy + zw );
  newMatrix[2][0] = 2.0 * ( xz - yw );
  newMatrix[2][1] = 2.0 * ( yz + xw );
  newMatrix[1][2] = 2.0 * ( yz - xw );
  this->SetVarMatrix(newMatrix);
}

/** Compute the matrix */
template <class TScalarType>
void
VersorTransform<TScalarType>
::ComputeMatrixParameters( void )
{
  m_Versor.Set( this->GetMatrix() );
}

/** Get the Jacobian */
template<class TScalarType>
const typename VersorTransform<TScalarType>::JacobianType &
VersorTransform<TScalarType>::
GetJacobian( const InputPointType & p ) const
{
  typedef typename VersorType::ValueType  ValueType;

  // compute derivatives with respect to rotation
  const ValueType vx = m_Versor.GetX();
  const ValueType vy = m_Versor.GetY();
  const ValueType vz = m_Versor.GetZ();
  const ValueType vw = m_Versor.GetW();

  this->m_Jacobian.Fill(0.0);

  const double px = p[0];
  const double py = p[1];
  const double pz = p[2];

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
  this->m_Jacobian[0][0] = 2.0 * (               (vyw+vxz)*py + (vzw-vxy)*pz)
                         / vw;
  this->m_Jacobian[1][0] = 2.0 * ((vyw-vxz)*px   -2*vxw   *py + (vxx-vww)*pz) 
                         / vw;
  this->m_Jacobian[2][0] = 2.0 * ((vzw+vxy)*px + (vww-vxx)*py   -2*vxw   *pz) 
                         / vw;

  this->m_Jacobian[0][1] = 2.0 * ( -2*vyw  *px + (vxw+vyz)*py + (vww-vyy)*pz) 
                         / vw;
  this->m_Jacobian[1][1] = 2.0 * ((vxw-vyz)*px                + (vzw+vxy)*pz) 
                         / vw;
  this->m_Jacobian[2][1] = 2.0 * ((vyy-vww)*px + (vzw-vxy)*py   -2*vyw   *pz) 
                         / vw;

  this->m_Jacobian[0][2] = 2.0 * ( -2*vzw  *px + (vzz-vww)*py + (vxw-vyz)*pz) 
                         / vw;
  this->m_Jacobian[1][2] = 2.0 * ((vww-vzz)*px   -2*vzw   *py + (vyw+vxz)*pz) 
                         / vw;
  this->m_Jacobian[2][2] = 2.0 * ((vxw+vyz)*px + (vyw-vxz)*py               ) 
                         / vw;
  return this->m_Jacobian;

}
  
/** Print self */
template<class TScalarType>
void
VersorTransform<TScalarType>::
PrintSelf(std::ostream &os, Indent indent) const
{

  Superclass::PrintSelf(os,indent);
  
  os << indent << "Versor: " << m_Versor  << std::endl;
}

} // namespace

#endif
