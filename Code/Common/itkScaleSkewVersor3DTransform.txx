/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScaleSkewVersor3DTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkScaleSkewVersor3DTransform_txx
#define _itkScaleSkewVersor3DTransform_txx

#include "itkScaleSkewVersor3DTransform.h"


namespace itk
  {

// Constructor with default arguments
template <class TScalarType>
ScaleSkewVersor3DTransform<TScalarType>
::ScaleSkewVersor3DTransform() :
  Superclass(OutputSpaceDimension, ParametersDimension)
{
  m_Scale.Fill( 1.0 );
  m_Skew.Fill( 0.0 );
}


// Constructor with arguments
template<class TScalarType>
ScaleSkewVersor3DTransform<TScalarType>::
ScaleSkewVersor3DTransform( unsigned int spaceDimension, 
                            unsigned int parametersDimension):
  Superclass(spaceDimension, parametersDimension)
{
  m_Scale.Fill( 1.0 );
  m_Skew.Fill( 0.0 );
}

// Constructor with arguments
template<class TScalarType>
ScaleSkewVersor3DTransform<TScalarType>::
ScaleSkewVersor3DTransform( const MatrixType & matrix,
                            const OutputVectorType & offset):
  Superclass(matrix, offset)
{
  this->ComputeMatrixParameters();
}
 

// Set Parameters
template <class TScalarType>
void
ScaleSkewVersor3DTransform<TScalarType>
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
  VersorType newVersor;
  newVersor.Set(axis);
  this->Set_M_Versor( newVersor );

  itkDebugMacro( <<"Versor is now " << newVersor );
  
  // Matrix must be defined before translation so that offset can be computed
  // from translation
  m_Scale[0] = parameters[6];
  m_Scale[1] = parameters[7];
  m_Scale[2] = parameters[8];

  m_Skew[0] = parameters[9];
  m_Skew[1] = parameters[10];
  m_Skew[2] = parameters[11];
  m_Skew[3] = parameters[12];
  m_Skew[4] = parameters[13];
  m_Skew[5] = parameters[14];

  // Transfer the translation part
  TranslationType newTranslation;
  newTranslation[0] = parameters[3];
  newTranslation[1] = parameters[4];
  newTranslation[2] = parameters[5];
  this->Set_M_Translation(newTranslation);
  this->ComputeMatrix();
  this->ComputeOffset();

  itkDebugMacro(<<"After setting paramaters ");
  }



//
// Get Parameters
// 
// Parameters are ordered as:
//
// p[0:2] = right part of the versor (axis times sin(t/2))
// p[3:5] = translation components
// p[6:8] = Scale
// p[9:14] = Skew {xy, xz, yx, yz, zx, zy}
//

template <class TScalarType>
const typename ScaleSkewVersor3DTransform<TScalarType>::ParametersType &
ScaleSkewVersor3DTransform<TScalarType>
::GetParameters( void ) const
  {
  itkDebugMacro( << "Getting parameters ");

  this->m_Parameters[0] = this->GetVersor().GetX();
  this->m_Parameters[1] = this->GetVersor().GetY();
  this->m_Parameters[2] = this->GetVersor().GetZ();

  this->m_Parameters[3] = this->GetTranslation()[0];
  this->m_Parameters[4] = this->GetTranslation()[1];
  this->m_Parameters[5] = this->GetTranslation()[2];

  this->m_Parameters[6] = this->GetScale()[0];
  this->m_Parameters[7] = this->GetScale()[1];
  this->m_Parameters[8] = this->GetScale()[2];

  this->m_Parameters[9] = this->GetSkew()[0];  
  this->m_Parameters[10] = this->GetSkew()[1];  
  this->m_Parameters[11] = this->GetSkew()[2];  
  this->m_Parameters[12] = this->GetSkew()[3];  
  this->m_Parameters[13] = this->GetSkew()[4];  
  this->m_Parameters[14] = this->GetSkew()[5];  

  itkDebugMacro(<<"After getting parameters " << this->m_Parameters );

  return this->m_Parameters;
  }

template <class TScalarType>
void
ScaleSkewVersor3DTransform<TScalarType>
::SetIdentity()
  {
  m_Scale.Fill( 1.0 );
  m_Skew.Fill( 0.0 );
  Superclass::SetIdentity();
  }


template <class TScalarType>
void
ScaleSkewVersor3DTransform<TScalarType>
::SetScale( const ScaleVectorType & scale )
{
  m_Scale = scale;
  this->ComputeMatrix();
}

template <class TScalarType>
void
ScaleSkewVersor3DTransform<TScalarType>
::SetSkew( const SkewVectorType & skew )
{
  m_Skew = skew;
  this->ComputeMatrix();
}

// Compute the matrix
template <class TScalarType>
void
ScaleSkewVersor3DTransform<TScalarType>
::ComputeMatrix( void )
{

  VersorType versor = Superclass::GetVersor();
  
  const TScalarType vx = versor.GetX();
  const TScalarType vy = versor.GetY();
  const TScalarType vz = versor.GetZ();
  const TScalarType vw = versor.GetW();
      
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
  newMatrix[0][0] = m_Scale[0] - 2.0 * ( yy + zz );
  newMatrix[1][1] = m_Scale[1] - 2.0 * ( xx + zz );
  newMatrix[2][2] = m_Scale[2] - 2.0 * ( xx + yy );
  newMatrix[0][1] = 2.0 * ( xy - zw )  + ( m_Skew[0] );
  newMatrix[0][2] = 2.0 * ( xz + yw )  + ( m_Skew[1] );
  newMatrix[1][0] = 2.0 * ( xy + zw )  + ( m_Skew[2] );
  newMatrix[1][2] = 2.0 * ( yz - xw )  + ( m_Skew[3] );
  newMatrix[2][0] = 2.0 * ( xz - yw )  + ( m_Skew[4] );
  newMatrix[2][1] = 2.0 * ( yz + xw )  + ( m_Skew[5] );
  this->Set_M_Matrix ( newMatrix );
}

template <class TScalarType>
void
ScaleSkewVersor3DTransform<TScalarType>
::ComputeMatrixParameters( void )
{
    itkExceptionMacro( << "Setting the matrix of a ScaleSkewVersor3D transform is not supported at this time." );
}

 
// Print self
template<class TScalarType>
void
ScaleSkewVersor3DTransform<TScalarType>::
PrintSelf(std::ostream &os, Indent indent) const
{

  Superclass::PrintSelf(os,indent);
  
  os << indent << "Scale:       " << m_Scale        << std::endl;
  os << indent << "Skew:        " << m_Skew         << std::endl;
}

// Set parameters
template<class TScalarType>
const typename ScaleSkewVersor3DTransform<TScalarType>::JacobianType &
ScaleSkewVersor3DTransform<TScalarType>::
GetJacobian( const InputPointType & p ) const
{
  typedef typename VersorType::ValueType  ValueType;

  // compute derivatives with respect to rotation
  const ValueType vx = this->GetVersor().GetX();
  const ValueType vy = this->GetVersor().GetY();
  const ValueType vz = this->GetVersor().GetZ();
  const ValueType vw = this->GetVersor().GetW();

  this->m_Jacobian.Fill(0.0);

  const double px = p[0] - this->GetCenter()[0];
  const double py = p[1] - this->GetCenter()[1];
  const double pz = p[2] - this->GetCenter()[2];

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

  this->m_Jacobian[0][3] = 1.0;
  this->m_Jacobian[1][4] = 1.0;
  this->m_Jacobian[2][5] = 1.0;

  this->m_Jacobian[0][6] = px;
  this->m_Jacobian[1][7] = py;
  this->m_Jacobian[2][8] = pz;

  this->m_Jacobian[0][9]  = py;
  this->m_Jacobian[0][10] = pz;
  this->m_Jacobian[1][11] = px;
  this->m_Jacobian[1][12] = pz;
  this->m_Jacobian[2][13] = px;
  this->m_Jacobian[2][14] = py;

  return this->m_Jacobian;

}



} // namespace

#endif
