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
::VersorRigid3DTransform() :
  Superclass(OutputSpaceDimension, ParametersDimension)
{
}


// Constructor with arguments
template<class TScalarType>
VersorRigid3DTransform<TScalarType>::
VersorRigid3DTransform( unsigned int outputSpaceDim, 
                        unsigned int paramDim) :
  Superclass(outputSpaceDim,paramDim)
{
}
 

// Constructor with arguments
template<class TScalarType>
VersorRigid3DTransform<TScalarType>::
VersorRigid3DTransform( const MatrixType & matrix,
                        const OutputVectorType & offset) :
  Superclass(matrix,offset)
{
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
  VersorType newVersor;
  newVersor.Set(axis);
  this->SetVarVersor( newVersor );
  this->ComputeMatrix();

  itkDebugMacro( <<"Versor is now " << this->GetVersor() );
  
   
  // Transfer the translation part
  TranslationType newTranslation;
  newTranslation[0] = parameters[3];
  newTranslation[1] = parameters[4];
  newTranslation[2] = parameters[5];
  this->SetVarTranslation(newTranslation);
  this->ComputeOffset();

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();

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

  this->m_Parameters[0] = this->GetVersor().GetX();
  this->m_Parameters[1] = this->GetVersor().GetY();
  this->m_Parameters[2] = this->GetVersor().GetZ();

  // Transfer the translation
  this->m_Parameters[3] = this->GetTranslation()[0];
  this->m_Parameters[4] = this->GetTranslation()[1];
  this->m_Parameters[5] = this->GetTranslation()[2];

  itkDebugMacro(<<"After getting parameters " << this->m_Parameters );

  return this->m_Parameters;
}

// Set parameters
template<class TScalarType>
const typename VersorRigid3DTransform<TScalarType>::JacobianType &
VersorRigid3DTransform<TScalarType>::
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

  return this->m_Jacobian;

}
  
// Print self
template<class TScalarType>
void
VersorRigid3DTransform<TScalarType>::
PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // namespace

#endif
